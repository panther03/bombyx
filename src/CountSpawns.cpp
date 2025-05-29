#include "CountSpawns.hpp"
#include "IR.hpp"
#include "clang/AST/Expr.h"
#include <string>
#include <unordered_map>

class SymbolicCount {
public:
    // To expand this with other types of expressions, can make an ADT with other variants,
    // like IfCountExpr & so on.
    using ForCountExpr = std::pair<IRExpr*, SymbolicCount*>;
    int N = 0;
    std::list<ForCountExpr> S;

    SymbolicCount(int N): N(N) {}
    ~SymbolicCount() {
        for (auto &fe: S) {
            delete fe.second;
        }
    }
    SymbolicCount& operator+=(const int &m) {
        if (N >= 0) {
            N += m;
        }
        return *this;
    }

    SymbolicCount& operator+=(const ForCountExpr &fe) {
        S.push_back(fe);
        return *this;
    }

    SymbolicCount& operator+=(const SymbolicCount &other) {
        N += other.N;
        for (auto &fe : other.S) {
            S.push_back(fe);
        }
        return *this;
    }
    
    bool isEmpty() const {
        return (N == 0 && S.empty());
    }
};

class SymbolicCountPrintWrap {
    SymbolicCount &SC;
    IRPrintContext &C;
public:
    SymbolicCountPrintWrap(SymbolicCount &SC, IRPrintContext &C) : SC(SC), C(C) {}

    friend llvm::raw_ostream& operator<<(llvm::raw_ostream &Out, const SymbolicCountPrintWrap &SCW) {
        auto &SC = SCW.SC;
        Out << "SymbolicCount(N=" << SC.N << ", S=[";
        for (const auto &fe : SC.S) {
            auto SWrap = SymbolicCountPrintWrap(*(fe.second), SCW.C);
            Out << "( ";
            fe.first->print(Out, SCW.C);
            Out << " | " << SWrap << "), ";
        }
        Out << "])";
        return Out;
    }
};

SymbolicCountPrintWrap wrap(SymbolicCount &SC, IRPrintContext &C) {
    return SymbolicCountPrintWrap(SC, C);
}

bool operator==(const SymbolicCount& L, const SymbolicCount& R) {
    if (L.N < 0 || L.N != R.N || L.S.size() != R.S.size()) return false;
    bool eq = true;
    auto itL = L.S.begin();
    auto itR = R.S.begin();
    for (int i = 0; i < L.S.size(); i++) {
        auto eL = *itL;
        auto eR = *itR;
        // TODO: this only checks that the IRExprs have the same pointer, so it is more restrictive than necessary
        // need to add equality to IRExpr itself
        eq = eq && (eL.first == eR.first) && (*(eL.second) == *(eR.second));
        itL++;
        itR++;
    }
    return eq;
}

// nightmarish hack storing an integer in the pointer as a symbolic variable
// a normal IRVarRef shouldn't be misaligned so we keep the LSB as 1 for sanity check
#define SYMVAR_TO_VARREF(SymV) ((IRVarDecl*)((SymV << 1) + 1))
#define VARREF_NOT_SYMBOLIC(VR) (((size_t)VR & 1) == 0)
#define VARREF_TO_SYMVAR(VR) (((size_t)VR) >> 1)


class SpawnCounter {
public:
    std::unordered_map<IRStmt*, SymbolicCount*> ClsCntLookup;
private:
    std::vector<IRExpr*> GlobalEnv;
    std::unordered_map<IRBasicBlock*, int> JoinCount;
    SymbolicCount *InvalidCount;
    using EnvTy = std::unordered_map<IRVarRef, size_t>;

    IRExpr* extractLoopCount(LoopIRStmt *LS, EnvTy &PreEnv, EnvTy &PostEnv) {
        if (auto CondE = dyn_cast<BinopIRExpr>(LS->Cond.get())) {
            int IterOfs = 0;
            IRExpr* IterSel = nullptr;
            // TODO: doesnt handle a negative loop increment
            switch (CondE->Op) {
                case BinopIRExpr::BINOP_GE: { IterOfs = 1; };
                case BinopIRExpr::BINOP_GT: { IterSel = CondE->Right.get(); break; }
                case BinopIRExpr::BINOP_LE: { IterOfs = 1; }
                case BinopIRExpr::BINOP_LT: { IterSel = CondE->Left.get(); break; }
                default: {}
            }
            if (!IterSel) return nullptr;
            IdentIRExpr* IterVar = dyn_cast<IdentIRExpr>(IterSel);
            if (!IterVar) return nullptr;
            
            auto *IVarPostLoop = GlobalEnv[PostEnv[IterVar->Ident]];
            auto IncS = dyn_cast<BinopIRExpr>(IVarPostLoop);
            if (!IncS || IncS->Op != BinopIRExpr::BINOP_ADD) return nullptr;
            
            auto IncL = dyn_cast<IdentIRExpr>(IncS->Left.get());
            if (!IncL) return nullptr;

            if (VARREF_TO_SYMVAR(IncL->Ident) != PreEnv[IterVar->Ident]) return nullptr;

            // TODO: add something for when the initialization is not 0
            auto *IncE = IncS->Right.get()->clone();
            auto *IterOpp = ((IterSel == CondE->Left.get()) ? CondE->Right.get() : CondE->Left.get())->clone();
            return new BinopIRExpr(BinopIRExpr::BINOP_DIV, IterOpp, IncE);
        }
        return nullptr;
    }

    void addToEnv(EnvTy &Env, IRVarRef Dest, IRExpr *Src) {
        // TODO: this analysis is unsound when the variable is aliased to something else
        // you could have code like 
        // int i = 0; (v0)
        // i += 1     (v1)
        // y = &i;    
        // *y = -1;   (i |-> v1)
        // <- here the environment would say that i -> v1 = v0 + 1, when in fact i = -1
        auto *SrcC = Src->clone();
        ExprIdentifierVisitor _(SrcC, [&](IRVarRef &VR, bool Dest) {
            assert(!Dest);
            assert(VARREF_NOT_SYMBOLIC(VR));

            if (Env.find(VR) != Env.end()) {
                VR = SYMVAR_TO_VARREF(Env[VR]);
            }
        });
        Env[Dest] = GlobalEnv.size();
        GlobalEnv.push_back(SrcC);
    }   

    void countSpawns(
        EnvTy &Env,
        SymbolicCount* CDest,
        IRBasicBlock *B) {
        assert(CDest);
        if (JoinCount.find(B) != JoinCount.end()) {
            auto &JC = --JoinCount[B];
            if (JC > 0) return;
        }
        auto *CDestL = CDest;
        for (auto &S: *B) {
            if (auto *ES = dyn_cast<ESpawnIRStmt>(S.get())) {
                *CDestL += 1;
            } else if (auto *CDS = dyn_cast<ClosureDeclIRStmt>(S.get())) {
                CDestL = new SymbolicCount(0);
                assert(ClsCntLookup.find(CDS) == ClsCntLookup.end());
                ClsCntLookup[CDS] = CDestL;
            } else if (auto *CS = dyn_cast<CopyIRStmt>(S.get())) {
                addToEnv(Env, CS->Dest, CS->Src.get());
            }
        }
        bool special = false;
        if (B->Term) { 
            if (auto *IS = dyn_cast<IfIRStmt>(B->Term)) {
                special = true;
                auto *JoinB = FindJoin(B->Succs[0], B->Succs[1]);
                bool empty = CDestL->isEmpty();
                assert((empty || JoinB) && "Closure declaration before if/else without join.. where would the spawn next go?");
                
                JoinCount[JoinB] = 2 + (JoinB != B->Succs[1] ? 1 : 0);

                auto *IfCnt = new SymbolicCount(0);
                EnvTy IfEnv = Env;
                countSpawns(IfEnv, IfCnt, B->Succs[0]);

                bool determined = false;
                if (JoinB != B->Succs[1]) { // have a real else branch
                    auto *ElseCnt = new SymbolicCount(0);
                    EnvTy ElseEnv = Env;
                    countSpawns(ElseEnv, ElseCnt, B->Succs[1]);
                    // If the if and else branches spawn the same amount, then we know the overall spawn count.
                    if (*IfCnt == *ElseCnt) {
                        determined = true;
                        *CDestL += *IfCnt;
                    }
                    delete ElseCnt;
                } else {
                    // If there are no spawns in this if, it doesnt matter that we cannot determine the condition.
                    determined = IfCnt->isEmpty();
                }
                delete IfCnt;

                if (!determined) {
                    CDestL->N = -1;
                }

                if (JoinB) {
                    countSpawns(Env, CDestL, JoinB);
                }
            } else if (auto *LS = dyn_cast<LoopIRStmt>(B->Term)) {
                special = true;
                JoinCount[B] = 2;
                auto *BodyCnt = new SymbolicCount(0);
                EnvTy BodyEnv = Env;
                countSpawns(BodyEnv, BodyCnt, B->Succs[0]);
                IRExpr* loopCount = extractLoopCount(LS, Env, BodyEnv);

                if (!BodyCnt->isEmpty()) {
                    if (loopCount) {
                        *CDestL += std::make_pair(loopCount, BodyCnt);
                    } else {
                        CDestL->N = -1;
                        delete BodyCnt;
                    }
                } else {
                    if (loopCount) delete loopCount;
                    delete BodyCnt;
                }

                countSpawns(Env, CDestL, B->Succs[1]);
            }
        }
        if (!special)  {
            assert (B->Succs.size() <= 1);
            if (!B->Succs.empty()) {
                countSpawns(Env, CDestL, B->Succs[0]);
            }
        }
    }

public:
    SpawnCounter() {
        InvalidCount = new SymbolicCount(0);
    }
    ~SpawnCounter() {
        delete InvalidCount;
        for (auto &[_, Cnt] : ClsCntLookup) {
            delete Cnt;
        }
        for (auto &E: GlobalEnv) {
            delete E;
        }
    }
    void countSpawns(IRBasicBlock *Entry) {
        EnvTy Env;
        countSpawns(Env, InvalidCount, Entry);
        if (!InvalidCount->isEmpty()) {
            PANIC("Espawn used before closure declaration..");
        }
    }
};

IRExpr* SymCountToExpr(SymbolicCount *C) {
    auto *NELit = new IntLiteralIRExpr(C->N);
    IRExpr *L = NELit;
    for (const auto &fe : C->S) {
        auto *MulE = new BinopIRExpr(BinopIRExpr::BINOP_MUL, fe.first, SymCountToExpr(fe.second));
        L = new BinopIRExpr(BinopIRExpr::BINOP_ADD, L, MulE);
    }
    return L;
}

void CountSpawns(IRProgram &P, ASTContext &C) {
    auto IC = IRPrintContext {
        .ASTCtx = C,
        .NewlineSymbol = "\n"
    };
    for (auto &F: P) {
        SpawnCounter SC;
        SC.countSpawns(F->getEntry());
        INFO {
            for (auto &[S, Cnt]: SC.ClsCntLookup) {
                if (!S) continue;
                auto *CDS = dyn_cast<ClosureDeclIRStmt>(S);
                assert(CDS);
                llvm::outs() << CDS->Fn->getName() << " -> " << wrap(*Cnt, IC) << "\n";
            }
        }

        for (auto &[S, Cnt]: SC.ClsCntLookup) {
            if (!S) continue;
            auto *CDS = dyn_cast<ClosureDeclIRStmt>(S);
            assert(CDS);
            CDS->annotateSpawnCount(SymCountToExpr(Cnt));
        }
    }
}