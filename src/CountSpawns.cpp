#include "CountSpawns.hpp"
#include "IR.hpp"
#include <unordered_map>

class SymbolicCount {
public:
    // To expand this with other types of expressions, can make an ADT with other variants,
    // like IfCountExpr & so on.
    using ForCountExpr = std::pair<IRExpr*, SymbolicCount*>;
    int N = 0;
    std::list<ForCountExpr> S;

    SymbolicCount(int N): N(N) {}
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

class SpawnCounter {
public:
    std::unordered_map<IRStmt*, SymbolicCount*> ClsCntLookup;
private:
    std::vector<IRExpr> GlobalEnv;
    std::unordered_map<IRBasicBlock*, int> JoinCount;
    SymbolicCount *InvalidCount;
    using EnvTy = std::unordered_map<IRVarRef, int>;

    IRExpr* extractLoopCount(LoopIRStmt *LS, EnvTy &PreEnv, EnvTy &PostEnv) {
        return nullptr;
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
        for (auto &[_,SC] : ClsCntLookup) {
            delete SC;
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



void CountSpawns(IRProgram &P) {
    for (auto &F: P) {
        SpawnCounter SC;
        SC.countSpawns(F->getEntry());
        for (auto &[S, Cnt]: SC.ClsCntLookup) {
            if (!S) continue;
            auto *CDS = dyn_cast<ClosureDeclIRStmt>(S);
            assert(CDS);
            llvm::outs() << CDS->Fn->getName() << " -> " << Cnt->N << "\n";
        }
    }
}