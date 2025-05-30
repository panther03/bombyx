#include "DAE.hpp"
#include "IR.hpp"

void DAEAtInd(IRProgram &P, IRBasicBlock *B, int Ind) {
    static int DaeCount = 0;
    assert(Ind > 1);
    auto *RB = B->splitAt(Ind);
    B->removeAt(Ind-2);
    B->Term = new SyncIRStmt();

    for (auto *S: B->Succs) {
        RB->Succs.insert(S);
    }
    B->Succs.clear();
    B->Succs.insert(RB);
    
    auto *AccessStmt = dyn_cast<CopyIRStmt>(B->getAt(Ind-2));
    if (!AccessStmt) {
        PANIC("DAE annotation should be proceeded by a copy IR statement (decouplable access)");
    }

    IRFunction *LoadF = P.createFunc(B->getParent()->getName() + "_dae_" + std::to_string(DaeCount++), AccessStmt->Dest->Type);
    LoadF->Info.IsTask = true;

    std::unordered_map<IRVarRef, IRVarRef> Remap;
    std::vector<IRExpr*> CallInArgs;

    ExprIdentifierVisitor _ (AccessStmt->Src.get(), [&](auto &VR, bool lhs) {
        if (Remap.find(VR) == Remap.end()) {
            IRVarDecl NewDecl = *VR;
            NewDecl.DeclLoc = IRVarDecl::ARG;
            LoadF->Vars.push_back(NewDecl);
            Remap[VR] = &LoadF->Vars.back();
            VR = Remap[VR];

            CallInArgs.push_back(new IdentIRExpr(VR));
        } else {
            VR = Remap[VR];
        }
    });

    auto *LB = LoadF->createBlock();
    LB->Term = new ReturnIRStmt(AccessStmt->Src.release());

    AccessStmt->Src = std::make_unique<ISpawnIRExpr>(LoadF, CallInArgs);
}

void DAE(IRProgram &P) {
    std::vector<IRFunction*> FWorkList;
    for (auto &F: P) {
        FWorkList.push_back(F.get());
    }

    for (auto *F: FWorkList) {
        std::vector<std::pair<IRBasicBlock*, int>> SplitWorkList;

        for (auto &B: *F) {
            int i = 0;
            bool dae_already = false;
            for (auto &S: *B) {
                if (auto *SAS = dyn_cast<ScopeAnnotIRStmt>(S.get())) {
                    if (SAS->SA == ScopeAnnot::SA_DAE_HERE) {
                        if (dae_already) {
                            PANIC("unsupported: 2 DAE in the same basic block");
                        }
                        SplitWorkList.push_back(std::make_pair(B.get(), i+2));
                        dae_already = true;
                    }
                }
                i++;
            }
        }

        for (auto &[B, ind]: SplitWorkList) {
            DAEAtInd(P, B, ind);
        }
    }
}