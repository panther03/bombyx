#include <clang/AST/ExprCilk.h>
#include <clang/AST/StmtCilk.h>
#include <llvm/ADT/SetVector.h>
#include <set>
#include <unordered_map>

#include "IR.hpp"
#include "util.hpp"
#include "clang/AST/Expr.h"

using namespace llvm;

/////////////////////////////
// CreateContinuationFuns //
///////////////////////////

struct CreateContinuationFuns {
  struct ContFun {
    IRFunction *F;
    std::set<IRVarRef> Args;
    std::set<IRVarRef> Locals;
  };

  // Each "path" in this vector will become a function.
  // The first path corresponds to the original function.
  // Note that the sets need to be ordered to preserve DFS order,
  // allowing us to compute the values actually needed by the path
  std::vector<SetVector<IRBasicBlock *>> Paths;
  std::unordered_map<IRBasicBlock *, int> PathLookup;
  std::vector<ContFun> ContFuns;

private:
  IRBasicBlock *duplicateBasicBlock(IRBasicBlock *B,
                                    SetVector<IRBasicBlock *> &CurrPath) {
    IRBasicBlock *CloneBB = B->getParent()->createBlock();
    B->clone(CloneBB);

    // Copy over successors
    for (IRBasicBlock *Succ : B->Succs) {
      CloneBB->Succs.insert(Succ);
    }

    B->iteratePreds([&](IRBasicBlock *Pred) -> void {
      if (CurrPath.contains(Pred)) {
        Pred->Succs.remove(B);
        Pred->Succs.insert(CloneBB);
      }
    });
    return CloneBB;
  }

  void createSyncPaths(IRFunction &F) {
    Paths.resize(1);

    std::vector<std::pair<IRBasicBlock *, int>> WorkList;
    WorkList.push_back(std::make_pair(F.getEntry(), 0));

    int Fresh = 0;
    while (!WorkList.empty()) {
      auto [B, CurrLevel] = WorkList.back();
      WorkList.pop_back();

      if (PathLookup.find(B) != PathLookup.end()) {
        // if we've seen it before but it is in the same level, we just skip
        if (PathLookup[B] == CurrLevel) {
          continue;
        } else {
          B = duplicateBasicBlock(B, Paths[CurrLevel]);
        }
      }

      Paths[CurrLevel].insert(B);
      PathLookup.insert(std::make_pair(B, CurrLevel));

      if (B->Term && isa<SyncIRStmt>(B->Term)) {
        // sync instruction should have only one successor
        IRBasicBlock *SISucc = *(B->Succs.begin());
        if (PathLookup.find(SISucc) != PathLookup.end()) {
          continue;
        }
        Fresh++;
        Paths.resize(Fresh + 1);
        CurrLevel = Fresh;
      }

      for (auto *Succ : B->Succs) {
        WorkList.push_back(std::make_pair(Succ, CurrLevel));
      }
    }

    int i = 0;
    INFO {
      for (auto &Path : Paths) {
        llvm::outs() << "path " << i << ": ";
        for (auto &B : Path) {
          llvm::outs() << "BB" << B->getInd() << ", ";
        }
        llvm::outs() << "\n";
        i++;
      }
    }
  }

  void analyzeStmt(IRStmt *S, std::set<IRVarRef> &free, std::set<IRVarRef> &refd) {
    std::set<IRVarRef> V;
      ExprIdentifierVisitor _(S, [&](auto &VR,bool lhs) {
        if (!lhs) {
          V.insert(VR);
        }
      });
      for (auto *D: V) {
        if (refd.find(D) == refd.end()) {
          free.insert(D);
          refd.insert(D);
        }
      }

      if (auto *CS = dyn_cast<CopyIRStmt>(S)) {
        if (refd.find(CS->Dest) == refd.end()) {
          refd.insert(CS->Dest);
          free.erase(CS->Dest);
        }
    }
  }

  // TODO: this function is not going to handle more complex cases like
  // a store being present in only one branch and a load at the join
  // (so the value is free for the whole function)
  void analyzePath(ContFun &CF,
                   SetVector<IRBasicBlock *> &path,
                   std::set<IRVarRef> *inFrees) {
    std::set<IRVarRef> &free = CF.Args;
    std::set<IRVarRef> &refd = CF.Locals;

    // Values that will be used in the proceeding blocks regardless of whether
    // they are used here. Only removed if created here.
    if (inFrees) {
      for (auto &v : *inFrees) {
        free.insert(v);
      }
    }

    for (auto &bb : path) {
      for (auto &S : *bb) {
        analyzeStmt(S.get(), free, refd);
      }
      if (bb->Term) {
        analyzeStmt(bb->Term, free, refd);
      }
    }
    for (auto *v : free) {
      if (refd.find(v) != refd.end()) {
        refd.erase(v);
      }
    }
  }

public:
  CreateContinuationFuns(IRFunction &F) {
    INFO {
      F.cleanVars();
      outs() << F.getName() << " (init):\n";
      F.printVars(outs());
    }
    createSyncPaths(F);

    if (Paths.size() <= 1) {
      // Not a function with syncs
      return;
    }

    std::deque<IRBasicBlock *> WorkList;
    for (auto &B : F) {
      // don't care about the original function
      if (PathLookup[B.get()] == 0)
        continue;

      // every function we will need to create has its
      // own terminator block, add these
      if (B->Succs.empty()) {
        WorkList.push_back(B.get());
      }
    }

    for (int p = 0; p < Paths.size() - 1; p++) {
      std::string CfName = F.getName() + "_cont" + std::to_string(p);
      ContFun CF = ContFun {
        .F = F.getParent()->createFunc(CfName),
        .Args = std::set<IRVarRef>(),
        .Locals = std::set<IRVarRef>()
      };
      CF.F->Info.IsTask = true;
      ContFuns.push_back(CF);
    }
    // just used for checking assumptions
    std::vector<bool> visited(Paths.size() - 1, 0);

    while (!WorkList.empty()) {
      auto *bb = WorkList.front();
      WorkList.pop_front();

      assert(PathLookup.find(bb) != PathLookup.end());
      if (PathLookup[bb] == 0)
        continue;
      int path = PathLookup[bb] - 1;
      // we should only visit a path once, because sync continue blocks should
      // only have one parent
      // TODO: does this assumption make sense?
      assert(!visited[path]);

      std::set<IRVarRef> *inFrees = NULL;
      if (!bb->Succs.empty()) {
        if (auto *succBb = *(bb->Succs.begin())) {
          assert(PathLookup.find(succBb) != PathLookup.end());
          inFrees = &(ContFuns[PathLookup[succBb] - 1].Args);
        }
      }

      analyzePath(ContFuns[path],
                  Paths[path + 1], inFrees);
      visited[path] = true;

      auto *startBb = Paths[path + 1][0];

      startBb->iteratePreds(
          [&](IRBasicBlock *Pred) -> void { WorkList.push_front(Pred); });
    }

    for (int p = 0; p < Paths.size(); p++) {
      auto &Path = Paths[p];
      std::unordered_map<IRVarRef, IRVarRef> Remap;

      if (p > 0) { 
        auto &CF = ContFuns[p-1];
        for (auto *Arg: CF.Args) {
          CF.F->Vars.push_back(IRVarDecl {
            .Type = Arg->Type,
            .Name = Arg->Name,
            .DeclLoc = IRVarDecl::ARG,
          });
          Remap[Arg] = &(CF.F->Vars.back());
        }
        for (auto *Local: CF.Locals) {
          CF.F->Vars.push_back(IRVarDecl {
            .Type = Local->Type,
            .Name = Local->Name,
            .DeclLoc = IRVarDecl::LOCAL,
          });
          Remap[Local] = &(CF.F->Vars.back());
        }
      }

      for (auto *B : Path) {
        IRFunction *SpawnNextDest = nullptr;
        if (B->Term) {
          if (isa<SyncIRStmt>(B->Term)) {
            auto *succBb = *(B->Succs.begin());
            assert(succBb);
            assert(PathLookup.find(succBb) != PathLookup.end());
            assert(PathLookup[succBb] > 0);
            delete B->Term;
            SpawnNextDest = ContFuns[PathLookup[succBb] - 1].F;
            B->Term = new SpawnNextIRStmt(SpawnNextDest);
            B->Succs.clear();
          }
        }
        if (p > 0) {
          B->getParent()->moveBlock(B, ContFuns[p - 1].F);
          auto RemapCB = [&](auto &VR, bool lhs){
            assert(Remap.find(VR) != Remap.end());
            VR = Remap[VR];
          };
          for (auto &S: *B) {
            ExprIdentifierVisitor e(S.get(), RemapCB);
          }
          if (B->Term) {
            ExprIdentifierVisitor e(B->Term, RemapCB);
          }
        }
        if (SpawnNextDest) {
          B->getParent()->Info.SpawnNextList.insert(SpawnNextDest);
        }
      }
    }

    F.cleanVars();

    INFO {
      outs() << F.getName() << ":\n";
      F.printVars(outs());
  
      int I = 0;
      for (auto &CF : ContFuns) {
        outs() << "ContF" << CF.F->getInd() << ":\n";
        CF.F->printVars(outs());
        I++;
      } 
    }
  }
};

//////////////////////////
// FinalizeExplicitCPS //
////////////////////////

struct FinalizeExplicitCPS {
  std::vector<std::pair<IRBasicBlock*, IRStmt*>> ClosureDeclWorkList;

  class ScopeStartMapper : public ScopedIRTraverser {
    private:
      std::unordered_map<IRBasicBlock *, IRBasicBlock *> &ScopeStarts;
      std::vector<IRBasicBlock *> ScopeStack;
      bool pushNext = false;
    
      void handleScope(ScopeEvent SE) override {
        if (SE == ScopeEvent::Open || SE == ScopeEvent::Else) {
          pushNext = true;
        }
        if (SE == ScopeEvent::Close || SE == ScopeEvent::Else) {
          ScopeStack.pop_back();
        }
      }
    
      void visitBlock(IRBasicBlock *B) override {
        if (pushNext) {
          ScopeStack.push_back(B);
          pushNext = false;
        }
        ScopeStarts[B] = ScopeStack.back();
      }
    
    public:
      ScopeStartMapper(
          std::unordered_map<IRBasicBlock *, IRBasicBlock *> &ScopeStarts)
          : ScopeStarts(ScopeStarts) {}
    
      void reset(IRBasicBlock *Entry) {
        ScopeStack.clear();
        ScopeStack.push_back(Entry);
      }
  };

  void PlaceClosureDecl(IRBasicBlock *StartB, IRStmt *CDS) {
    size_t ind = 0;
    do {
      ind = 0;
      for (auto it = StartB->begin(); it != StartB->end(); ind++, it++) {
        if (isa<ESpawnIRStmt>(it->get())) {
          goto found_decl_loc;
        }
      }
      if (StartB->Succs.size() != 1) {
        break;
      }
      StartB = StartB->Succs[0];
    } while (true);

    found_decl_loc:
    StartB->insertAt(std::min(ind, StartB->lenInsns()), CDS);
  }

  void CreateClosureDecls(
    IRFunction *F,
    std::unordered_map<IRBasicBlock *, IRBasicBlock *> &ScopeStarts) {
    for (auto &B : *F) {
      if (B->Term) {
        if (auto *SNTerm = dyn_cast<SpawnNextIRStmt>(B->Term)) {
          auto *StartB = ScopeStarts[B.get()];
          assert(StartB);
    
          auto *DeclS = new ClosureDeclIRStmt(SNTerm->Fn);
          SNTerm->Decl = DeclS;
          for (auto &DestVar: SNTerm->Fn->Vars) {
            if (DestVar.DeclLoc == IRVarDecl::ARG) {
              // find the same variable in this function's list
              for (auto &SrcVar: F->Vars) {
                if (SrcVar.Name == DestVar.Name && SrcVar.Type == DestVar.Type) {
                  DeclS->addCallerToCaleeVarMapping(&SrcVar, &DestVar);
                }
              }
            }
          }
          ClosureDeclWorkList.push_back(std::make_pair(StartB, DeclS));
        }
      }
    }
  }

  SpawnNextIRStmt *DFSTillSpawnNext(IRBasicBlock *StartB) {
    std::vector<IRBasicBlock *> WorkList;
    std::set<IRBasicBlock *> SeenList;
    WorkList.push_back(StartB);

    SpawnNextIRStmt *FoundSpawnNext = nullptr;

    while (!WorkList.empty()) {
      IRBasicBlock *B = WorkList.back();
      WorkList.pop_back();
      SeenList.insert(B);

      if (B->Term && isa<SpawnNextIRStmt>(B->Term)) {
        if (FoundSpawnNext) {
          // TODO improve this error message
          PANIC("Ambiguous spawnNext for spawn!");
        } else {
          FoundSpawnNext = dyn_cast<SpawnNextIRStmt>(B->Term);
        }
      }

      for (auto &Succ : B->Succs) {
        if (SeenList.find(Succ) == SeenList.end()) {
          WorkList.push_back(Succ);
        }
      }
    }

    return FoundSpawnNext;
  }

  void MakeSpawnsExplicit(IRFunction *F) {
    for (auto &B: *F) {
      for (auto &S: *B) {
        // TODO handle void spawn
        IRLvalExpr *Dest = nullptr;
        ISpawnIRExpr *IS = nullptr;
        bool Local = false;
        if (auto CS = dyn_cast<CopyIRStmt>(S.get())) {
          // TODO: a check for other ispawns embedded in the expr
          IS = dyn_cast<ISpawnIRExpr>(CS->Src.get());
          if (IS) {
            CS->Dest->IsEphemeral = true;
            Dest = new IdentIRExpr(CS->Dest);
            CS->Src.release();
          }
          Local = true;
        } else if (auto SS = dyn_cast<StoreIRStmt>(S.get())) {
          IS = dyn_cast<ISpawnIRExpr>(SS->Src.get());
          if (IS) {
            Dest = SS->Dest.release();
            SS->Src.release();
          }
        }
        if (Dest && IS) {
          // Find corresponding spawn next statement.
          SpawnNextIRStmt *SN = DFSTillSpawnNext(B.get());
          if (!SN) {
            PANIC("Spawn has a return value, but no corresponding spawn "
              "next..");
          }

          std::vector<IRExpr*> Args;
          for (auto &Arg : IS->Args) {
            Args.push_back(Arg.get());
            Arg.release();
          }

          IRFunction *Fn = nullptr;
          if (auto SFn = std::get_if<IRFunction *>(&IS->Fn)) {
            Fn = *SFn;
          } else {
            PANIC("Implicit spawn destination still unknown, needs to be known for explicit conversion");
          }

          delete IS;


          S = std::make_unique<ESpawnIRStmt>(Dest, Fn, SN, Args, Local);
          F->Info.SpawnList.insert(Fn);
        }
      }
    }
  }

  FinalizeExplicitCPS(IRFunction *F) {
    std::unordered_map<IRBasicBlock *, IRBasicBlock *> ScopeStarts;
    ScopeStartMapper SSM(ScopeStarts);
    SSM.reset(F->getEntry());
    SSM.traverse(*F);
    CreateClosureDecls(F, ScopeStarts);
    MakeSpawnsExplicit(F);
    for (auto &[StartB, CDS] : ClosureDeclWorkList) {
      PlaceClosureDecl(StartB, CDS);
    }
  }
};

void MakeExplicit(IRProgram &P) {
  std::vector<IRFunction *> WorkList;
  for (auto &F : P) {
    WorkList.push_back(F.get());
  }

  for (auto &F : WorkList) {
    CreateContinuationFuns CCF(*F);
    std::vector<IRFunction*> FWorkList{F};
    for (auto &CF : CCF.ContFuns) {
      FWorkList.push_back(CF.F);
    }
    for (auto *F: FWorkList) {
      FinalizeExplicitCPS FC(F);
    }
  }
}