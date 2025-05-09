#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Analysis/CFG.h>
#include <clang/AST/StmtVisitor.h>
#include <unordered_map>

#include "OpenCilk2IR.hpp"
#include "IR.hpp"
#include "clang/AST/ASTFwd.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCilk.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCilk.h"
#include "llvm/Support/ErrorHandling.h"

class Stmt2IRVisitor : public clang::StmtVisitor<Stmt2IRVisitor> {
  private:
    IRBasicBlock* CurrB;
    std::vector<IRExpr*> ExprStack;

    std::unordered_map<ASTVarRef, IRVarRef> &VarLookup;
    IRFunction *F;
    bool ExprCtx = false;
    bool CallCtx = false;

    IRExpr* getExpr(Expr *E) {
      assert(E);
      ExprCtx = true;
      Stmt2IRVisitor::Visit(E);
      assert(ExprStack.back());
      auto *IRE = ExprStack.back();
      ExprStack.pop_back();
      ExprCtx = false;
      return IRE;
    }

    void handleStmt(Stmt *S) {
      assert(S);
      Stmt2IRVisitor::Visit(S);
      if (ExprStack.size() > 0) {
        // should not be expecting to process statements inside expressions
        assert(ExprStack.size() == 1);
        auto *E = ExprStack.back();
        ExprStack.pop_back();
        auto *EW = new ExprWrapIRStmt(E);
        //assert(isa<CallIRExpr>(EW->Expr.get()));
        CurrB->pushStmtBack((IRStmt*)EW);
      }
    }

    void handleAssign(IRExpr *Dest, IRExpr *Src) {
      if (auto *DI = dyn_cast<IdentIRExpr>(Dest)) {
        auto *CS = new CopyIRStmt(DI->Ident, Src);
        delete DI;
        CurrB->pushStmtBack((IRStmt*)CS);
      } else if (auto *LDest = dyn_cast<IRLvalExpr>(Dest)) {
        auto *SS = new StoreIRStmt(LDest, Src);
        CurrB->pushStmtBack((IRStmt*)SS);
      } else {
        llvm_unreachable("Unsupported LHS of assign");
      }
    }

  public:
    Stmt2IRVisitor(IRFunction *F, std::unordered_map<ASTVarRef, IRVarRef> &VarLookup) : F(F), VarLookup(VarLookup) {
      CurrB = F->createBlock();
    }

    IRBasicBlock* getCurrBlock() {
      return CurrB;
    }
  
    void VisitStmt(Stmt *S) {
      llvm::errs() << "Unhandled statement node: " << S->getStmtClassName() << "\n";
      llvm_unreachable("Unhandled statement node");
    }
    void VisitExpr(Expr *E) {
      llvm::errs() << "Unhandled expr node: " << E->getStmtClassName() << "\n";
      llvm_unreachable("Unhandled expr node");
    }

    ////////////
    // Stmts //
    //////////

    void VisitCompoundStmt(CompoundStmt *Node) {
      for (auto &child: Node->children()) {
        handleStmt(child);
      }
    }

    void VisitDeclStmt(DeclStmt *Node) {
      for (auto *D: Node->decls()) {
        IRVarRef VR;
        if (auto *VD= dyn_cast<ValueDecl>(D)) { 
          Sym VDS = PutSym(VD->getName().str());
          F->Vars.push_back(IRVarDecl {
            .Type = VD->getType(),
            .Name = VDS,
            .DeclLoc = IRVarDecl::LOCAL,
            .Parent = F
          });
          VR = &F->Vars.back();
          VarLookup[VD] = VR;
        } else {
          llvm_unreachable("Unsupported non-named decl encountered");
        }

        if (Node->child_begin() != Node->child_end()) {
          Stmt *S = *Node->child_begin();
          if (auto *E = dyn_cast<Expr>(S)) {
            auto *IE = getExpr(E);
            auto *CopyS = new CopyIRStmt(VR, IE);
            CurrB->pushStmtBack((IRStmt*) CopyS);
          } else {
            llvm_unreachable("Unsupported: RHS of DeclStmt is not an Expr");
          }
        }
      } 
    }

    void VisitIfStmt(IfStmt *IS) {
      // ??
      assert(!IS->hasInitStorage());

      auto *Cond = getExpr(IS->getCond());

      auto *IRS = new IfIRStmt(Cond);
      CurrB->Term = (IRTerminatorStmt*) IRS;
      IRBasicBlock *BranchB = CurrB;
      CurrB = F->createBlock();
      BranchB->Succs.insert(CurrB);
      IRBasicBlock *JoinB = F->createBlock();
      handleStmt(IS->getThen());
      CurrB->Succs.insert(JoinB);

      if (IS->hasElseStorage()) {
        CurrB = F->createBlock();
        BranchB->Succs.insert(CurrB);
        handleStmt(IS->getElse());
        CurrB->Succs.insert(JoinB);
      } else {
        BranchB->Succs.insert(JoinB);
      }
      CurrB = JoinB;
    }


    // drop statements after a return
    void VisitForStmt(ForStmt *FS) {

      auto *ForB = F->createBlock();
      auto *BodyB = F->createBlock();
      auto *IncB = F->createBlock();
      auto *JoinB = F->createBlock();

      CurrB->Succs.insert(ForB);
      IncB->Succs.insert(ForB);
      ForB->Succs.insert(BodyB);
      ForB->Succs.insert(JoinB);

      CurrB = ForB;
      handleStmt(FS->getInit());
      assert(CurrB == ForB);
      IRStmt *InitS = nullptr;
      if (CurrB->back()) {
        InitS = CurrB->back().get();
        InitS->setSilent();
      }      
      
      CurrB = IncB;
      handleStmt(FS->getInc());
      assert(CurrB == IncB);
      IRStmt *IncS = nullptr;
      if (CurrB->back()) {
        IncS = CurrB->back().get();
        IncS->setSilent();
      }

      IRExpr *Cond = nullptr;
      if (FS->getCond()) {
        Cond = getExpr(FS->getCond());  
      }

      auto *ForS = new LoopIRStmt(Cond, IncS, InitS);
      ForB->Term = (IRTerminatorStmt*) ForS;
      
      CurrB = BodyB;
      handleStmt(FS->getBody());
      CurrB->Succs.insert(IncB);

      CurrB = JoinB;
    }

    void VisitReturnStmt(ReturnStmt *RS) {
      assert(!CurrB->Term);
      IRExpr *RE = nullptr;
      if (RS->getRetValue()) {
        RE = getExpr(RS->getRetValue());
      }
      auto *RetS = new ReturnIRStmt(RE);
      CurrB->Term = (IRTerminatorStmt*)RetS;
    }

    void VisitWhileStmt(WhileStmt *WS) {
      auto *WhileB = F->createBlock();
      auto *BodyB = F->createBlock();
      auto *JoinB = F->createBlock();

      CurrB->Succs.insert(WhileB);
      WhileB->Succs.insert(BodyB);
      WhileB->Succs.insert(JoinB);

      IRExpr *Cond = nullptr;
      Cond = getExpr(WS->getCond());

      auto *WhileS = new LoopIRStmt(Cond, nullptr, nullptr);
      WhileB->Term = (IRTerminatorStmt*) WhileS;
      
      CurrB = BodyB;
      handleStmt(WS->getBody());
      CurrB->Succs.insert(WhileB);

      CurrB = JoinB;
    }

    void VisitDoWhileStmt(DoStmt *DS) {
      auto *DoAnnot = new ScopeAnnotIRStmt(ScopeAnnot::SA_DO);
      CurrB->pushStmtBack((IRStmt*)DoAnnot);

      auto *LoopB = F->createBlock();
      auto *JoinB = F->createBlock();

      CurrB->Succs.insert(LoopB);

      CurrB = LoopB;
      handleStmt(DS->getBody());
      CurrB->Succs.insert(LoopB);
      CurrB->Succs.insert(JoinB);

      // probably unneeded?
      // auto *CloseAnnot = new ScopeAnnotIRStmt(ScopeAnnot::SA_CLOSE);
      // CurrB->pushStmtBack((IRStmt*)DoAnnot);

      IRExpr *Cond = getExpr(DS->getCond());
      auto *WhileS = new LoopIRStmt(Cond, nullptr, nullptr);
      CurrB->Term = (IRTerminatorStmt*) WhileS;

      CurrB = JoinB;
    }

    void VisitCilkSyncStmt(CilkSyncStmt *Node) {
      assert(!CurrB->Term);
      auto *SyncS = new SyncIRStmt();
      CurrB->Term = (IRTerminatorStmt*)SyncS;
      auto *JoinB = F->createBlock();
      CurrB->Succs.insert(JoinB);
      CurrB = JoinB;
    }

    ////////////
    // Exprs //
    //////////

    void VisitImplicitCastExpr(ImplicitCastExpr *Node) {
      ExprStack.push_back(getExpr(Node->getSubExpr()));
    } 

    void VisitParenExpr(ParenExpr *Node) {
      ExprStack.push_back(getExpr(Node->getSubExpr()));
    } 

    void VisitIntegerLiteral(IntegerLiteral *Node) {
      auto *LE = new LiteralIRExpr(Node);
      ExprStack.push_back((IRExpr*)LE);
    }

    void VisitStringLiteral(StringLiteral *Node) {
      auto *LE = new LiteralIRExpr(Node);
      ExprStack.push_back((IRExpr*)LE);
    }

    void VisitBinaryOperator(BinaryOperator *Node)  {
      IRExpr *Left = getExpr(Node->getLHS());
      IRExpr *Right = getExpr(Node->getRHS());

      BinopIRExpr::BinopOp Op;
      switch (Node->getOpcode()) {
        case clang::BO_Assign: break;
        case clang::BO_MulAssign: 
        case clang::BO_Mul: Op = BinopIRExpr::BINOP_MUL; break;
        case clang::BO_DivAssign: 
        case clang::BO_Div: Op = BinopIRExpr::BINOP_DIV; break;
        case clang::BO_RemAssign:
        case clang::BO_Rem: Op = BinopIRExpr::BINOP_MOD; break;
        case clang::BO_AddAssign:
        case clang::BO_Add: Op = BinopIRExpr::BINOP_ADD; break;
        case clang::BO_SubAssign:
        case clang::BO_Sub: Op = BinopIRExpr::BINOP_SUB; break;
        case clang::BO_ShlAssign:
        case clang::BO_Shl: Op = BinopIRExpr::BINOP_SHL; break;
        case clang::BO_ShrAssign:
        case clang::BO_Shr: Op = BinopIRExpr::BINOP_SHR; break;
        case clang::BO_LT: Op = BinopIRExpr::BINOP_LT; break;
        case clang::BO_GT: Op = BinopIRExpr::BINOP_GT; break;
        case clang::BO_LE: Op = BinopIRExpr::BINOP_LE; break;
        case clang::BO_GE: Op = BinopIRExpr::BINOP_GE; break;
        case clang::BO_EQ: Op = BinopIRExpr::BINOP_EQ; break;
        case clang::BO_NE: Op = BinopIRExpr::BINOP_NEQ; break;
        case clang::BO_AndAssign:
        case clang::BO_And: Op = BinopIRExpr::BINOP_AND; break;
        case clang::BO_OrAssign:
        case clang::BO_Or: Op = BinopIRExpr::BINOP_OR; break;
        case clang::BO_XorAssign:
        case clang::BO_Xor: Op = BinopIRExpr::BINOP_XOR; break;
        case clang::BO_LAnd: Op = BinopIRExpr::BINOP_LAND; break;
        case clang::BO_LOr: Op = BinopIRExpr::BINOP_LOR; break;
        default: {
          llvm::errs() << "Unknown binary operator: " << Node->getOpcodeStr() << "\n";
          llvm_unreachable("Unknown binary operator");
        }          
      }

      if (Node->getOpcode() == clang::BO_Assign)  {
        assert(!ExprCtx);
        handleAssign(Left, Right);
      } else if (Node->getOpcode() > clang::BO_Assign && Node->getOpcode() <= clang::BO_OrAssign) {
        assert(!ExprCtx);
        auto *DI = dyn_cast<IdentIRExpr>(Left);
        assert(DI);
        auto *LeftC = new IdentIRExpr(DI->Ident);
        BinopIRExpr *BE = new BinopIRExpr(Op, LeftC, Right);
        handleAssign(Left, (IRExpr*)BE);
      } else {
        BinopIRExpr *BE = new BinopIRExpr(Op, Left, Right);
        ExprStack.push_back((IRExpr*) BE);
      }
    }

    void VisitUnaryOperator(UnaryOperator *Node) {
      IRExpr *SE = getExpr(Node->getSubExpr());

      UnopIRExpr::UnopOp Op;
      switch(Node->getOpcode()) {
        case clang::UO_AddrOf: {
          auto *RE = new RefIRExpr(SE);
          ExprStack.push_back((IRExpr*)RE);
          return;
        }
        case clang::UO_Deref: {
          auto *DR = new DRefIRExpr(SE);
          ExprStack.push_back((IRExpr*)DR);
          return;
        }
        case clang::UO_LNot: Op = UnopIRExpr::UNOP_L_NOT; break;
        case clang::UO_Not: Op = UnopIRExpr::UNOP_NOT; break;
        case clang::UO_Minus: Op = UnopIRExpr::UNOP_NEG; break;
        case clang::UO_PreDec: Op = UnopIRExpr::UNOP_PREDEC; break;
        case clang::UO_PostDec: Op = UnopIRExpr::UNOP_POSTDEC; break;
        case clang::UO_PreInc: Op = UnopIRExpr::UNOP_PREINC; break;
        case clang::UO_PostInc: Op = UnopIRExpr::UNOP_POSTINC; break;
        default: {
          llvm::errs() << "Unknown unary operator: " << Node->getOpcodeStr(Node->getOpcode()) << "\n";
          llvm_unreachable("Unknown unary operator");
        }          
      }

      UnopIRExpr *UE = new UnopIRExpr(Op, SE);
      ExprStack.push_back((IRExpr*)UE);
    }

    void VisitArraySubscriptExpr(ArraySubscriptExpr *Node) {
      auto *Arr = getExpr(Node->getBase());

      if (auto *ArrIdent = dyn_cast<IdentIRExpr>(Arr)) {
        auto *ArrRef = ArrIdent->Ident;
        delete ArrIdent;
        auto *Ind = getExpr(Node->getIdx());
        IndexIRExpr *IE = new IndexIRExpr(ArrRef, Ind);
        ExprStack.push_back((IRExpr*)IE);
      } else {
        llvm::errs() << "Unsupported: non-identifier used for array subscript expression.";
        llvm_unreachable("Non-identifier used for array subscript");
      }
    }

    void VisitCallExpr(CallExpr *Node) {
      CallCtx = true;
      auto *FnExpr = getExpr(Node->getCallee());
      CallCtx = false;

      std::vector<IRExpr*> Args;
      for (auto *ArgE: Node->arguments()) {
        Args.push_back(getExpr(ArgE));
      }

      if (auto *FnIdentE = dyn_cast<FIdentIRExpr>(FnExpr)) {
        IRFunRef FnIdent = FnIdentE->FR;
        delete FnIdentE;
        CallIRExpr *CE = new CallIRExpr(FnIdent, Args);
        ExprStack.push_back((IRExpr*)CE);
      } else {
        llvm::errs() << "Unsupported: non function identifier used for call expression.";
        llvm_unreachable("Non function identifier used for call expression.");
      }
    }

    void VisitCilkSpawnExpr(CilkSpawnExpr *Node) {
      auto *SpawnE = getExpr(Node->getSpawnedExpr());
      if (auto *CallE = dyn_cast<CallIRExpr>(SpawnE)) {
        std::vector<IRExpr*> Args;
        for (auto &Arg : CallE->Args) {
          Args.push_back(Arg.get());
          Arg.release();
        }
        IRFunRef FR = CallE->Fn;
        delete CallE;
        auto *SE = new ISpawnIRExpr(FR, Args);
        ExprStack.push_back((IRExpr*)SE);
      }
    }

    void VisitDeclRefExpr(DeclRefExpr *DRE) {
      ASTVarRef VR = DRE->getDecl();
      //printf("looking for %p\n", VR);
      //for (auto it = VarLookup.begin(); it != VarLookup.end(); it++) {
      //  auto &[k, v] = *it;
      //  printf("%p -> %s\n", k, v->Name.c_str());
      //}
      if (VarLookup.find(VR) != VarLookup.end()) {
        auto *IS = new IdentIRExpr(VarLookup[VR]);
        ExprStack.push_back((IRExpr*)IS);
      } else if (CallCtx) {
        IRFunRef FR(VR);
        auto *FS = new FIdentIRExpr(FR);
        ExprStack.push_back((IRExpr*)FS);
      } else {
        llvm::errs() << "Could not find variable: " << VR->getDeclName();
        abort();
      }
    }
};

using FunLookupTy = std::unordered_map<const clang::FunctionDecl*, IRFunction*>;

class Cilk2IRVisitor : public clang::RecursiveASTVisitor<Cilk2IRVisitor> {
private:
  clang::ASTContext *Context;
  IRProgram &P;
  NullStmt &Sentinel;

public:
  FunLookupTy FunLookup;
  explicit Cilk2IRVisitor(clang::ASTContext *Context, IRProgram &P,
                          NullStmt &Sentinel)
      : Context(Context), P(P), Sentinel(Sentinel) {}

  bool VisitFunctionDecl(clang::FunctionDecl *Decl) { 
    if (Decl->getBody()) {
      IRFunction *F = P.createFunc(Decl->getName().str());
      F->Info.RootFun = Decl;
      std::unordered_map<ASTVarRef, IRVarRef> VarLookup;
      for (auto *Param : Decl->parameters()) {
        Sym PSym = PutSym(Param->getName().str());
        F->Vars.push_back(IRVarDecl {
          .Type = Param->getType(),
          .Name = PSym,
          .DeclLoc = IRVarDecl::ARG,
          .Parent = F
        });
        VarLookup[Param] = &F->Vars.back();
      }
      Stmt2IRVisitor sv(F, VarLookup);
      sv.Visit(Decl->getBody());

      F->Exit = sv.getCurrBlock();
      FunLookup[Decl] = F;
    }
    return true;
  }
}; 

class FinalizeVisitor: public IRExprVisitor<FinalizeVisitor> {
  FunLookupTy &FunLookup;
public:
  FinalizeVisitor(FunLookupTy &FunLookup): FunLookup(FunLookup) {}

  void VisitISpawn(ISpawnIRExpr *Node) {
    if (auto *VarRef = std::get_if<ASTVarRef>(&Node->Fn)) {
      auto *FR = dyn_cast<FunctionDecl>(*VarRef); 
      assert(FR);
      if (FunLookup.find(FR) != FunLookup.end()) {
        auto *SpawnDest = FunLookup[FR];
        Node->Fn = SpawnDest;
        SpawnDest->Info.IsTask = true;
      }
    } else {
      llvm_unreachable("Expected an AST variable reference in ISpawnIRExpr");
    }
  }
};

void finalizeFunction(IRFunction *F, FunLookupTy &FunLookup) {
  FinalizeVisitor FV(FunLookup);
  for (auto &B : *F) {
    for (auto &S : *B.get()) {
      FV.VisitStmt(S.get());
    }
    if (B->Term && isa<ReturnIRStmt>(B->Term) && (B.get() != F->Exit)) {
      B->Succs.clear();
      B->Succs.insert(F->Exit);
    }
  }
}

void OpenCilk2IR(IRProgram &P, clang::ASTContext *Context, SourceManager &SM, NullStmt &Sentinel) {
  Cilk2IRVisitor Visitor(Context, P, Sentinel);
  // Only visit declarations declared in the input TU
  auto Decls = Context->getTranslationUnitDecl()->decls();
  for (auto &Decl : Decls) {
    // Ignore declarations out of the main translation unit.
    //
    // SourceManager::isInMainFile method takes into account locations
    // expansion like macro expansion scenario and checks expansion
    // location instead if spelling location if required.
    if (!SM.isInMainFile(Decl->getLocation()))
      continue;
    Visitor.TraverseDecl(Decl);
  }

  for (auto &F: P) {
    finalizeFunction(F.get(), Visitor.FunLookup);
  }
}