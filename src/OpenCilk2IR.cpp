#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/StmtVisitor.h>
#include <clang/Analysis/CFG.h>
#include <unordered_map>

#include "IR.hpp"
#include "OpenCilk2IR.hpp"
#include "clang/AST/ASTFwd.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCilk.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCilk.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/Support/ErrorHandling.h"

//////////////////////////////////
// Scan AST for relevant tasks //
////////////////////////////////
using FunLookupTy =
    std::unordered_map<const clang::FunctionDecl *, IRFunction *>;

class CilkAnalyzeVisitor
    : public clang::RecursiveASTVisitor<CilkAnalyzeVisitor> {
  FunctionDecl *CurrF = nullptr;
  bool SpawnCtx = true;

public:
  std::set<FunctionDecl *> Tasks;
  std::set<FunctionDecl *> TaskCallers;
  explicit CilkAnalyzeVisitor() {}

  bool VisitFunctionDecl(FunctionDecl *Decl) {
    CurrF = Decl;
    return true;
  }

  void HandleSpawn(CallExpr *Expr) {
    if (Expr->getDirectCallee()) {
      Tasks.insert(Expr->getDirectCallee());
      TaskCallers.insert(CurrF);
    } else {
      PANIC("cannot deduce destination of spawn");
    }
  }

  bool VisitCallExpr(CallExpr *Expr) {
    if (Tasks.find(Expr->getDirectCallee()) != Tasks.end()) {
      HandleSpawn(Expr);
    }
    return true;
  }

  bool VisitCilkSpawnExpr(CilkSpawnExpr *Expr) {
    if (auto *CExpr = dyn_cast<CallExpr>(Expr->getSpawnedExpr())) {
      HandleSpawn(CExpr);
    } else {
      PANIC("unrecognized spawned expression, should be a function call");
    }
    return true;
  }

  bool VisitCilkSpawnStmt(CilkSpawnStmt *Stmt) {
    if (auto *CExpr = dyn_cast<CallExpr>(Stmt->getSpawnedStmt())) {
      HandleSpawn(CExpr);
    } else {
      PANIC("unrecognized spawned expression, should be a function call");
    }
    return true;
  }
};

////////////////////////
// Convert AST to IR //
//////////////////////

class Stmt2IRVisitor : public clang::StmtVisitor<Stmt2IRVisitor> {
private:
  IRBasicBlock *CurrB;
  std::vector<IRExpr *> ExprStack;

  std::unordered_map<ASTVarRef, IRVarRef> &VarLookup;
  std::set<FunctionDecl *> Tasks;
  IRFunction *F;
  bool ExprCtx = false;
  bool CallCtx = false;
  bool SpawnCtx = false;
  bool SyncNext = false;

  IRExpr *getExpr(Expr *E) {
    assert(E);
    ExprCtx = true;
    Stmt2IRVisitor::Visit(E);
    assert(ExprStack.back());
    auto *IRE = ExprStack.back();
    ExprStack.pop_back();
    ExprCtx = false;
    return IRE;
  }

  void pushIRStmt(IRStmt *S) {
    CurrB->pushStmtBack(S);
    if (SyncNext) {
      VisitCilkSyncStmt(nullptr);
      SyncNext = false;
    }
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
      // assert(isa<CallIRExpr>(EW->Expr.get()));
      pushIRStmt((IRStmt *)EW);
    }
  }

  void handleAssign(IRExpr *Dest, IRExpr *Src) {
    if (auto *DI = dyn_cast<IdentIRExpr>(Dest)) {
      auto *CS = new CopyIRStmt(DI->Ident, Src);
      delete DI;
      pushIRStmt((IRStmt *)CS);
    } else if (auto *LDest = dyn_cast<IRLvalExpr>(Dest)) {
      auto *SS = new StoreIRStmt(LDest, Src);
      pushIRStmt((IRStmt *)SS);
    } else {
      llvm_unreachable("Unsupported LHS of assign");
    }
  }

public:
  Stmt2IRVisitor(IRFunction *F,
                 std::unordered_map<ASTVarRef, IRVarRef> &VarLookup,
                 std::set<FunctionDecl *> Tasks)
      : F(F), VarLookup(VarLookup), Tasks(Tasks) {
    CurrB = F->createBlock();
  }

  IRBasicBlock *getCurrBlock() { return CurrB; }

  void VisitStmt(Stmt *S) {
    llvm::errs() << "Unhandled statement node: " << S->getStmtClassName()
                 << "\n";
    llvm_unreachable("Unhandled statement node");
  }
  void VisitExpr(Expr *E) {
    llvm::errs() << "Unhandled expr node: " << E->getStmtClassName() << "\n";
    llvm_unreachable("Unhandled expr node");
  }

  void HandleCilkSpawn(IRExpr *SpawnedE) {
    if (auto *CallE = dyn_cast<CallIRExpr>(SpawnedE)) {
      std::vector<IRExpr *> Args;
      for (auto &Arg : CallE->Args) {
        Args.push_back(Arg.get());
        Arg.release();
      }
      IRFunRef FR = CallE->Fn;
      delete CallE;
      auto *SE = new ISpawnIRExpr(FR, Args);
      ExprStack.push_back((IRExpr *)SE);
    } else {
      PANIC("unsupported: cilk spawn on non call expression");
    }
  }

  ////////////
  // Stmts //
  //////////

  void VisitCompoundStmt(CompoundStmt *Node) {
    for (auto &child : Node->children()) {
      handleStmt(child);
    }
  }

  void VisitLabelStmt(LabelStmt *Node) {
    if (Node->getDecl()->getName() == "__bombyx_dae_here") {
      pushIRStmt(new ScopeAnnotIRStmt(ScopeAnnot::SA_DAE_HERE));
      handleStmt(Node->getSubStmt());
    } else {
      PANIC("unsupported: label statement")
    }
  }

  void VisitDeclStmt(DeclStmt *Node) {
    for (auto *D : Node->decls()) {
      IRVarRef VR;
      if (auto *VD = dyn_cast<ValueDecl>(D)) {
        Sym VDS = PutSym(VD->getName().str());
        F->Vars.push_back(IRVarDecl{
            .Type = VD->getType(),
            .Name = VDS,
            .DeclLoc = IRVarDecl::LOCAL,
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
          pushIRStmt((IRStmt *)CopyS);
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
    CurrB->Term = (IRTerminatorStmt *)IRS;
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
    ForB->Term = (IRTerminatorStmt *)ForS;

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
    CurrB->Term = (IRTerminatorStmt *)RetS;
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
    WhileB->Term = (IRTerminatorStmt *)WhileS;

    CurrB = BodyB;
    handleStmt(WS->getBody());
    CurrB->Succs.insert(WhileB);

    CurrB = JoinB;
  }

  void VisitDoWhileStmt(DoStmt *DS) {
    auto *DoAnnot = new ScopeAnnotIRStmt(ScopeAnnot::SA_DO);
    pushIRStmt((IRStmt *)DoAnnot);

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
    CurrB->Term = (IRTerminatorStmt *)WhileS;

    CurrB = JoinB;
  }

  void VisitCilkSyncStmt(CilkSyncStmt *Node) {
    assert(!CurrB->Term);
    auto *SyncS = new SyncIRStmt();
    CurrB->Term = (IRTerminatorStmt *)SyncS;
    auto *JoinB = F->createBlock();
    CurrB->Succs.insert(JoinB);
    CurrB = JoinB;
  }

  void VisitCilkSpawnStmt(CilkSpawnStmt *Stmt) {
    Expr *E = dyn_cast<Expr>(Stmt->getSpawnedStmt());
    if (!E)  {
      PANIC("unrecognized expression in cilk spawn statement");
    }
    SpawnCtx = true;
    auto *SpawnE = getExpr(E);
    SpawnCtx = false;
    HandleCilkSpawn(SpawnE);
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

  void VisitMemberExpr(MemberExpr *Node) {
    IdentIRExpr *IE = dyn_cast<IdentIRExpr>(getExpr(Node->getBase()));
    assert(IE);
    auto *AE = new AccessIRExpr(IE->Ident, Node->getMemberDecl()->getName().str(), Node->isArrow());
    delete IE;
    ExprStack.push_back(AE);
  }

  void VisitIntegerLiteral(IntegerLiteral *Node) {
    auto *LE = new ASTLiteralIRExpr(Node);
    ExprStack.push_back((IRExpr *)LE);
  }

  void VisitFloatingLiteral(FloatingLiteral *Node) {
    auto *LE = new ASTLiteralIRExpr(Node);
    ExprStack.push_back((IRExpr *)LE);
  }

  void VisitStringLiteral(StringLiteral *Node) {
    auto *LE = new ASTLiteralIRExpr(Node);
    ExprStack.push_back((IRExpr *)LE);
  }

  void VisitBinaryOperator(BinaryOperator *Node) {
    IRExpr *Left = getExpr(Node->getLHS());
    IRExpr *Right = getExpr(Node->getRHS());

    BinopIRExpr::BinopOp Op;
    switch (Node->getOpcode()) {
    case clang::BO_Assign:
      break;
    case clang::BO_MulAssign:
    case clang::BO_Mul:
      Op = BinopIRExpr::BINOP_MUL;
      break;
    case clang::BO_DivAssign:
    case clang::BO_Div:
      Op = BinopIRExpr::BINOP_DIV;
      break;
    case clang::BO_RemAssign:
    case clang::BO_Rem:
      Op = BinopIRExpr::BINOP_MOD;
      break;
    case clang::BO_AddAssign:
    case clang::BO_Add:
      Op = BinopIRExpr::BINOP_ADD;
      break;
    case clang::BO_SubAssign:
    case clang::BO_Sub:
      Op = BinopIRExpr::BINOP_SUB;
      break;
    case clang::BO_ShlAssign:
    case clang::BO_Shl:
      Op = BinopIRExpr::BINOP_SHL;
      break;
    case clang::BO_ShrAssign:
    case clang::BO_Shr:
      Op = BinopIRExpr::BINOP_SHR;
      break;
    case clang::BO_LT:
      Op = BinopIRExpr::BINOP_LT;
      break;
    case clang::BO_GT:
      Op = BinopIRExpr::BINOP_GT;
      break;
    case clang::BO_LE:
      Op = BinopIRExpr::BINOP_LE;
      break;
    case clang::BO_GE:
      Op = BinopIRExpr::BINOP_GE;
      break;
    case clang::BO_EQ:
      Op = BinopIRExpr::BINOP_EQ;
      break;
    case clang::BO_NE:
      Op = BinopIRExpr::BINOP_NEQ;
      break;
    case clang::BO_AndAssign:
    case clang::BO_And:
      Op = BinopIRExpr::BINOP_AND;
      break;
    case clang::BO_OrAssign:
    case clang::BO_Or:
      Op = BinopIRExpr::BINOP_OR;
      break;
    case clang::BO_XorAssign:
    case clang::BO_Xor:
      Op = BinopIRExpr::BINOP_XOR;
      break;
    case clang::BO_LAnd:
      Op = BinopIRExpr::BINOP_LAND;
      break;
    case clang::BO_LOr:
      Op = BinopIRExpr::BINOP_LOR;
      break;
    default: {
      llvm::errs() << "Unknown binary operator: " << Node->getOpcodeStr()
                   << "\n";
      llvm_unreachable("Unknown binary operator");
    }
    }

    if (Node->getOpcode() == clang::BO_Assign) {
      assert(!ExprCtx);
      handleAssign(Left, Right);
    } else if (Node->getOpcode() > clang::BO_Assign &&
               Node->getOpcode() <= clang::BO_OrAssign) {
      assert(!ExprCtx);
      auto *DI = dyn_cast<IdentIRExpr>(Left);
      assert(DI);
      auto *LeftC = new IdentIRExpr(DI->Ident);
      BinopIRExpr *BE = new BinopIRExpr(Op, LeftC, Right);
      handleAssign(Left, (IRExpr *)BE);
    } else {
      BinopIRExpr *BE = new BinopIRExpr(Op, Left, Right);
      ExprStack.push_back((IRExpr *)BE);
    }
  }

  void VisitUnaryOperator(UnaryOperator *Node) {
    IRExpr *SE = getExpr(Node->getSubExpr());

    UnopIRExpr::UnopOp Op;
    switch (Node->getOpcode()) {
    case clang::UO_AddrOf: {
      auto *RE = new RefIRExpr(SE);
      ExprStack.push_back((IRExpr *)RE);
      return;
    }
    case clang::UO_Deref: {
      auto *DR = new DRefIRExpr(SE);
      ExprStack.push_back((IRExpr *)DR);
      return;
    }
    case clang::UO_LNot:
      Op = UnopIRExpr::UNOP_L_NOT;
      break;
    case clang::UO_Not:
      Op = UnopIRExpr::UNOP_NOT;
      break;
    case clang::UO_Minus:
      Op = UnopIRExpr::UNOP_NEG;
      break;
    case clang::UO_PreDec:  // Op =  UnopIRExpr::UNOP_PREDEC; break;
    case clang::UO_PostDec: // Op =  UnopIRExpr::UNOP_POSTDEC; break;
    case clang::UO_PreInc:  // Op =  UnopIRExpr::UNOP_PREINC; break;
    case clang::UO_PostInc:
      PANIC("TODO: need to make increment/decrement lower to a statement");
      break; // Op = UnopIRExpr::UNOP_POSTINC; break;
    default: {
      llvm::errs() << "Unknown unary operator: "
                   << Node->getOpcodeStr(Node->getOpcode()) << "\n";
      llvm_unreachable("Unknown unary operator");
    }
    }

    UnopIRExpr *UE = new UnopIRExpr(Op, SE);
    ExprStack.push_back((IRExpr *)UE);
  }

  void VisitArraySubscriptExpr(ArraySubscriptExpr *Node) {
    auto *Arr = getExpr(Node->getBase());

    if (auto *ArrLval = dyn_cast<IRLvalExpr>(Arr)) {
      auto *Ind = getExpr(Node->getIdx());
      IndexIRExpr *IE = new IndexIRExpr(ArrLval, Ind);
      ExprStack.push_back((IRExpr *)IE);
    } else {
      llvm::errs()
          << "Unsupported: non-lvalue used for array subscript expression.";
      llvm_unreachable("Non-lvalue used for array subscript");
    }
  }

  void VisitCallExpr(CallExpr *Node) {
    CallCtx = true;
    auto *FnExpr = getExpr(Node->getCallee());
    CallCtx = false;

    std::vector<IRExpr *> Args;
    for (auto *ArgE : Node->arguments()) {
      Args.push_back(getExpr(ArgE));
    }

    if (auto *FnIdentE = dyn_cast<FIdentIRExpr>(FnExpr)) {
      IRFunRef FnIdent = FnIdentE->FR;
      delete FnIdentE;
      CallIRExpr *CE = new CallIRExpr(FnIdent, Args);
      ExprStack.push_back((IRExpr *)CE);
    } else {
      llvm::errs()
          << "Unsupported: non function identifier used for call expression.";
      llvm_unreachable("Non function identifier used for call expression.");
    }

    if (!SpawnCtx & Tasks.find(Node->getDirectCallee()) != Tasks.end()) {
      auto *SpawnedE = ExprStack.back();
      ExprStack.pop_back();
      HandleCilkSpawn(SpawnedE);
      SyncNext = true;
    }
  }

  void VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *Node) {
    ExprStack.push_back(new ASTLiteralIRExpr(Node));
  }

  void VisitCStyleCastExpr(CStyleCastExpr *Node) {
    auto *E = getExpr(Node->getSubExpr());
    auto *CastE = new CastIRExpr(Node->getType(), E);
    ExprStack.push_back(CastE);
  }

  

  void VisitCilkSpawnExpr(CilkSpawnExpr *Node) {
    SpawnCtx = true;
    auto *SpawnE = getExpr(Node->getSpawnedExpr());
    SpawnCtx = false;
    HandleCilkSpawn(SpawnE);
  }

  void VisitDeclRefExpr(DeclRefExpr *DRE) {
    ASTVarRef VR = DRE->getDecl();
    // printf("looking for %p\n", VR);
    // for (auto it = VarLookup.begin(); it != VarLookup.end(); it++) {
    //   auto &[k, v] = *it;
    //   printf("%p -> %s\n", k, v->Name.c_str());
    // }
    if (VarLookup.find(VR) != VarLookup.end()) {
      auto *IS = new IdentIRExpr(VarLookup[VR]);
      ExprStack.push_back((IRExpr *)IS);
    } else if (CallCtx) {
      IRFunRef FR(VR);
      auto *FS = new FIdentIRExpr(FR);
      ExprStack.push_back((IRExpr *)FS);
    } else {
      // TODO put a warning here instead
      // llvm::errs() << "Could not find variable: " << VR->getDeclName();
      // abort();
      ExprStack.push_back(new ASTLiteralIRExpr(DRE));
    }
  }
};

class Cilk2IRVisitor : public clang::RecursiveASTVisitor<Cilk2IRVisitor> {
private:
  clang::ASTContext *Context;
  IRProgram &P;
  std::set<FunctionDecl *> &Tasks;
  std::set<FunctionDecl *> &TaskCallers;

public:
  FunLookupTy FunLookup;
  explicit Cilk2IRVisitor(clang::ASTContext *Context, IRProgram &P,
                          std::set<FunctionDecl *> &Tasks,
                          std::set<FunctionDecl *> &TaskCallers)
      : Context(Context), P(P), Tasks(Tasks), TaskCallers(TaskCallers) {}

  bool VisitFunctionDecl(clang::FunctionDecl *Decl) {
    if (Tasks.find(Decl) == Tasks.end() &&
        TaskCallers.find(Decl) == TaskCallers.end()) {
      return true;
    }

    if (Decl->getBody()) {
      IRFunction *F =
          P.createFunc(Decl->getName().str(), Decl->getDeclaredReturnType());
      F->Info.RootFun = Decl;
      std::unordered_map<ASTVarRef, IRVarRef> VarLookup;
      for (auto *Param : Decl->parameters()) {
        Sym PSym = PutSym(Param->getName().str());
        F->Vars.push_back(IRVarDecl{
            .Type = Param->getType(),
            .Name = PSym,
            .DeclLoc = IRVarDecl::ARG,
        });
        VarLookup[Param] = &F->Vars.back();
      }
      Stmt2IRVisitor sv(F, VarLookup, Tasks);
      sv.Visit(Decl->getBody());

      FunLookup[Decl] = F;
    } else if (Tasks.find(Decl) != Tasks.end()) {
      PANIC("unsupported: forward declaration of task function");
    }
    return true;
  }
};

class FinalizeVisitor : public IRExprVisitor<FinalizeVisitor> {
  FunLookupTy &FunLookup;

public:
  FinalizeVisitor(FunLookupTy &FunLookup) : FunLookup(FunLookup) {}

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
    if (B->Term && isa<ReturnIRStmt>(B->Term)) {
      B->Succs.clear();
      //  B->Succs.insert(F->Exit);
    }
  }
}

void OpenCilk2IR(IRProgram &P, clang::ASTContext *Context, SourceManager &SM) {
  CilkAnalyzeVisitor AVisitor;
  Cilk2IRVisitor Visitor(Context, P, AVisitor.Tasks, AVisitor.TaskCallers);
  // Only visit declarations declared in the input TU
  auto Decls = Context->getTranslationUnitDecl()->decls();
  for (auto &Decl : Decls) {
    if (!SM.isInMainFile(Decl->getLocation()))
      continue;
    AVisitor.TraverseDecl(Decl);
  }
  DBG {
    llvm::outs() << "tasks\n";
    for (auto *Task : AVisitor.Tasks) {
      llvm::outs() << Task->getName() << "\n";
    }
    llvm::outs() << "taskcallers\n";
    for (auto *Task : AVisitor.TaskCallers) {
      llvm::outs() << Task->getName() << "\n";
    }
  }

  for (auto &Decl : Decls) {
    // Ignore declarations out of the main translation unit.
    //
    // SourceManager::isInMainFile method takes into account locations
    // expansion like macro expansion scenario and checks expansion
    // location instead if spelling location if required.
    if (!SM.isInMainFile(Decl->getLocation()))
      continue;
    AVisitor.TraverseDecl(Decl);
    Visitor.TraverseDecl(Decl);
  }

  for (auto &F : P) {
    finalizeFunction(F.get(), Visitor.FunLookup);
  }
}