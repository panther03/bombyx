#pragma once

#include <clang/AST/Stmt.h>
#include <llvm/ADT/SetVector.h>

#include <deque>
#include <memory>
#include <set>
#include <vector>

#include "clang/AST/Decl.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/Support/Casting.h>

#include "util.hpp"

using namespace clang;

typedef const clang::NamedDecl *ASTVarRef;

class IRStmt;
class IRBasicBlock;
class IRFunction;
class IRProgram;
class IRExpr;

enum ScopeAnnot { SA_OPEN, SA_CLOSE, SA_DO, SA_ELSE, SA_DAE_HERE };

typedef const clang::QualType IRType;
typedef int Sym;

struct SymTable {
  std::vector<std::string> Table;
  std::unordered_map<std::string, size_t> DupCnt;
};

extern SymTable GSymTable;
extern const std::string& GetSym(Sym S);
extern Sym PutSym(std::string Name);

struct IRVarDecl {
  IRType Type;
  Sym Name;
  enum { LOCAL, ARG } DeclLoc;
  bool IsEphemeral = false; 
};

typedef std::variant<ASTVarRef, IRFunction *> IRFunRef;
typedef IRVarDecl *IRVarRef;

static void identPrintSimple(llvm::raw_ostream &Out, IRVarRef VR) {
  Out << GetSym(VR->Name);
}

struct IRPrintContext {
  clang::ASTContext &ASTCtx;
  const char *NewlineSymbol;
  bool GraphVizEscapeChars = false;
  std::function<void(llvm::raw_ostream&, IRVarRef)> IdentCB = &identPrintSimple;
};

class IRExpr {
public:
  enum IRExprKind {
    EXK_BINOP,
    EXK_UNOP,
    EXK_REF,
    EXK_LITERAL,
    EXK_FIDENT,
    EXK_ISPAWN,
    EXK_LVAL_FIRST,
    EXK_LVAL_IDENT,
    EXK_LVAL_ACCESS,
    EXK_LVAL_INDEX,
    EXK_LVAL_DREF,
    EXK_LVAL_LAST,
    EXK_CAST,
    EXK_CALL,
    EXK_SYM_VAR
  };

private:
  const IRExprKind Kind;

public:
  bool Silent = false;

  IRExprKind getKind() const { return Kind; }

  IRExpr(IRExprKind K) : Kind(K) {}

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) {
    assert(false && "IRExpr::print not implemented");
  }

  virtual IRExpr* clone() {
    assert(false && "IRExpr::clone not implemented");
  }

  virtual ~IRExpr() = default;
};

struct IRLvalExpr : public IRExpr {
public:
  IRLvalExpr(IRExprKind K) : IRExpr(K) {}

  static bool classof(const IRExpr *E) {
    return E->getKind() >= IRExpr::EXK_LVAL_FIRST &&
           E->getKind() <= IRExpr::EXK_LVAL_LAST;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) {
    assert(false && "IRLvalExpr::print should not be called");
  }
};

struct IndexIRExpr : IRLvalExpr {
  IRVarRef Arr;
  std::unique_ptr<IRExpr> Ind;

public:
  IndexIRExpr(IRVarRef Arr, IRExpr *Ind)
      : Arr(Arr), Ind(Ind), IRLvalExpr(EXK_LVAL_INDEX) {}

  static bool classof(const IRExpr *E) {
    return E->getKind() == EXK_LVAL_INDEX;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct DRefIRExpr : IRLvalExpr {
  std::unique_ptr<IRExpr> Expr;

public:
  DRefIRExpr(IRExpr *E) : Expr(E), IRLvalExpr(EXK_LVAL_DREF) {}

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_LVAL_DREF; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct IdentIRExpr : IRLvalExpr {
  IRVarRef Ident;
public:
  IdentIRExpr(IRVarRef Ident) : Ident(Ident), IRLvalExpr(EXK_LVAL_IDENT) {}

  static bool classof(const IRExpr *E) {
    return E->getKind() == EXK_LVAL_IDENT;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct SymVarIRExpr : IRExpr {
  int SymVar;
public:
  SymVarIRExpr(int SymVar) : SymVar(SymVar), IRExpr(EXK_SYM_VAR) {}

  static bool classof(const IRExpr *E) {
    return E->getKind() == EXK_SYM_VAR;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct AccessIRExpr : IRLvalExpr {
  IRVarRef Struct;
  std::string Field;
  bool Arrow;

public:
  AccessIRExpr(IRVarRef Struct, std::string Field)
      : Struct(Struct), Field(Field), IRLvalExpr(EXK_LVAL_ACCESS) {}

  static bool classof(const IRExpr *E) {
    return E->getKind() == EXK_LVAL_ACCESS;
  }
  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct RefIRExpr : IRExpr {
  std::unique_ptr<IRExpr> E;

public:
  RefIRExpr(IRExpr *E) : E(E), IRExpr(EXK_REF) {}

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_REF; }
  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct BinopIRExpr : IRExpr {
public:
  enum BinopOp {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,
    BINOP_LT,
    BINOP_GT,
    BINOP_LE,
    BINOP_GE,
    BINOP_SHL,
    BINOP_SHR,
    BINOP_AND,
    BINOP_OR,
    BINOP_XOR,
    BINOP_EQ,
    BINOP_NEQ,
    BINOP_LAND,
    BINOP_LOR
  };
  void printBinop(llvm::raw_ostream &Out) const {
    switch (Op) {
    case BINOP_ADD:
      Out << "+";
      break;
    case BINOP_SUB:
      Out << "-";
      break;
    case BINOP_MUL:
      Out << "*";
      break;
    case BINOP_DIV:
      Out << "/";
      break;
    case BINOP_MOD:
      Out << "%";
      break;
    case BINOP_AND:
      Out << "&";
      break;
    case BINOP_OR:
      Out << "|";
      break;
    case BINOP_XOR:
      Out << "^";
      break;
    case BINOP_EQ:
      Out << "==";
      break;
    case BINOP_NEQ:
      Out << "!=";
      break;
    case BINOP_LAND:
      Out << "&&";
      break;
    case BINOP_LOR:
      Out << "||";
      break;
    case BINOP_LT:
      Out << "<";
      break;
    case BINOP_GT:
      Out << ">";
      break;
    case BINOP_LE:
      Out << "<=";
      break;
    case BINOP_GE:
      Out << ">=";
      break;
    case BINOP_SHL:
      Out << "<<";
      break;
    case BINOP_SHR:
      Out << ">>";
      break;
    default:
      llvm::errs() << "Unknown binary operator\n";
      llvm_unreachable("Unknown binary operator");  
    }
  }

  const BinopOp Op;
  std::unique_ptr<IRExpr> Left;
  std::unique_ptr<IRExpr> Right;

public:
  BinopIRExpr(BinopOp Op, IRExpr *Left, IRExpr *Right)
      : Op(Op), Left(Left), Right(Right), IRExpr(EXK_BINOP) {}

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_BINOP; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct UnopIRExpr : IRExpr {
public:
  enum UnopOp {
    UNOP_NEG,
    UNOP_L_NOT,
    UNOP_NOT,
    UNOP_PREINC,
    UNOP_POSTINC,
    UNOP_PREDEC,
    UNOP_POSTDEC
  };

  const UnopOp Op;
  std::unique_ptr<IRExpr> Expr;
  bool printUnop(const char * &Out) const {
    switch (Op) {
    case UNOP_NEG:
      Out = "-";
      return false;
    case UNOP_L_NOT:
      Out = "!";
      return false;
    case UNOP_NOT:
      Out = "~";
      return false;
    case UNOP_PREDEC:
      Out = "--";
      return false;
    case UNOP_POSTDEC:
      Out = "--";
      return true;
    case UNOP_PREINC: 
      Out = "++";
      return false;
    case UNOP_POSTINC:
      Out = "++";
      return true;
    }
    return false;
  }

public:
  UnopIRExpr(UnopOp Op, IRExpr *Expr) : Op(Op), Expr(Expr), IRExpr(EXK_UNOP) {}

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_UNOP; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct LiteralIRExpr : IRExpr {
private:
  clang::Expr *Lit;

public:
  LiteralIRExpr(clang::Expr *Lit) : Lit(Lit), IRExpr(EXK_LITERAL) {}

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_LITERAL; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct CastIRExpr : IRExpr {
private: 
  IRType CastType;
public:
  std::unique_ptr<IRExpr> E;
  CastIRExpr(IRType CastType, IRExpr *E) : CastType(CastType), E(E), IRExpr(EXK_CAST) {}

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_CAST; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct FIdentIRExpr : IRExpr {
public:
  IRFunRef FR;
  FIdentIRExpr(IRFunRef FR) : FR(FR), IRExpr(EXK_FIDENT) {}

  static bool classof(const IRExpr *FR) { return FR->getKind() == EXK_FIDENT; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct ISpawnIRExpr : IRExpr {
  IRFunRef Fn;
  std::vector<std::unique_ptr<IRExpr>> Args;

public:
  ISpawnIRExpr(IRFunRef Fn, std::vector<IRExpr*> InArgs)
      : Fn(Fn), IRExpr(EXK_ISPAWN) {
        for (auto *Arg : InArgs) {
          Args.push_back(std::unique_ptr<IRExpr>(Arg));
        }
      }

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_ISPAWN; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

struct CallIRExpr : IRExpr {
public:
  IRFunRef Fn;
  std::vector<std::unique_ptr<IRExpr>> Args;
  CallIRExpr(IRFunRef Fn, std::vector<IRExpr*> InArgs)
      : Fn(Fn), IRExpr(EXK_CALL) {
        for (auto *Arg : InArgs) {
          Args.push_back(std::unique_ptr<IRExpr>(Arg));
        }
      }

  static bool classof(const IRExpr *E) { return E->getKind() == EXK_CALL; }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRExpr* clone() override;
};

class IRStmt {
public:
  enum IRStmtKind {
    STK_TERMINATOR_START,
    STK_LOOP,
    STK_IF,
    STK_SPAWN_NEXT,
    STK_CLOSURE_DECL,
    STK_RETURN,
    STK_SYNC,
    STK_TERMINATOR_END,
    STK_ESPAWN,
    STK_WRAP,
    STK_STORE,
    STK_COPY,
    STK_SCOPE_ANNOT,

//    ForInc,
//    ForInit,
//    VoidSpawn
  };

private:
  const IRStmtKind Kind;

public:
  bool Silent = false;

  IRStmtKind getKind() const { return Kind; }

  void setSilent() { Silent = true; }

  IRStmt(IRStmtKind K) : Kind(K) {}

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) {
    assert(false && "IRStmt::print not implemented");
  }

  virtual IRStmt* clone() {
    assert(false && "IRStmt::clone not implemented");
  }

  virtual ~IRStmt() = default;
};

struct ClosureDeclIRStmt : IRStmt {
  IRFunction *Fn;
  std::unordered_map<IRVarRef, IRVarRef> Caller2Callee;
public:
  ClosureDeclIRStmt(IRFunction *Fn) : Fn(Fn), IRStmt(STK_CLOSURE_DECL) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_CLOSURE_DECL;
  }

  void addCallerToCaleeVarMapping(IRVarRef VR1, IRVarRef VR2) {
    Caller2Callee[VR1] = VR2;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct IRTerminatorStmt : public IRStmt {
public:
  IRTerminatorStmt(IRStmtKind K) : IRStmt(K) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() >= STK_TERMINATOR_START &&
           S->getKind() <= STK_TERMINATOR_END;
  }
};

struct LoopIRStmt : IRTerminatorStmt {
  std::unique_ptr<IRExpr> Cond;
  IRStmt *Inc;
  IRStmt *Init;

public:
  LoopIRStmt(IRExpr *Cond) : Cond(Cond), IRTerminatorStmt(STK_LOOP) {}
  LoopIRStmt(IRExpr *Cond, IRStmt *Inc, IRStmt *Init)
      : Cond(Cond), Inc(Inc), Init(Init), IRTerminatorStmt(STK_LOOP) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_LOOP;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct IfIRStmt : IRTerminatorStmt {
  std::unique_ptr<IRExpr> Cond;

public:
  IfIRStmt(IRExpr *Cond) : Cond(Cond), IRTerminatorStmt(STK_IF) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_IF;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct SpawnNextIRStmt : IRTerminatorStmt {
  IRFunction *Fn;
  ClosureDeclIRStmt *Decl;

public:
  SpawnNextIRStmt(IRFunction *Fn) : Fn(Fn), IRTerminatorStmt(STK_SPAWN_NEXT) {}

  void setDecl(ClosureDeclIRStmt *InDecl) {
    Decl = InDecl;
  }

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_SPAWN_NEXT;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct SyncIRStmt : IRTerminatorStmt {
public:
  SyncIRStmt() : IRTerminatorStmt(STK_SYNC) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_SYNC;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};
  
struct ReturnIRStmt : IRTerminatorStmt {
  std::unique_ptr<IRExpr> RetVal;

public:
  ReturnIRStmt(IRExpr *RetVal) : RetVal(RetVal), IRTerminatorStmt(STK_RETURN) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_RETURN;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct ESpawnIRStmt : IRStmt {
public:
  std::unique_ptr<IRLvalExpr> Dest;
  IRFunction *Fn;
  SpawnNextIRStmt* SN;
  std::vector<std::unique_ptr<IRExpr>> Args;
  bool Local;

public:
  ESpawnIRStmt(IRLvalExpr *Dest, IRFunction *Fn, SpawnNextIRStmt* SN,
               std::vector<IRExpr*> InArgs, bool Local)
      : Dest(Dest), Fn(Fn), SN(SN), IRStmt(STK_ESPAWN), Local(Local) {
        for (auto *Arg : InArgs) {
          Args.push_back(std::unique_ptr<IRExpr>(Arg));
        }
      }

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_ESPAWN;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct ExprWrapIRStmt : IRStmt {
  std::unique_ptr<IRExpr> Expr;

public:
  ExprWrapIRStmt(IRExpr *Expr) : Expr(Expr), IRStmt(STK_WRAP) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_WRAP;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct StoreIRStmt : IRStmt {
  std::unique_ptr<IRLvalExpr> Dest;
  std::unique_ptr<IRExpr> Src;

public:
  StoreIRStmt(IRLvalExpr *Dest, IRExpr *Src)
      : Dest(Dest), Src(Src), IRStmt(STK_STORE) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_STORE;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct CopyIRStmt : IRStmt {
  std::unique_ptr<IRExpr> Src;
  
  public:
  IRVarRef Dest;
  CopyIRStmt(IRVarRef Dest, IRExpr *Src)
      : Dest(Dest), Src(Src), IRStmt(STK_COPY) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_COPY;
  }

  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};

struct ScopeAnnotIRStmt : IRStmt {

public:
  ScopeAnnot SA;
  ScopeAnnotIRStmt(ScopeAnnot SA) : SA(SA), IRStmt(STK_SCOPE_ANNOT) {}

  static bool classof(const IRStmt *S) {
    return S->getKind() == IRStmt::STK_SCOPE_ANNOT;
  }
  virtual void print(llvm::raw_ostream &Out, IRPrintContext &Ctx) override;
  virtual IRStmt* clone() override;
};


// TODO not really the right construct
template <typename Derived>
class IRExprVisitor {
  private:
    int Depth = -1;
  public:
    void Visit(IRExpr *E) {
      Depth++;
      Derived* DerivedThis = static_cast<Derived*>(this);
      switch (E->getKind()) {
        case IRExpr::EXK_BINOP: DerivedThis->VisitBinop(llvm::dyn_cast<BinopIRExpr>(E)); return;
        case IRExpr::EXK_UNOP: DerivedThis->VisitUnop(llvm::dyn_cast<UnopIRExpr>(E)); return;
        case IRExpr::EXK_CALL: DerivedThis->VisitCall(llvm::dyn_cast<CallIRExpr>(E)); return;
        case IRExpr::EXK_ISPAWN: DerivedThis->VisitISpawn(llvm::dyn_cast<ISpawnIRExpr>(E)); return;
        case IRExpr::EXK_FIDENT: DerivedThis->VisitFIdent(llvm::dyn_cast<FIdentIRExpr>(E)); return;
        case IRExpr::EXK_REF: DerivedThis->VisitRef(llvm::dyn_cast<RefIRExpr>(E)); return;
        case IRExpr::EXK_LITERAL: DerivedThis->VisitLiteral(llvm::dyn_cast<LiteralIRExpr>(E)); return;
        case IRExpr::EXK_LVAL_IDENT: DerivedThis->VisitIdent(llvm::dyn_cast<IdentIRExpr>(E)); return;
        case IRExpr::EXK_LVAL_ACCESS: DerivedThis->VisitAccess(llvm::dyn_cast<AccessIRExpr>(E)); return;
        case IRExpr::EXK_LVAL_DREF: DerivedThis->VisitDRef(llvm::dyn_cast<DRefIRExpr>(E)); return;
        case IRExpr::EXK_LVAL_INDEX: DerivedThis->VisitIndex(llvm::dyn_cast<IndexIRExpr>(E)); return;    
        case IRExpr::EXK_CAST: DerivedThis->VisitCast(llvm::dyn_cast<CastIRExpr>(E)); return;    
        default: PANIC("impossible expr");
      }
    }
    void VisitBinop(BinopIRExpr *Node) {
      Visit(Node->Left.get());
      Visit(Node->Right.get());
    }
    void VisitUnop(UnopIRExpr *Node) {
      Visit(Node->Expr.get());
    }
    void VisitRef(RefIRExpr *Node) {
      Visit(Node->E.get());
    }
    void VisitLiteral(LiteralIRExpr *Node) {}
    void VisitFIdent(FIdentIRExpr *Node) {}
    void VisitISpawn(ISpawnIRExpr *Node) {
      for (auto &Arg : Node->Args) {
        Visit(Arg.get());
      }
    }
    void VisitCall(CallIRExpr *Node) {
      for (auto &Arg : Node->Args) {
        Visit(Arg.get());
      }
    }
    void VisitIdent(IdentIRExpr *Node) {}
    void VisitAccess(AccessIRExpr *Node) {}
    void VisitIndex(IndexIRExpr *Node) {
      Visit(Node->Ind.get());
    }
    void VisitCast(CastIRExpr *Node) {
      Visit(Node->E.get());
    }
    void VisitDRef(DRefIRExpr *Node)  {
      Visit(Node->Expr.get());
    }
    void VisitStmt(IRStmt *S) {
      switch (S->getKind()) {
        case IRStmt::STK_COPY: {
          CopyIRStmt *CS = llvm::dyn_cast<CopyIRStmt>(S);
          Visit(CS->Src.get());
          return;
        }
        case IRStmt::STK_ESPAWN: {
          ESpawnIRStmt *ES = llvm::dyn_cast<ESpawnIRStmt>(S);
          Visit(ES->Dest.get());
          for (auto &Arg: ES->Args) {
            Visit(Arg.get());
          }
          return;
        }
        case IRStmt::STK_STORE: {
          StoreIRStmt *SS = llvm::dyn_cast<StoreIRStmt>(S);
          Visit(SS->Dest.get());
          Visit(SS->Src.get());
          return;
        }
        case IRStmt::STK_WRAP: {
          ExprWrapIRStmt *EWS = llvm::dyn_cast<ExprWrapIRStmt>(S);
          Visit(EWS->Expr.get());
          return;
        }
        case IRStmt::STK_IF: {
          IfIRStmt *IS = llvm::dyn_cast<IfIRStmt>(S);
          Visit(IS->Cond.get());
          return;
        }
        case IRStmt::STK_LOOP: {
          LoopIRStmt *LS = llvm::dyn_cast<LoopIRStmt>(S);
          Visit(LS->Cond.get());
          return;
        }
        case IRStmt::STK_RETURN: {
          ReturnIRStmt *RS = llvm::dyn_cast<ReturnIRStmt>(S);
          if (RS->RetVal) {
            Visit(RS->RetVal.get());
          }
          return;
        }
        case IRStmt::STK_SCOPE_ANNOT: {
          ScopeAnnotIRStmt *SAS = llvm::dyn_cast<ScopeAnnotIRStmt>(S);
          return;
        }
        case IRStmt::STK_SYNC:
        case IRStmt::STK_SPAWN_NEXT: {
          return;
        }
        default: PANIC("impossible stmt");
      }
    }
  };

class IRBasicBlock {
private:
  using IRStmtPtr = std::unique_ptr<IRStmt>;
  std::deque<IRStmtPtr> Stmts;
  IRFunction *Parent;
  unsigned Ind;
  
  public:
  llvm::SetVector<IRBasicBlock *> Succs;
  IRTerminatorStmt* Term;

  friend class IRFunction;

  IRBasicBlock(unsigned Ind, IRFunction *Parent) : Ind(Ind), Parent(Parent), Term(nullptr) {}
  void iteratePreds(std::function<void(IRBasicBlock *B)> CB);

  void pushStmtBack(IRStmt *stmt) { Stmts.push_back(IRStmtPtr(stmt)); }
  void pushStmtFront(IRStmt *stmt) { Stmts.push_front(IRStmtPtr(stmt)); }
  // Clones contents of basic block. Does not clone successors.
  void clone(IRBasicBlock *Dest);
  IRBasicBlock* splitAt(int Index);
  IRStmt* getAt(int Index);
  void removeAt(int Index);

  // void graphPrintStmt(llvm::raw_ostream &Out, clang::ASTContext &,
  //                     const Stmt *S, const char *NewlineSymbol);

  void print(llvm::raw_ostream &Out, IRPrintContext &Ctx);

  void dumpGraph(llvm::raw_ostream &Out, clang::ASTContext &Context);

  void moveBlock(IRFunction *NewParent);

  using IRBlockListTy = std::deque<IRStmtPtr>;
  using iterator = IRBlockListTy::iterator;
  using const_iterator = IRBlockListTy::const_iterator;

  IRStmtPtr &front() { return Stmts.front(); }
  IRStmtPtr &back() { return Stmts.back(); }

  iterator begin() { return Stmts.begin(); }
  iterator end() { return Stmts.end(); }
  const_iterator begin() const { return Stmts.begin(); }
  const_iterator end() const { return Stmts.end(); }

  IRFunction *getParent() const { return Parent; }
  unsigned getInd() const { return Ind; }

  ~IRBasicBlock() {
    if (Term) {
      delete Term;
    }
  }
};

class IRFunction {
public:
  struct IRFunctionInfo {
    bool IsTask = false;
    const FunctionDecl *RootFun = nullptr;
    std::set<IRFunction*> SendArgList;
    std::set<IRFunction*> SpawnList;
    std::set<IRFunction*> SpawnNextList;
  };
private:
  using IRBlockPtr = std::unique_ptr<IRBasicBlock>;
  IRProgram *Parent;
  unsigned Ind;

  IRType Ret;
  std::string Name;
  std::list<IRBlockPtr> Blocks;

public:
  std::list<IRVarDecl> Vars;
  IRFunctionInfo Info;

  friend class IRBasicBlock;
  friend class IRProgram;

  IRFunction(unsigned Ind, const std::string &Name, IRProgram *Parent) : Parent(Parent), Ind(Ind), Name(Name) {}
  IRBasicBlock *createBlock();

  void printVars(llvm::raw_ostream &out) {
    for (auto &V : Vars) {
      if (V.DeclLoc == IRVarDecl::ARG) {
        out << "[A]:";
      }
      out << GetSym(V.Name) << " ";
    }
    out << "\n";
  }

  void print(llvm::raw_ostream &out, clang::ASTContext &Context);
  void dumpGraph(llvm::raw_ostream &out, clang::ASTContext &Context);
  // void dumpArgs(llvm::raw_ostream &out);
  void moveBlock(IRBasicBlock *B, IRFunction *Dest);
  void cleanVars();

  using IRBlockListTy = std::list<IRBlockPtr>;
  using iterator = IRBlockListTy::iterator;
  using const_iterator = IRBlockListTy::const_iterator;

  bool empty() { return Blocks.empty(); }
  IRBlockPtr &front() { return Blocks.front(); }
  IRBlockPtr &back() { return Blocks.back(); }
  // Front of the function should always be the entry block.
  IRBasicBlock *getEntry() { return Blocks.front().get(); }

  iterator begin() { return Blocks.begin(); }
  iterator end() { return Blocks.end(); }
  const_iterator begin() const { return Blocks.begin(); }
  const_iterator end() const { return Blocks.end(); }

  IRProgram *getParent() const { return Parent; }
  unsigned getInd() const { return Ind; }
  const std::string &getName() const { return Name; }
};

class IRProgram {
private:
  using IRFuncPtr = std::unique_ptr<IRFunction>;
  std::vector<IRFuncPtr> Funcs;

public:
  std::unordered_map<const Stmt *, IRBasicBlock *> Ast2IrDestination;
  std::unordered_map<std::string, IRFunction *> RootFunLookup;

  IRProgram() {}
  IRFunction *createFunc(const std::string &Name);

  void print(llvm::raw_ostream &out, clang::ASTContext &Context);
  void dumpGraph(llvm::raw_ostream &out, clang::ASTContext &Context);

  using IRFuncListTy = std::vector<IRFuncPtr>;
  using iterator = IRFuncListTy::iterator;
  using const_iterator = IRFuncListTy::const_iterator;

  IRFuncPtr &front() { return Funcs.front(); }
  IRFuncPtr &back() { return Funcs.back(); }

  iterator begin() { return Funcs.begin(); }
  iterator end() { return Funcs.end(); }
  const_iterator begin() const { return Funcs.begin(); }
  const_iterator end() const { return Funcs.end(); }
};

IRBasicBlock* FindJoin(IRBasicBlock* Left, IRBasicBlock *Right);
class ScopedIRTraverser {
protected:
  enum ScopeEvent { None, Open, Close, Else };

private:
  struct WorkItem {
    IRBasicBlock *B;
    ScopeEvent SE;

    WorkItem(ScopeEvent SE0) {
      B = nullptr;
      SE = SE0;
    }
    WorkItem(IRBasicBlock *B0) {
      B = B0;
      SE = None;
    }
  };

  std::vector<WorkItem> WorkList;
  std::unordered_map<IRBasicBlock *, int> JoinCounts;
  virtual void handleScope(ScopeEvent SE) {}
  virtual void visitBlock(IRBasicBlock *B) {}

public:
  void traverse(IRFunction &F);
};

/*
class ScopedIRPrinter : public ScopedIRTraverser {
private:
  int indent = 0;
  clang::ASTContext *C;

  void printIndentation() {
    for (int i = 0; i < indent; i++) {
      llvm::outs() << "\t";
    }
  }

  void handleScope(ScopeEvent SE) override {
    if (SE == Close || SE == Else) {
      assert(indent > 0);
      indent--;
      printIndentation();
      llvm::outs() << "}\n";
    }
    if (SE == Else) {
      printIndentation();
      llvm::outs() << "else\n";
    }
    if (SE == Open || SE == Else) {
      printIndentation();
      llvm::outs() << "{\n";
      indent++;
    }
  }

  void visitBlock(IRBasicBlock *B) override {
    B->print(llvm::outs(), *C, "\n");
  }

public:
  ScopedIRPrinter(clang::ASTContext *C) : C(C) {}
};*/

class ExprIdentifierVisitor : public IRExprVisitor<ExprIdentifierVisitor> {
public:
  using IdCallbackType = std::function<void(IRVarRef&, bool)>;
private:
  IdCallbackType CB;
public:
  
  ExprIdentifierVisitor(IRStmt *S, IdCallbackType CB) : CB(CB) {
    VisitStmt(S);
  }

  ExprIdentifierVisitor(IRExpr *E, IdCallbackType CB) : CB(CB) {
    Visit(E);
  }

  void VisitStmt(IRStmt *S) {
    if (auto CS = dyn_cast<CopyIRStmt>(S)) {
      CB(CS->Dest, true);
    }
    IRExprVisitor<ExprIdentifierVisitor>::VisitStmt(S);
  }

  void VisitIdent(IdentIRExpr *Node) {
    CB(Node->Ident, false);
  }

  void VisitAccess(AccessIRExpr *Node) {
    CB(Node->Struct, false);
  }

  void VisitIndex(IndexIRExpr *Node) {
    CB(Node->Arr, false);
    Visit(Node->Ind.get());
  }
};
/*
struct ExprIdentifierIterator {
private:
  std::vector<const IRExpr *> WorkList;
  std::deque<const DeclRefExpr *> ReturnQueue;
public:
  ExprIdentifierIterator(const clang::Stmt *Stmt) {
    if (const IRExpr *E = dyn_cast<IRExpr>(Stmt)) {
      WorkList.push_back(E);
      ++(*this);
    }
  }

  const DeclRefExpr *operator*() const { return ReturnQueue.back(); }

  ExprIdentifierIterator &operator++() {
    if (!ReturnQueue.empty()) {
      ReturnQueue.pop_back();
    }
    while (ReturnQueue.empty() && !WorkList.empty()) {
      const Expr *E = WorkList.back();
      WorkList.pop_back();
      for (const auto &C : E->children()) {
        if (const auto *ID = dyn_cast<DeclRefExpr>(C)) {
          ReturnQueue.push_front(ID);
        } else if (const Expr *CE = dyn_cast<Expr>(C)) {
          WorkList.push_back(CE);
        }
      }
    }
    return *this;
  }

  bool done() { return ReturnQueue.empty(); }
};*/