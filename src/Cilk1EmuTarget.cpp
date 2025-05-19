#include <clang/AST/ASTContext.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Type.h>
#include <clang/Basic/SourceManager.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <llvm/Support/raw_ostream.h>

#include "Cilk1EmuTarget.hpp"
#include "IR.hpp"
#include "util.hpp"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCilk.h"
#include "clang/AST/Stmt.h"

#define TAB "    "

////////////////////////////////////////
// 1. Forward declarations, closures //
//////////////////////////////////////

void printFunDecl(IRFunction *F, llvm::raw_ostream &out, clang::ASTContext &C) {
  if (F->Info.IsTask) {
    out << "THREAD(" << F->getName() << ")";
  } else if (F->Info.RootFun) {
    out << F->Info.RootFun->getReturnType().getAsString();
    out << " " << F->Info.RootFun->getName() << "(";
    bool first = true;
    for (auto &ArgDecl : F->Info.RootFun->parameters()) {
      if (!first) {
        out << ", ";
      } else {
        first = false;
      }
      ArgDecl->print(out, 0);
    }
    out << ")";
  } else {
    PANIC("Non-root fun should be a task!");  
  }
}

void printClosureDecl(IRFunction *F, llvm::raw_ostream &out,
                      clang::ASTContext &C) {
  out << "CLOSURE_DEF(" << F->getName() << ",\n";
  for (auto &Var : F->Vars) {
    if (Var.DeclLoc == IRVarDecl::ARG) {
      out << TAB;
      Var.Type.print(out, C.getPrintingPolicy());
      out << " " << GetSym(Var.Name) << ";\n";
    }
  }
  out << ");\n";
}

/////////////////////////////////////////////////////
// 2. Rewrite source to remove modified functions //
///////////////////////////////////////////////////

void printOriginalSource(IRProgram &P, llvm::raw_ostream &out,
                         clang::ASTContext &C, clang::CompilerInstance &CI) {
  Rewriter R;
  SourceManager &SM = CI.getSourceManager();
  R.setSourceMgr(SM, CI.getLangOpts());
  for (auto &F: P) {
    if (F->Info.RootFun) {
      R.RemoveText(F->Info.RootFun->getSourceRange());
    }
  }
  R.getEditBuffer(SM.getMainFileID()).write(out);
}

////////////////////////////
// 3. Print IR Functions //
//////////////////////////

void printLocals(IRFunction *F, clang::ASTContext &C, llvm::raw_ostream &Out) {
  for (auto &Local: F->Vars) {
    if (Local.DeclLoc == IRVarDecl::LOCAL) {
      Out << TAB;
      Local.Type.print(Out, C.getPrintingPolicy());
      Out << " " << GetSym(Local.Name) << ";\n";
    }
  }
}

/*
struct DeclInfo {
  int ReuseCnt;
  enum { Arg, Local } Type;
};

// https://stackoverflow.com/questions/32685540/why-cant-i-compile-an-unordered-map-with-a-pair-as-key
struct pair_hash {
  template <class T1, class T2>
  std::size_t operator () (const std::pair<T1,T2> &p) const {
      auto h1 = std::hash<T1>{}(p.first);
      auto h2 = std::hash<T2>{}(p.second);

      // Mainly for demonstration purposes, i.e. works but is overly simple
      // In the real world, use sth. like boost.hash_combine
      return h1 ^ h2;  
  }
};

typedef std::unordered_map<std::pair<IRFunction*, IRVarRef>, DeclInfo, pair_hash> DeclMap;
#define DM_ENT(F,VR) (std::make_pair(F,VR))

void DeclMapInit(DeclMap &DM, IRFunction *F) {
  std::unordered_map<std::string, int> ReuseCnts;
  for (auto Arg : F->Args) {
    DM[DM_ENT(F, Arg)] = DeclInfo{.Type = DeclInfo::Arg};
    ReuseCnts[Arg->getDeclName().getAsString()] = 0;
  }
  for (auto Arg : F->Materialized) {
    DM[DM_ENT(F, Arg)] = DeclInfo{.Type = DeclInfo::Arg};
    ReuseCnts[Arg->getDeclName().getAsString()] = 0;
  }
  for (auto Local : F->Locals) {
    std::string LocalName = Local->getNameAsString();
    if (ReuseCnts.find(LocalName) == ReuseCnts.end()) {
      ReuseCnts[LocalName] = 0;
    } else {
      ReuseCnts[LocalName] += 1;
    }
    DM[DM_ENT(F, Local)] =
        DeclInfo{.ReuseCnt = ReuseCnts[LocalName], .Type = DeclInfo::Local};
  }
}

bool DeclMapLookup(DeclMap &DM, IRFunction *F, IRVarRef VR, std::string &replaced) {
  if (DM.find(DM_ENT(F,VR)) == DM.end()) {
    return false;
  }
  auto &DI = DM[DM_ENT(F,VR)];

  switch (DI.Type) {
    case DeclInfo::Arg: {

      replaced = "largs->" + VR->getName().str();
      return true;
    }
    case DeclInfo::Local: {
      if (DI.ReuseCnt > 0) {
        replaced = VR->getName().str() + std::to_string(DI.ReuseCnt);
        return true;
      } else {
        return false;
      }
    }
  }
  return false;
}

void DeclMapPrint(DeclMap &DM) {
  for (auto const& [P, DI] : DM)  {
    auto const& [F, VR] = P;
    F->printName(llvm::outs());
    llvm::outs() << "," << VR->getName() << " -> ";
    if (DI.Type == DeclInfo::Arg) {
      llvm::outs() << "Arg\n";
    } else {
      llvm::outs() << "Local (RC " << DI.ReuseCnt << ")\n";
    }
  }
}



class StmtNameRemapper : public clang::RecursiveASTVisitor<StmtNameRemapper> {
private:
  DeclMap &DM;
  Rewriter &R;
public: 
  IRFunction *F;
  explicit StmtNameRemapper(DeclMap &DM, Rewriter &R): DM(DM), R(R) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr *Dre) {
    if (DM.find(DM_ENT(F, Dre->getDecl())) != DM.end()) {  
      std::string replaced;
      if (DeclMapLookup(DM, F, Dre->getDecl(), replaced)) {
        R.ReplaceText(Dre->getSourceRange(), replaced);
      }
    }
    return true;
  }
}; */

class Cilk1EmuPrinter : public ScopedIRTraverser {
private:
  llvm::raw_ostream &Out;
  IRPrintContext &C;
  int SpawnCtr = 0;
  int IndentLvl = 1;

  llvm::raw_ostream& Indent() {
    for (int i = 0; i < IndentLvl; i++) Out << TAB;
    return Out;
  }

  void handleScope(ScopeEvent SE) override {
    switch (SE) { 
      case ScopeEvent::Close: {
        assert(IndentLvl > 0);
        IndentLvl--;
        Indent() << "}\n";
        break;
      }
      case ScopeEvent::Open: {
        Out << " {\n";
        IndentLvl++;
        break;
      }
      case ScopeEvent::Else: {
        assert(IndentLvl > 0);
        IndentLvl--;
        Indent() << "} else {\n";
        IndentLvl++;
        break;
      }
      default: {}
    }
  }

  void handleSpawnNextDecl(ClosureDeclIRStmt *DS, IRFunction *F) {
    const std::string &SpawnNextFnName = DS->Fn->getName();
    Indent() << SpawnNextFnName << "_closure " << "SN_" << SpawnNextFnName << "c";
    if (F->Info.IsTask) {
      Out << "(largs->k);\n";
    } else {
      Out << "(CONT_DUMMY);\n";
    }
    Indent();
    Out << "spawn_next<" << SpawnNextFnName << "_closure> " << "SN_" << SpawnNextFnName << "(SN_" << SpawnNextFnName << "c);\n";
    
  }

  void handleSpawnNext(SpawnNextIRStmt *S, IRFunction *F) {
    const std::string &SpawnNextFnName = S->Fn->getName();
    for (auto &[SrcVar,DstVar] : S->Decl->Caller2Callee) {
      if (!SrcVar->IsEphemeral) {
        Indent() << "((" << SpawnNextFnName << "_closure*)SN_" << SpawnNextFnName;
        Out << ".cls.get())->" << GetSym(DstVar->Name) << " = ";
        S->Fn->printVar(Out, SrcVar);
        Out << ";\n";
      }
    }
    Indent() << "// Original sync was here\n";
  }

  void handleSpawn(ESpawnIRStmt *ES, IRFunction *F) {
    const std::string &SpawnFnName = ES->Fn->getName();
    const std::string &SpawnNextFnName = ES->SN->Fn->getName();
    Indent() << "cont sp" << SpawnCtr << "k;\n";
    // TODO make it work on non-local
    if (ES->Local) {
      auto *IdentDest = dyn_cast<IdentIRExpr>(ES->Dest.get());
      assert(IdentDest);
      Indent() << "SN_BIND(SN_" << SpawnNextFnName << ", &sp" << SpawnCtr << "k, " << GetSym(IdentDest->Ident->Name) << ");\n";
    } else {
      Indent() << "SN_BIND_EXT(SN_" << SpawnNextFnName << ", &sp" << SpawnCtr << "k, &(";
      ES->Dest->print(Out, C);
      Out << "));\n";
    }
    

    
    Indent() << SpawnFnName << "_closure sp" << SpawnCtr << "c(sp" << SpawnCtr << "k);\n";

    // we do not create spawn destination functions. 
    // we expect them to be in argument first order
    //assert(ES->Fn->Info.RootFun);
    auto DstArgIt = ES->Fn->Vars.begin();
    // TODO: this doesnt seem to be working with DAE
    for (auto &Arg: ES->Args) {
      auto &DstArg = *DstArgIt;
      assert(DstArg.DeclLoc == IRVarDecl::ARG);
      Indent() << "sp" << SpawnCtr << "c." << GetSym(DstArg.Name);
      Out << " = ";
      Arg->print(Out, C);
      Out << ";\n";
      DstArgIt++;
    }

    Indent() << "spawn<" << SpawnFnName << "_closure> sp"  << SpawnCtr << "(sp" << SpawnCtr << "c);\n\n";
  }

  void visitStmt(IRStmt *S, IRBasicBlock *B) {
    auto *F = B->getParent();
    if (S->Silent) return;

    if (auto *ES = dyn_cast<ESpawnIRStmt>(S)) {
      handleSpawn(ES, F);
      SpawnCtr++;
    } else if (auto *SNS = dyn_cast<SpawnNextIRStmt>(S)) {
      handleSpawnNext(SNS, F);
    } else if (auto *CDS = dyn_cast<ClosureDeclIRStmt>(S)) {
      handleSpawnNextDecl(CDS, F);
    } else if (auto *RS = dyn_cast<ReturnIRStmt>(S)) {
      Indent();
      if (F->Info.IsTask) {
        Out << "SEND_ARGUMENT(largs->k, ";
        RS->RetVal->print(Out, C);
        Out << ");\n"; 
      } else {
        RS->print(Out, C);
        Out << ";\n";
      }
    } else { 
      Indent();
      S->print(Out, C);
      if (!isa<IfIRStmt>(S) && !isa<LoopIRStmt>(S)) {
        Out << ";\n";
      }
    }
  }

  void visitBlock(IRBasicBlock *B) override {

    for (auto &S: *B) {
      visitStmt(S.get(), B);
    }
    if (B->Term) visitStmt(B->Term, B);
  }

public:
  Cilk1EmuPrinter(llvm::raw_ostream &Out, IRPrintContext &C) : Out(Out), C(C) {}
};

void PrintCilk1Emu(IRProgram &P, llvm::raw_ostream &out, clang::ASTContext &C,
                   clang::CompilerInstance &CI) {
  // 1. Print forward declarations of each function, include Cilk1 emulation
  // file.
  out << "#include \"cilk_explicit.hh\"\n";
  for (auto &F : P) {
    printFunDecl(F.get(), out, C);
    out << ";\n";
  }
  out << "\n";
  for (auto &F : P) {
    if (F->Info.IsTask) {
      printClosureDecl(F.get(), out, C);
    }
  }
  // 2. Print the original source file with the original root functions removed.
  printOriginalSource(P, out, C, CI);

  // 3. Print the implementation of each function.  
  for (auto &F: P) {
    
    printFunDecl(F.get(), out, C);
    out << " {\n";

    printLocals(F.get(), C, out);

    if (F->Info.IsTask) {
      out << TAB << F->getName() << "_closure *largs = (" << F->getName() << "_closure*)(args.get());\n";
    }

    
    auto IRC = IRPrintContext {
      .ASTCtx = C,
      .NewlineSymbol = "\n",
      .GraphVizEscapeChars = false
    };
    Cilk1EmuPrinter Printer(out, IRC);
    Printer.traverse(*F);
    if (F->Info.IsTask) {
      out << "    return;\n";
    }
    // great hack
    if (F->getName() == "main") {
      out << "    return 0;\n";
    }
    out << "}\n";
  }
}