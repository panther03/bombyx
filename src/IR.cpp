#include "IR.hpp"
#include "util.hpp"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <clang/AST/ASTContext.h>
#include <clang/AST/StmtCilk.h>
#include <memory>
#include <regex>

SymTable GSymTable;

const std::string& GetSym(Sym S) {
  return GSymTable.Table[S];
}

Sym PutSym(std::string Name) {
  int NameCnt = -1;
  if (GSymTable.DupCnt.find(Name) != GSymTable.DupCnt.end()) {
    NameCnt = GSymTable.DupCnt[Name];
  } 
  GSymTable.DupCnt[Name] = NameCnt+1;
  if (NameCnt >= 0) {
    Name += std::to_string(NameCnt);
  }
  GSymTable.Table.push_back(Name);
  return GSymTable.Table.size() - 1;
}

/////////////
// IRExpr //
///////////

void IndexIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx) {
  assert(Ind);
  Ctx.IdentCB(Out, Arr);
  Out << "[";
  Ind->print(Out, Ctx);
  Out << "]";
}

IRExpr* IndexIRExpr::clone() {
  assert(Ind);
  IRExpr *NewInd = Ind->clone();
  return new IndexIRExpr(Arr, NewInd);
}

void RefIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(E);
  Out << "&(";
  E->print(Out, Ctx);
  Out << ")";
}

IRExpr* RefIRExpr::clone() {
  assert(E);
  IRExpr *NewE = E->clone();
  return new RefIRExpr(NewE);
}

void DRefIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Expr);
  Out << "*(";
  Expr->print(Out, Ctx);
  Out << ")";
}

IRExpr* DRefIRExpr::clone() {
  assert(Expr);
  IRExpr *NewE = Expr->clone();
  return new DRefIRExpr(NewE);
}

void AccessIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Struct);
  Ctx.IdentCB(Out, Struct);
  if (Arrow) {
    Out << "->";
  } else {
    Out << ".";
  }
  Out << Field;
}

IRExpr* AccessIRExpr::clone() {
  assert(Struct);
  return new AccessIRExpr(Struct, Field);
}

void IdentIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Ident);
  Ctx.IdentCB(Out, Ident);
}

IRExpr* IdentIRExpr::clone() {
  assert(Ident);
  return new IdentIRExpr(Ident);
}

void SymVarIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  Out << "sv" << SymVar;
}

IRExpr* SymVarIRExpr::clone() {
  return new SymVarIRExpr(SymVar);
}

void FIdentIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  if (auto *F = std::get_if<IRFunction *>(&FR)) {
    Out << (*F)->getName();
  } else {
    Out << std::get<ASTVarRef>(FR)->getName();
  }
}

IRExpr* FIdentIRExpr::clone() {
  if (auto *F = std::get_if<IRFunction *>(&FR)) {
    return new FIdentIRExpr(*F);
  } else {
    return new FIdentIRExpr(std::get<ASTVarRef>(FR));
  }
}

void LiteralIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Lit);
  Lit->printPretty(Out, nullptr, Ctx.ASTCtx.getPrintingPolicy());
}

IRExpr* LiteralIRExpr::clone() {
  assert(Lit);
  return new LiteralIRExpr(Lit);
}

void BinopIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Left && Right);
  Out << "(";
  Left->print(Out, Ctx);
  Out << " ";
  printBinop(Out);
  Out << " ";
  Right->print(Out, Ctx);
  Out << ")";
}

IRExpr* BinopIRExpr::clone() {
  assert(Left && Right);
  IRExpr *NewLeft = Left->clone();
  IRExpr *NewRight = Right->clone();
  return new BinopIRExpr(Op, NewLeft, NewRight);
}

void UnopIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Expr);
  Out << "(";
  const char *Op;
  if (printUnop(Op)) {
    Expr->print(Out, Ctx);
    Out << Op;
  } else { 
    Out << Op;
    Expr->print(Out, Ctx);
  }
  Out << ")";
}

IRExpr* UnopIRExpr::clone() {
  assert(Expr);
  IRExpr *NewE = Expr->clone();
  return new UnopIRExpr(Op, NewE);
}

void ISpawnIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  Out << "spawn ";
  if (auto *F = std::get_if<IRFunction *>(&Fn)) {
    Out << (*F)->getName();
  } else {
    Out << std::get<ASTVarRef>(Fn)->getName();
  }
  Out << "(";
  bool first = true;
  for (auto &Arg : Args) {
    if (first) {
      first = false;
    } else {
      Out << ",";
    }
    Arg->print(Out, Ctx);
  }
  Out << ")";
}

IRExpr* ISpawnIRExpr::clone() {
  std::vector<IRExpr *> NewArgs;
  for (auto &Arg : Args) {
    NewArgs.push_back(Arg->clone());
  }
  if (auto *F = std::get_if<IRFunction *>(&Fn)) {
    return new ISpawnIRExpr(*F, NewArgs);
  } else {
    return new ISpawnIRExpr(std::get<ASTVarRef>(Fn), NewArgs);
  }
}

void CallIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  if (auto *F = std::get_if<IRFunction *>(&Fn)) {
    Out << (*F)->getName();
  } else {
    Out << std::get<ASTVarRef>(Fn)->getName();
  }
  Out << "(";
  bool first = true;
  for (auto &Arg : Args) {
    if (first) {
      first = false;
    } else {
      Out << ",";
    }
    Arg->print(Out, Ctx);
  }
  Out << ")";
}

IRExpr* CallIRExpr::clone() {
  std::vector<IRExpr *> NewArgs;
  for (auto &Arg : Args) {
    NewArgs.push_back(Arg->clone());
  }
  if (auto *F = std::get_if<IRFunction *>(&Fn)) {
    return new CallIRExpr(*F, NewArgs);
  } else {
    return new CallIRExpr(std::get<ASTVarRef>(Fn), NewArgs);
  }
}

void CastIRExpr::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  Out << "((";
  CastType.print(Out, Ctx.ASTCtx.getPrintingPolicy());
  Out << ") ";
  E->print(Out, Ctx);
  Out << ")";
}

IRExpr* CastIRExpr::clone() {
  return new CastIRExpr(CastType, E->clone());
}

/////////////
// IRStmt //
///////////
void LoopIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  if (Inc || Init) {
    Out << "for (";
  } else {
    Out << "while (";
  }

  if (Init) {
    Init->print(Out, Ctx);
  }
  Out << ";";
  Cond->print(Out, Ctx);
  Out << ";";
  if (Inc) {
    Inc->print(Out, Ctx);
  }
  Out << ")";
}

IRStmt* LoopIRStmt::clone() {
  assert(Cond);
  IRExpr *NewCond = Cond->clone();
  IRStmt *NewInc = Inc ? Inc->clone() : nullptr;
  IRStmt *NewInit = Init ? Init->clone() : nullptr;
  return new LoopIRStmt(NewCond, NewInc, NewInit);
}

void IfIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Cond);
  Out << "if (";
    Cond->print(Out, Ctx);
  Out << ")";
}

IRStmt* IfIRStmt::clone() {
  assert(Cond);
  IRExpr *NewCond = Cond->clone();
  return new IfIRStmt(NewCond);
}

void SpawnNextIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Fn);
  Out << "spawnNext ";
  Out << Fn->getName();
}

IRStmt* SpawnNextIRStmt::clone() {
  assert(Fn);
  return new SpawnNextIRStmt(Fn);
}

void ESpawnIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Dest);
  Out << "espawn @";
  Dest->print(Out, Ctx);
  Out << " ";
  Out << Fn->getName();
  Out << "(";
  bool first = true;
  for (auto &Arg : Args) {
    if (first) {
      first = false;
    } else {
      Out << ",";
    }
    Arg->print(Out, Ctx);
  }
  Out << ") [" << SN->Fn->getName() << "]";
}

IRStmt* ESpawnIRStmt::clone() {
  std::vector<IRExpr *> NewArgs;
  for (auto &Arg : Args) {
    NewArgs.push_back(Arg->clone());
  }
  return new ESpawnIRStmt(dyn_cast<IRLvalExpr>(Dest->clone()), Fn, SN, NewArgs, Local);
}

void ExprWrapIRStmt ::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Expr);
  Expr->print(Out, Ctx);
}

IRStmt* ExprWrapIRStmt::clone() {
  assert(Expr);
  return new ExprWrapIRStmt(Expr->clone());
}

void StoreIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Dest);
  Dest->print(Out, Ctx);
  Out << " = ";
  Src->print(Out, Ctx);
}

IRStmt* StoreIRStmt::clone() {
  assert(Dest && Src);
  IRLvalExpr *NewDest = dyn_cast<IRLvalExpr>(Dest->clone());
  IRExpr *NewSrc = Src->clone();
  return new StoreIRStmt(NewDest, NewSrc);
}

void CopyIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  assert(Dest);
  Ctx.IdentCB(Out, Dest);
  Out << " = ";
  Src->print(Out, Ctx);
}

IRStmt* CopyIRStmt::clone() {
  assert(Dest && Src);
  IRExpr *NewSrc = Src->clone();
  return new CopyIRStmt(Dest, NewSrc);
}

void SyncIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  Out << "sync";
}

IRStmt* SyncIRStmt::clone() {
  return new SyncIRStmt();
}

void ClosureDeclIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  Out << "cdef ";
  Out << Fn->getName();
  Out << "(";
  for (auto &[src,dst] : Caller2Callee) {
    Out << " " << GetSym(src->Name);
  }
  Out << " )";
}

IRStmt* ClosureDeclIRStmt::clone() {
  PANIC("unimplemented");
  return nullptr;
}


void ReturnIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){
  Out << "return ";
  if (RetVal) {
    RetVal->print(Out, Ctx);
  }
}

IRStmt* ReturnIRStmt::clone() {
  assert(RetVal);
  IRExpr *NewRetVal = RetVal->clone();
  return new ReturnIRStmt(NewRetVal);
}

void ScopeAnnotIRStmt::print(llvm::raw_ostream &Out, IRPrintContext &Ctx){}

IRStmt* ScopeAnnotIRStmt::clone() {
  return new ScopeAnnotIRStmt(SA);
}

//void IRStmt::printAllIdentifiers() {
//  for (auto it = ExprIdentifierIterator(innerStmt); !it.done(); ++it) {
//    llvm::outs() << (*it)->getNameInfo().getAsString() << "\n";
//  }
//}

///////////////////
// IRBasicBlock //
/////////////////

void IRBasicBlock::iteratePreds(std::function<void(IRBasicBlock *B)> CB) {
  for (auto &B : *Parent) {
    auto &BSuccs = (B.get())->Succs;
    if (BSuccs.contains(this)) {
      CB(B.get());
    }
  }
}

void IRBasicBlock::clone(IRBasicBlock *Dest) {
  for (auto &Stmt : Stmts) {
    Dest->pushStmtBack(Stmt->clone());
  }
  if (Term != nullptr) {
    Dest->Term = dyn_cast<IRTerminatorStmt>(Term->clone());
  }
}

IRBasicBlock* IRBasicBlock::splitAt(int Index) {
  assert(Index >= 0 && Index < Stmts.size());
  auto *RBlock = Parent->createBlock();
  for (int i = Index; i < Stmts.size(); i++) {
    RBlock->pushStmtBack(Stmts[i].release());
  }
  if (Term) {
    RBlock->Term = Term;
    Term = nullptr;
  }
  Stmts.resize(Index);
  return RBlock;
}

IRStmt* IRBasicBlock::getAt(int Index) {
  assert(Index >= 0 && Index < Stmts.size());
  return Stmts[Index].get();
}

void IRBasicBlock::removeAt(int Index) {
  assert(Index >= 0 && Index < Stmts.size());
  auto it = begin() + Index;
  Stmts.erase(it);
}

void IRBasicBlock::print(llvm::raw_ostream &Out, IRPrintContext &Ctx) {
  int I = 1;
  int j = 0;

  std::string OutS;
  llvm::raw_string_ostream OutOS(OutS);

  llvm::raw_ostream *OutT = Ctx.GraphVizEscapeChars ? &OutOS : &Out;
  for (auto &Stmt : Stmts) {
    *OutT << "   " << I << ": ";
    Stmt->print(*OutT, Ctx);
    *OutT << Ctx.NewlineSymbol;

    I++;
  }
  if (Term) { 
    *OutT << "   T: ";
    Term->print(*OutT, Ctx);
    *OutT << Ctx.NewlineSymbol;
  }

  if (Ctx.GraphVizEscapeChars) {
    //|(\\[^l])
    std::regex re("([\"<>]|\\\\n)");
    OutS = std::regex_replace(OutS, re, "\\$1");
    Out << OutS;
  }
}

void IRBasicBlock::dumpGraph(llvm::raw_ostream &Out,
                             clang::ASTContext &Context) {
  Out << "\"{ [B" << getInd();
  Out << "]\\l";
  IRPrintContext Ctx = IRPrintContext {
    .ASTCtx = Context,
    .NewlineSymbol = "\\l",
    .GraphVizEscapeChars = true
  };
  print(Out, Ctx);
  Out << "}\"";
}

/////////////////
// IRFunction //
///////////////


IRBasicBlock *IRFunction::createBlock() {
  IRBlockPtr B = std::make_unique<IRBasicBlock>(Blocks.size(), this);
  IRBasicBlock *Bp = B.get();
  Blocks.push_back(std::move(B));
  return Bp;
}

void IRFunction::moveBlock(IRBasicBlock *B, IRFunction *Dest) {
  IRFunction::iterator BlockIt = begin();
  std::advance(BlockIt, B->getInd());
  IRBlockPtr OwnedB = std::move(*BlockIt);
  Blocks.erase(BlockIt);
  int I = 0;
  for (auto &MyB : Blocks) {
    MyB->Ind = I;
    I++;
  }
  B->Ind = Dest->Blocks.size();
  Dest->Blocks.push_back(std::move(OwnedB));
  B->Parent = Dest;
}

void IRFunction::cleanVars() {
  std::set<IRVarRef> accessed;

  for (auto &B: *this) {
    auto VisitF = [&](auto &VR, bool lhs){ 
      accessed.insert(VR);
    };
    for (auto &S: *B) {
      ExprIdentifierVisitor _(S.get(), VisitF);
    }
    if (B->Term) {
      ExprIdentifierVisitor _(B->Term, VisitF);
    }
  }

  auto it = Vars.begin();
  while (it != Vars.end()) {
    auto *VR = &(*it);
    if (accessed.find(VR) == accessed.end() && VR->DeclLoc != IRVarDecl::ARG) {
      // Remove variable declaration
      auto itc = it;
      it++;
      Vars.erase(itc);
    } else {
      it++;
    }
  }
}


void IRFunction::print(llvm::raw_ostream &out, clang::ASTContext &Context) {
  int i = 0;
  for (auto &B : Blocks) {
    fprintf(stdout, BHGREEN "Block %d" COLOR_RESET "\n", i);
    out << "PREDS: ";
    B->iteratePreds(
        [&](IRBasicBlock *Pred) -> void { out << Pred->getInd() << " "; });
    out << "\n";
    IRPrintContext Ctx = IRPrintContext {
      .ASTCtx = Context,
      .NewlineSymbol = "\n"
    };
    B->print(out, Ctx);
    out << "SUCCS: ";
    for (auto *Succ : B->Succs) {
      out << Succ->getInd() << " ";
    }
    out << "\n\n\n";
    i++;
  }
}

void IRFunction::dumpGraph(llvm::raw_ostream &out, clang::ASTContext &Context) {
  out << "subgraph clusterfn" << Ind;
  out << "{\nlabel=\"" << getName() << "\"\n";

  for (auto &B : Blocks) {
    auto *BB = B.get();
    out << "    Node" << Ind << "_" << BB->getInd();
    out << " [shape=record,";
    if (BB->Ind == 0) {
      out << "fontcolor=\"blue\",color=\"blue\",";
    }/* else if (BB == BB->Parent->Exit) {
      out << "fontcolor=\"green\",color=\"green\",";
    }*/
    out << "label=";
    BB->dumpGraph(out, Context);
    out << " ];\n";
  }

  for (auto &B : Blocks) {
    for (auto &Succ : B.get()->Succs) {
      out << "    Node" << Ind << "_" << B.get()->getInd();
      out << " -> Node" << Ind << "_" << Succ->getInd();
      out << ";\n";
    }
    /*
    const IRBasicBlock *HasSpawnToSpawnNext = nullptr;
    for (auto &S : *B) {
      if ((Spawn2SpawnNext.find(S.get()) != Spawn2SpawnNext.end())) {
        HasSpawnToSpawnNext = Spawn2SpawnNext[S.get()];
        break;
      }
    }
    if (HasSpawnToSpawnNext) {
      out << "    \"Node" << Ind << "_" << B->getInd();
      out << "\" -> \"Node" << Ind << "_" << HasSpawnToSpawnNext->getInd()
          << "\"";
      out << "  [style=\"dashed\" color=\"green\"];\n";
    }*/
  }
  out << "}\n";
}

/*
void IRFunction::dumpArgs(llvm::raw_ostream &out) {
  out << "\tArgs: ";
  for (auto *v : Args) {
    out << v->getName() << ", ";
  }
  out << "\n\tLocals: ";
  for (auto *v : Locals) {
    out << v->getName() << ", ";
  }
  if (!Materialized.empty()) {
    out << "\n\tMaterialized: ";
    for (auto *v : Materialized) {
      out << v->getName() << ", ";
    }
  }
  out << "\n";
}*/

////////////////
// IRPRogram //
//////////////

IRFunction *IRProgram::createFunc(const std::string &Name) {
  IRFuncPtr F = std::make_unique<IRFunction>(Funcs.size(), Name, this);
  IRFunction *Fp = F.get();
  Funcs.push_back(std::move(F));
  return Fp;
}


void IRProgram::print(llvm::raw_ostream &out, clang::ASTContext &Context) {
  for (auto &F : Funcs) {
    F.get()->print(out, Context);
  }
}

void IRProgram::dumpGraph(llvm::raw_ostream &out, clang::ASTContext &Context) {
  out << "digraph unnamed {\ncompound=true;\n";
  for (auto &F : Funcs) {
    F->dumpGraph(out, Context);
  }
  //for (auto &F : Funcs) {
  //  for (const auto &[B, SnD] : F->SpawnNext2Cont) {
  //    out << "    \"Node" << F->Ind << "_" << B->getInd();
  //    out << "\" -> \"Node" << SnD->Ind << "_" << 0 << "\"";
  //    out << "  [style=\"dashed\" color=\"red\" lhead=clusterfn";
  //    out << SnD->Ind << "];\n";
  //  }
  //}
  out << "}\n";
}


////////////////////////
// ScopedIRTraverser //
//////////////////////

IRBasicBlock* FindJoin(IRBasicBlock* Left, IRBasicBlock *Right) {
  std::vector<IRBasicBlock*> WorkList;
  std::unordered_map<IRBasicBlock*, bool> Seen;

  WorkList.push_back(Left);
  while (!WorkList.empty()) {
    auto *B = WorkList.back();
    WorkList.pop_back();
    
    if (Seen.find(B) != Seen.end()) continue;
    Seen[B] = true;
    for (auto *Succ: B->Succs) {
      WorkList.push_back(Succ);
    }
  }

  WorkList.push_back(Right);
  while (!WorkList.empty()) {
    auto *B = WorkList.back();
    WorkList.pop_back();

    if (Seen.find(B) != Seen.end()) {
      if (Seen[B]) {
        return B;
      } else {
        continue;
      }
    } 
    Seen[B] = false;
    for (auto *Succ: B->Succs) {
      WorkList.push_back(Succ);
    }
  }
  return nullptr;
}


void ScopedIRTraverser::traverse(IRFunction &F) {
  WorkList.push_back(WorkItem(F.getEntry()));

  while (!WorkList.empty()) {
    auto W = WorkList.back();
    WorkList.pop_back();
    
    if (W.B) {
      assert(W.SE == None);
      auto B = W.B;

      int JC = 1;
      if (JoinCounts.find(B) != JoinCounts.end()) {
        JC = JoinCounts[B];
      }
      JC--;
      JoinCounts[B] = JC;
  
      if (JC != 0) {
        continue;
      }

      visitBlock(B);

      if (B->Term && isa<IfIRStmt>(B->Term)) {
        auto *ThenB = B->Succs[0];
        auto *ElseB = B->Succs[1];
        auto *JoinB = FindJoin(ThenB, ElseB);
        assert(JoinB != ThenB);
        if (JoinB) {
          WorkList.push_back(WorkItem(JoinB));
          JoinCounts[JoinB] = 2;
        }
        WorkList.push_back(WorkItem(Close));
        if (ElseB && ElseB != JoinB) {
          WorkList.push_back(WorkItem(ElseB));
          WorkList.push_back(WorkItem(Else));
          JoinCounts[JoinB] += 1;
        }
        WorkList.push_back(WorkItem(ThenB));
        WorkList.push_back(WorkItem(Open));
      } else if (B->Term && isa<LoopIRStmt>(B->Term) ) {
        auto *BodyB = B->Succs[0];
        auto *AfterB = B->Succs[1];
        // The common successor of the body and the loop itself should be the loop.
        assert(FindJoin(BodyB, B) == B);
  
        // we don't need a join count for AfterB because 
        // it will be looped back already
        WorkList.push_back(WorkItem(AfterB));
        WorkList.push_back(WorkItem(Close));
        WorkList.push_back(WorkItem(BodyB));
        WorkList.push_back(WorkItem(Open));
      } else {
        assert(B->Succs.size() <= 1);
        for (auto *Succ: B->Succs) {
          WorkList.push_back(WorkItem(Succ));
        }
      }
    } else {
      assert(W.SE != None);
      handleScope(W.SE);
    }
  }
}