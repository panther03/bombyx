#include "HardCilkTarget.hpp"
#include "IR.hpp"
#include "clang/AST/Type.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <llvm/Support/JSON.h>
#include <iostream>

#include "OpenCilk2IR.hpp"


#define ALIGN(x, A) ((x + A - 1) & -(A))
#define PADDING(x, A) (ALIGN(x, A) - x)

bool typeIsVoid(HardCilkType &Ty) {
  const HardCilkBaseType *BTy = std::get_if<HardCilkBaseType>(&Ty);
  return BTy && (*BTy == TY_VOID);
}

HardCilkRecordType clangRecordTypeToHardCilk(const RecordDecl *RD) {
    // loop through the fields of the record type
    std::vector<HardCilkRecordField> fields;
    for (auto field : RD->fields()) {
      std::unique_ptr<HardCilkType> HCT(clangTypeToHardCilk(field->getType()));
      fields.push_back(std::make_pair(field->getName().str(), std::move(HCT)));
    }
    return HardCilkRecordType {
      .Name = RD->getName().str(),
      .Fields = std::move(fields)
    };
}

HardCilkType* clangTypeToHardCilk(IRType &Ty) {
  HardCilkType *HCT = new HardCilkType();
  if (auto BTy = dyn_cast<BuiltinType>(Ty.getTypePtr())) {
    switch (BTy->getKind()) {
    // TODO ???? idk what to map here
    case clang::BuiltinType::Int:
      *HCT = TY_UINT32; break;
    case clang::BuiltinType::UInt:
      *HCT = TY_UINT32; break;
    case clang::BuiltinType::Char8:
      *HCT = TY_UINT8; break;
    case clang::BuiltinType::UChar:
      *HCT = TY_UINT8; break;
    case clang::BuiltinType::UShort:
      *HCT = TY_UINT16; break;
    case clang::BuiltinType::Short:
      *HCT = TY_UINT16; break;
    case clang::BuiltinType::Long:
      *HCT = TY_UINT32; break;
    case clang::BuiltinType::ULong:
      *HCT = TY_UINT32; break;
    case clang::BuiltinType::LongLong:
      *HCT = TY_UINT64; break;
    case clang::BuiltinType::ULongLong:
      *HCT = TY_UINT64; break;
    case clang::BuiltinType::Void:
      *HCT = TY_VOID; break;
    default: {
      PANIC("Unrecognized builtin type kind %d\n", BTy->getKind());
    }
    }
  } else if (isa<PointerType>(Ty.getTypePtr())) {
    *HCT = TY_ADDR;
  } else if (auto *ET = dyn_cast<ElaboratedType>(Ty.getTypePtr()))
  {
    auto *RT = ET->getNamedType()->getAs<RecordType>();
    if (!RT) {
      PANIC("Unrecognized elaborated type %s", ET->getTypeClassName());
    }
    auto *RD = RT->getAsRecordDecl();
    HardCilkRecordType HCRT = clangRecordTypeToHardCilk(RD);
   /*std::string ElabName;
    llvm::raw_string_ostream OutS(ElabName);
   auto *Q = ET->getQualifier();
   Q->dump(OutS);
    HCRT.Name = ElabName;*/
    *HCT = std::move(HCRT);
  } else if (auto *RT = dyn_cast<RecordType>(Ty.getTypePtr())) {
    auto *RD = RT->getAsRecordDecl();
    *HCT = clangRecordTypeToHardCilk(RD);
  } else {
    PANIC("Unrecognized LLVM type class: %s", Ty->getTypeClassName());
  }
  return HCT;
}

int hardCilkTypeSize(HardCilkBaseType Ty) {
  switch (Ty) {
  case TY_UINT8:
    return 1;
  case TY_UINT16:
    return 2;
  case TY_UINT32:
    return 4;
  case TY_UINT64:
    return 8;
  case TY_ADDR:
    return 8;
  default:
    return -1;
  }
  return -1;
}

int hardCilkTypeSize(HardCilkType *Ty) {
  if (auto *BTy = std::get_if<HardCilkBaseType>(Ty)) {
    return hardCilkTypeSize(*BTy);
  } else {
    auto &RTy = std::get<HardCilkRecordType>(*Ty);
    int size = 0;
    for (auto &[_,FieldTy] : RTy.Fields) {
      size += hardCilkTypeSize(FieldTy.get());
    }
    DBG {
      std::cerr << "record size: " << size << "\n";
    }
    return size;
  }
}

const char *printHardCilkType(HardCilkBaseType Ty) {
  switch (Ty) {
  case TY_UINT8:
    return "uint8_t";
  case TY_UINT16:
    return "uint16_t";
  case TY_UINT32:
    return "uint32_t";
  case TY_UINT64:
    return "uint64_t";
  case TY_ADDR:
    return "addr_t";
  default:
    return nullptr;
  }
}

llvm::raw_ostream & printHardCilkType(llvm::raw_ostream &Out, HardCilkType *Ty, bool Short=false) {
  if (auto *BTy = std::get_if<HardCilkBaseType>(Ty)) {
    Out << printHardCilkType(*BTy);
  } else {
    if (!Short) {
      Out << "struct ";
    } 
    Out << std::get<HardCilkRecordType>(*Ty).Name;
  }
  return Out;
}

void HardCilkTarget::analyzeSendArguments() {
  bool SomethingHappened = true;

  while (SomethingHappened) {
    SomethingHappened = false;

    for (auto &[F, Info] : TaskInfos) {
      for (auto &B : *F) {
        for (auto &S : *B) {
          // 1. A function F spawned with the continuation pointing at the
          // closure of a function G has G in its send argument list.
          if (auto *ES = dyn_cast<ESpawnIRStmt>(S.get())) {
            if (!ES->SN) continue;
            assert(TaskInfos.find(ES->Fn) != TaskInfos.end());
            assert(TaskInfos.find(ES->SN->Fn) != TaskInfos.end());
            auto &ESFInfo = TaskInfos[ES->Fn];
            auto SizeI = ESFInfo.SendArgList.size();
            ESFInfo.SendArgList.insert(ES->SN->Fn);
            SomethingHappened =
                SomethingHappened || (SizeI != (ESFInfo.SendArgList.size()));
          }
        }
        SpawnNextIRStmt *SNS = nullptr;
        // 2. A continuation inherits all of its root function's send argument
        // destinations.
        if (B->Term && (SNS = dyn_cast<SpawnNextIRStmt>(B->Term))) {
          assert(TaskInfos.find(SNS->Fn) != TaskInfos.end());
          auto &ContInfo = TaskInfos[SNS->Fn];
          auto SizeI = ContInfo.SendArgList.size();
          ContInfo.SendArgList.insert(Info.SendArgList.begin(),
                                      Info.SendArgList.end());
          SomethingHappened =
              SomethingHappened || (SizeI != (ContInfo.SendArgList.size()));
        }
      }
    }
  }
}

HardCilkTarget::HardCilkTarget(IRProgram &P, const std::string &AppName)
    : P(P), AppName(AppName) {
  for (auto &F : P) {
    for (auto &G : F->Info.SpawnList) {
      if (TaskInfos.find(G) == TaskInfos.end()) {
        TaskInfos[G] = HCTaskInfo();
      }
      TaskInfos[G].IsRoot |= !F->Info.IsTask;
    }
    // TODO: not very precise; won't handle the case of a spawn next inside a
    // continuation task that is from the main we need to just mark what tasks
    // main spawns and go from there
    if (F->Info.IsTask) {
      for (auto &G : F->Info.SpawnNextList) {
        if (TaskInfos.find(G) == TaskInfos.end()) {
          TaskInfos[G] = HCTaskInfo();
        }
        TaskInfos[G].IsCont = true;
      }
    }
  }
  for (auto &[T, Info] : TaskInfos) {
    Info.TaskSize =
        8 + (Info.IsCont
                 ? 4
                 : 0); // extra 8 bytes for continuation and 4 for counter
    for (auto &Var : T->Vars) { 
      if (Var.DeclLoc == IRVarDecl::ARG) {
        auto *HCT = clangTypeToHardCilk(Var.Type);
        Info.TaskSize += hardCilkTypeSize(HCT);
        delete HCT;
      }
    }
    Info.TaskPadding = PADDING(Info.TaskSize, 32);
    Info.RetTy = std::unique_ptr<HardCilkType>(clangTypeToHardCilk(T->getReturnType()));
  }
  analyzeSendArguments();
}

llvm::json::Object getSchedulerSide(HCTaskInfo &TaskInfo) {
  llvm::json::Object obj;
  obj["sideType"] = "scheduler";
  obj["numVirtualServers"] = 1;
  obj["capacityVirtualQueue"] = 4096;
  obj["capacityPhysicalQueue"] = 64;
  int64_t totalSize = (TaskInfo.TaskSize + TaskInfo.TaskPadding) * 8;
  obj["portWidth"] = totalSize;
  return obj;
}

llvm::json::Object getArgumentNotifierSide() {
  llvm::json::Object obj;
  obj["sideType"] = "argumentNotifier";
  obj["numVirtualServers"] = 1;
  obj["capacityVirtualQueue"] = 128;
  obj["capacityPhysicalQueue"] = 32;
  obj["portWidth"] = 64;
  return obj;
}

llvm::json::Object getAllocatorSide() {
  llvm::json::Object obj;
  obj["sideType"] = "allocator";
  obj["numVirtualServers"] = 1;
  obj["capacityVirtualQueue"] = 4096;
  obj["capacityPhysicalQueue"] = 32;
  obj["portWidth"] = 64;
  return obj;
}

llvm::json::Object printTaskDescriptor(IRFunction *Task, HCTaskInfo &TaskInfo) {
  llvm::json::Object obj;
  obj["name"] = Task->getName();
  obj["peHDLPath"] = "?";
  obj["isRoot"] = TaskInfo.IsRoot;
  obj["isCont"] = TaskInfo.IsCont;
  obj["dynamicMemAlloc"] = false;
  int64_t closureSize = (TaskInfo.TaskPadding + TaskInfo.TaskSize * 8); // closure size in bits
  obj["widthTask"] = closureSize;
  obj["widthMalloc"] = 0;
  obj["variableSpawn"] = false;
  std::vector<llvm::json::Value> sidesConfigs{getSchedulerSide(TaskInfo)};
  if (TaskInfo.IsCont) {
    sidesConfigs.push_back(getArgumentNotifierSide());
    sidesConfigs.push_back(getAllocatorSide());
  }
  obj["sidesConfigs"] = sidesConfigs;
  return obj;
}

void HardCilkTarget::PrintDescJson(llvm::raw_ostream &Out) {
  llvm::json::Object obj;
  obj["name"] = AppName;
  std::vector<llvm::json::Value> taskDescriptors;
  llvm::json::Object spawnList;
  llvm::json::Object spawnNextList;
  llvm::json::Object sendArgumentList;
  llvm::json::Object mallocList;
  for (auto &[F, Info] : TaskInfos) {
    taskDescriptors.push_back(printTaskDescriptor(F, Info));
    std::vector<llvm::json::Value> spawnListF;
    for (auto G : F->Info.SpawnList) {
      spawnListF.push_back(llvm::json::Value(G->getName()));
    }
    std::vector<llvm::json::Value> spawnNextListF;
    for (auto G : F->Info.SpawnNextList) {
      spawnNextListF.push_back(llvm::json::Value(G->getName()));
    }
    std::vector<llvm::json::Value> sendArgumentListF;
    for (auto G : Info.SendArgList) {
      sendArgumentListF.push_back(llvm::json::Value(G->getName()));
    }
    spawnList[F->getName()] = spawnListF;
    spawnNextList[F->getName()] = spawnNextListF;
    sendArgumentList[F->getName()] = sendArgumentListF;
  }
  obj["taskDescriptors"] = taskDescriptors;
  obj["spawnList"] = llvm::json::Value(std::move(spawnList));
  obj["spawnNextList"] = llvm::json::Value(std::move(spawnNextList));
  obj["sendArgumentList"] = llvm::json::Value(std::move(sendArgumentList));
  obj["mallocList"] = llvm::json::Value(std::move(mallocList));
  obj["widthAddress"] = 64;
  obj["widthContCounter"] = 32;
  obj["memorySizeSim"] = 16;
  obj["targetFrequency"] = 300;
  obj["fpgaModel"] = "ALVEO_U55C";
  llvm::json::Value objV(std::move(obj));
  Out << llvm::formatv("{0:2}", objV);
}

const char *DESCRIPTOR_TEMPLATE = R"(#pragma once
#include <cstdint>
#include <stddef.h>
#include <stdint.h>

#define MEM(mem_port, addr, type) \
  *((type(*))((uint8_t *)(mem_port) + (addr)))

#define MEM_ARR(mem_port, addr, idx, type) \
  *((type(*))((uint8_t *)(mem_port) + (addr) + (idx) * sizeof(type)))

#define MEM_STRUCT(mem_port, str, str_type, field) \
    ((str_type*)((uint8_t*)(mem) + (str)))->field

using namespace std;

using addr_t = uint64_t;

)";

#define TAB "  "

class HardCilkPrinter : public ScopedIRTraverser {
private:
  llvm::raw_ostream &Out;
  IRPrintContext &C;
  TaskInfosTy const& TaskInfos;
  int SpawnCtr = 0;
  int IndentLvl = 1;

  llvm::raw_ostream &Indent() {
    for (int i = 0; i < IndentLvl; i++)
      Out << TAB;
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
    default: {
    }
    }
  }

  void handleSpawnNextDecl(ClosureDeclIRStmt *DS, IRFunction *F) {
    const std::string &SpawnNextFnName = DS->Fn->getName();
    const std::string SpawnNextClsName = "SN_" + SpawnNextFnName + "c";
    Indent() << "uint32_t " << SpawnNextClsName << "_cnt = ";
    assert(DS->SpawnCount);
    C.ExprCB(&C, Out, DS->SpawnCount.get());
    Out << ";\n";
    Indent() << SpawnNextFnName << "_task " << SpawnNextClsName << ";\n";
    Indent() << SpawnNextClsName << "._cont = args._cont;\n";
    Indent() << SpawnNextClsName << "._counter = " << SpawnNextClsName << "_cnt;\n";
    Indent() << "addr_t " << SpawnNextClsName << "_k = closureIn.read();\n\n";
  }

  void handleSpawnNext(SpawnNextIRStmt *S, IRFunction *F) {
    const std::string &SpawnNextFnName = S->Fn->getName();
    const std::string SpawnNextName = "SN_" + SpawnNextFnName;

    for (auto &[SrcVar, DstVar] : S->Decl->Caller2Callee) {
      if (!SrcVar->IsEphemeral) {
        Indent() << SpawnNextName << "c." << GetSym(DstVar->Name) << " = ";
        C.IdentCB(Out, SrcVar);
        Out << ";\n";
      }
    }

    Indent() << SpawnNextFnName << "_spawn_next " << SpawnNextName << ";\n";
    Indent() << SpawnNextName << ".addr = SN_" << SpawnNextFnName << "c_k;\n";
    Indent() << SpawnNextName << ".data = SN_" << SpawnNextFnName << "c;\n";
    auto SnInfoIt = TaskInfos.find(S->Fn);
    assert(SnInfoIt != TaskInfos.end());
    auto &SnInfo = SnInfoIt->second;
    size_t SnTaskSize = llvm::Log2_32_Ceil(SnInfo.TaskSize);
    Indent() << SpawnNextName << ".size = " << SnTaskSize << ";\n";
    // TODO: is that what allow should be?
    Indent() << SpawnNextName << ".allow = SN_" << SpawnNextFnName << "c_cnt;\n"; 
    Indent() << "spawnNext.write(" << SpawnNextName << ");\n\n";
  }

  void handleSpawn(ESpawnIRStmt *ES, IRFunction *F) {
    const std::string &SpawnFnName = ES->Fn->getName();
    const std::string SpawnFnArgsName =
        (SpawnFnName + "_args") + std::to_string(SpawnCtr);
        Indent() << SpawnFnName << "_task " << SpawnFnArgsName << ";\n";
    if (ES->SN) {
      const std::string &SpawnNextFnName = ES->SN->Fn->getName();
      const std::string SpawnNextContName = "SN_" + SpawnNextFnName + "c_k";
      if (ES->Dest) {
        assert(ES->Local);
        auto *IdentDest = dyn_cast<IdentIRExpr>(ES->Dest.get());
        assert(IdentDest);
        Indent() << SpawnFnArgsName << "._cont = " << SpawnNextContName
                << " + offsetof(";
        Out << SpawnNextFnName << "_task, " << GetSym(IdentDest->Ident->Name)
            << ");\n";
      } else {
        Indent() << SpawnFnArgsName << "._cont = " << SpawnNextContName << ";\n";
      }
    } else {
      // TODO: is it ok for the task to not notify anything?
    }

    // we expect the functions to be in argument first order
    auto DstArgIt = ES->Fn->Vars.begin();
    for (auto &Arg : ES->Args) {
      auto &DstArg = *DstArgIt;
      assert(DstArg.DeclLoc == IRVarDecl::ARG);
      Indent() << SpawnFnArgsName << "." << GetSym(DstArg.Name);
      Out << " = ";
      C.ExprCB(&C, Out, Arg.get());
      Out << ";\n";
      DstArgIt++;
    }

    if (ES->Fn == F) {
      Indent() << "taskOut.write(" << SpawnFnArgsName << ");\n\n";
    } else {
      Indent() << "taskGlobalOut.write(" << SpawnFnArgsName << ");\n\n";
    }
  }

  void handleSendArg(ReturnIRStmt *RS, IRFunction *F) {
    if (F->isVoid()) {
      return;
    }
    static int RvCnt = 0;
    Indent() << "argOut.write(args._cont);\n";
    HardCilkType *RetType = clangTypeToHardCilk(F->getReturnType());
    printHardCilkType(Indent(), RetType, true) << "_arg_out a" << RvCnt << ";\n";
    Indent() << "a" << RvCnt << ".addr = args._cont;\n";
    Indent() << "a" << RvCnt << ".data = ";
    C.ExprCB(&C, Out, RS->RetVal.get());
    Out << ";\n";
    size_t RetTypeSz = llvm::Log2_32_Ceil(hardCilkTypeSize(RetType));
    Indent() << "a" << RvCnt << ".size = " << RetTypeSz << "; " << "// TODO calculation could be wrong fix manually for now\n";
    Indent() << "a" << RvCnt << ".allow = 1;\n";
    Indent() << "argDataOut.write(a" << RvCnt << ");\n";
    RvCnt++;
    delete RetType;
  }

  void visitStmt(IRStmt *S, IRBasicBlock *B) {
    auto *F = B->getParent();
    if (S->Silent)
      return;

    if (auto *ES = dyn_cast<ESpawnIRStmt>(S)) {
      handleSpawn(ES, F);
      SpawnCtr++;
    } else if (auto *SNS = dyn_cast<SpawnNextIRStmt>(S)) {
      handleSpawnNext(SNS, F);
    } else if (auto *CDS = dyn_cast<ClosureDeclIRStmt>(S)) {
      handleSpawnNextDecl(CDS, F);
    } else if (auto *RS = dyn_cast<ReturnIRStmt>(S)) {
      handleSendArg(RS, F);
    } else {
      Indent();
      S->print(Out, C);
      if (!isa<IfIRStmt>(S) && !isa<LoopIRStmt>(S)) {
        Out << ";\n";
      }
    }
  }

  void visitBlock(IRBasicBlock *B) override {

    for (auto &S : *B) {
      visitStmt(S.get(), B);
    }
    if (B->Term)
      visitStmt(B->Term, B);
  }

public:
  HardCilkPrinter(llvm::raw_ostream &Out, IRPrintContext &C, TaskInfosTy const& TaskInfos)
      : Out(Out), C(C), TaskInfos(TaskInfos) {}
};

void PrintHardCilkTask(llvm::raw_ostream &Out, clang::ASTContext &C,
                      HardCilkPrinter &Printer, IRFunction *Task, HCTaskInfo &Info) {
  std::vector<std::pair<std::string, std::string>> intfs;
  Out << "void " << Task->getName() << " (\n";
  intfs.push_back(std::make_pair("taskIn", Task->getName() + "_task"));
  bool HasMem = false;
  for (auto &Var: Task->Vars) {
    if (Var.DeclLoc == IRVarDecl::ARG) {
      auto *HCT = clangTypeToHardCilk(Var.Type);
      auto *HCBTp = std::get_if<HardCilkBaseType>(HCT);
      if (!HCBTp) {
        delete HCT;
        continue;
      } 
      auto HCBT = *HCBTp;
      delete HCT;
      // assume that an address argument means we need memory
      // and that that's the only way we would need memory bc we don't have malloc yet
      if (HCBT == HardCilkBaseType::TY_ADDR) {
        Out << "  void *mem,\n";
        HasMem = true;
        break;
      }
    }
  }
  bool SpawnsItself = false;
  assert(Task->Info.SpawnList.size() <= 2);
  for (IRFunction *SpawnTask : Task->Info.SpawnList) {
    if (SpawnTask == Task) {
      intfs.push_back(std::make_pair("taskOut", Task->getName() + "_task"));
    } else {
      intfs.push_back(std::make_pair("taskGlobalOut", SpawnTask->getName() + "_task"));
    }
  }
  if (Info.SendArgList.size() > 0) {
    if (Info.SendArgList.size() > 1) {
      PANIC("UNSUPPORTED: more than one send argmuent destination");
    }
    intfs.push_back(std::make_pair("argOut", "uint64_t"));
    if (!typeIsVoid(*Info.RetTy)) {
      std::string ArgDataOutTy;
      llvm::raw_string_ostream ArgDataOutTyS(ArgDataOutTy);
      printHardCilkType(ArgDataOutTyS, Info.RetTy.get(), true);
      intfs.push_back(std::make_pair("argDataOut", ArgDataOutTy + "_arg_out"));
    }
  }
  if (Task->Info.SpawnNextList.size() > 0) {
    if (Task->Info.SpawnNextList.size() > 1) {
      PANIC("UNSUPPORTED: more than one spawn next in a function");
    }
    auto &SNDest = *Task->Info.SpawnNextList.begin();
    intfs.push_back(std::make_pair("closureIn", "uint64_t"));
    intfs.push_back(
        std::make_pair("spawnNext", SNDest->getName() + "_spawn_next"));
  }

  bool first = true; 
  for (auto &[intfName, intfTy] : intfs) {
    if (!first) {
      Out << ",\n";
    }
    Out << "  hls::stream<" << intfTy << "> &" << intfName;
    first = false;
  }
  Out << "\n) {\n\n";

  for (auto &[intfName, _] : intfs) {
    Out << "#pragma HLS INTERFACE mode = axis port = " << intfName << "\n";
  }
  if (HasMem) {
    Out << "#pragma HLS INTERFACE mode = m_axi port = mem\n"; 
  }
  Out << "\n";

  for (auto &Local : Task->Vars) {
    if (Local.DeclLoc == IRVarDecl::LOCAL) {
      Out << TAB;
      auto *HCT = clangTypeToHardCilk(Local.Type);
      printHardCilkType(Out, HCT);
      delete HCT;
      Out << " " << GetSym(Local.Name) << ";\n";
    }
  }

  Out << "  " << intfs[0].second << " args = taskIn.read();\n\n";
  Printer.traverse(*Task);
  Out << "}\n\n";

}

void handleArrow(AccessIRExpr *AE, IRPrintContext *C, llvm::raw_ostream &Out) {
  const QualType PT = (AE->Struct->Type)->getPointeeType();
  std::string StructName = PT.getAsString();
  Out << "MEM_STRUCT(mem, ";
  C->IdentCB(Out, AE->Struct);
  Out << ", " << StructName << ", " << AE->Field << ")";
}

void handleArray(IndexIRExpr *IE, IRPrintContext *C, llvm::raw_ostream &Out) {
  Out << "MEM_ARR(mem, ";
  C->ExprCB(C, Out, IE->Arr.get());
  Out << ", ";
  C->ExprCB(C, Out, IE->Ind.get());
  Out << ", " << IE->ArrType.getAsString() << ")";
}

void handleDeref(DRefIRExpr *DE, IRPrintContext *C, llvm::raw_ostream &Out) {
  Out << "MEM(mem, ";
  C->ExprCB(C, Out, DE->Expr.get());
  Out << ", " << DE->PointeeType.getAsString() << ")";
}

void handleRef(RefIRExpr *RE, IRPrintContext *C, llvm::raw_ostream &Out) {
  Out << "((addr_t)&";
  C->ExprCB(C, Out, RE->E.get());
  Out << ")";
}

void HardCilkTarget::PrintHardCilk(llvm::raw_ostream &Out,
                                   clang::ASTContext &C) {
  Out << "#include \"hls_stream.h\"\n";
  Out << "#include \"" << AppName << "_defs.h\"\n\n";

  IRPrintContext IRC = IRPrintContext{
    .ASTCtx = C,
    .NewlineSymbol = "\n",
    .IdentCB =
        [&](llvm::raw_ostream &Out, IRVarRef VR) {
          switch (VR->DeclLoc) {
          case IRVarDecl::ARG: {
            Out << "args." << GetSym(VR->Name);
            break;
          }
          case IRVarDecl::LOCAL: {
            Out << GetSym(VR->Name);
            break;
          }
          default:
            PANIC("unsupported");
          }
        },
    .ExprCB =
        [&](IRPrintContext *C, llvm::raw_ostream &Out, IRExpr *E) {
          if (auto *AE = dyn_cast<AccessIRExpr>(E)) {
            if (AE->Arrow) {
              handleArrow(AE, C, Out);
            } else {
              AE->print(Out, *C);
            }
          } else if (auto *DE = dyn_cast<DRefIRExpr>(E)) {
            handleDeref(DE, C, Out);
          } else if (auto *IE = dyn_cast<IndexIRExpr>(E)) {
            handleArray(IE, C, Out);
          } else if (auto *RE = dyn_cast<RefIRExpr>(E)) {
            handleRef(RE, C, Out);
          } else {
            E->print(Out, *C);
          }
        }
};
  HardCilkPrinter Printer(Out, IRC, TaskInfos);
  for (auto &[T, Info] : TaskInfos) {
    PrintHardCilkTask(Out, C, Printer, T, Info);
  }
}

void HardCilkTarget::PrintDef(llvm::raw_ostream &Out, IRFunction *Task,
                              HCTaskInfo &Info) {
  Out << "struct __attribute__((packed))" << Task->getName() << "_task {\n";
  Out << "  addr_t _cont;\n";
  if (Info.IsCont) {
    Out << "  uint32_t _counter;\n";
  }
  for (auto &Var : Task->Vars) {
    if (Var.DeclLoc == IRVarDecl::ARG) {
      auto HCTy = clangTypeToHardCilk(Var.Type);
      printHardCilkType(Out << "  ", HCTy) << " " << GetSym(Var.Name)
          << ";\n";
      delete HCTy;
    }
  }
  if (Info.TaskPadding > 0) {
    Out << "  uint8_t _padding[" << Info.TaskPadding << "];\n";
  }
  Out << "};\n\n";

  if (Info.IsCont) {
    size_t SnSize = Info.TaskSize + Info.TaskPadding + hardCilkTypeSize(TY_ADDR) +
                    hardCilkTypeSize(TY_UINT32) * 2;
    size_t SnPadding = PADDING(SnSize, 32);
    Out << "struct " << Task->getName() << "_spawn_next {\n";
    Out << "  addr_t addr;\n";
    Out << "  " << Task->getName() << "_task data;\n";
    Out << "  uint32_t size;\n";
    Out << "  uint32_t allow;\n";
    if (SnPadding > 0) {
      Out << "  uint8_t _padding[" << SnPadding << "];\n";
    }
    Out << "};\n\n";
  }

  bool OkBaseType = true;
  if (auto *BTy = std::get_if<HardCilkBaseType>(Info.RetTy.get())) {
    OkBaseType = !ArgOutImplList[*BTy] && (*BTy != TY_VOID);
  }
  if (Info.SendArgList.size() != 0 && OkBaseType) {
    printHardCilkType(Out << "struct __attribute__((packed)) ", Info.RetTy.get(), true) << "_arg_out {\n";
    Out << "  addr_t addr;\n";
    printHardCilkType(Out << "  ", Info.RetTy.get()) << " data;\n";
    Out << "  uint32_t size;\n";
    Out << "  uint32_t allow;\n";
    size_t argOutSize = hardCilkTypeSize(Info.RetTy.get()) +
                        hardCilkTypeSize(TY_UINT64) +
                        hardCilkTypeSize(TY_UINT32) * 2;
    size_t argOutPadding = PADDING(argOutSize, 32);
    if (argOutPadding > 0) {
      Out << "  uint8_t _padding[" << argOutPadding << "];\n";
    }
    Out << "};\n\n";
    if (auto BTy = std::get_if<HardCilkBaseType>(Info.RetTy.get())) {
      ArgOutImplList[*BTy] = true;
    }
  }
}

void HardCilkTarget::PrintDefs(llvm::raw_ostream &Out) {
  Out << DESCRIPTOR_TEMPLATE;
  for (auto *RD : GRecordDecls) {
    HardCilkRecordType HCRT = clangRecordTypeToHardCilk(RD);
    Out << "struct __attribute__((packed)) " << HCRT.Name << " {\n";
    for (auto &field: HCRT.Fields) {
      printHardCilkType(Out << "  ", field.second.get()) << " " << field.first << ";\n";
    }
    Out << "};\n\n";
  }
  for (auto &[T, Info] : TaskInfos) {
    PrintDef(Out, T, Info);
  }
}

void HardCilkTarget::PrintDriver(llvm::raw_ostream &Out) {
  IRFunction *DF = nullptr;
  IRFunction *DFC = nullptr;
  for (auto &F: P) {
    if (F->getName() == "bombyx_driver") {
      DF = F.get();
      if (DF->Info.SpawnNextList.size() != 1) {
        PANIC("Driver should have exactly one continuation.");
      }
      DFC = *DF->Info.SpawnNextList.begin();
      if (!DFC->Info.SpawnNextList.empty() || !DFC->Info.SpawnList.empty()) {
        PANIC("Driver continuation should not have continuations, or spawn anything");
      }
    }
  }
  if (!DF || !DFC) {
    PANIC("To print driver code, please include a function named bombyx_driver with exactly one cilk_sync.");
  }
  assert(DF->Info.SpawnList.size() == 1 && "unimplemented: handle multiple spawn types in driver");
  
}