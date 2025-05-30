#include "HardCilkTarget.hpp"
#include "IR.hpp"
#include "clang/AST/Type.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"

#include <llvm/Support/JSON.h>

#define ALIGN(x, A) ((x+A-1) & -(A))
#define PADDING(x, A) (ALIGN(x,A) - x)

HardCilkType clangTypeToHardCilk(IRType &Ty) {
    if (auto BTy = dyn_cast<BuiltinType>(Ty.getTypePtr())) {
        switch (BTy->getKind()) {
            // TODO ???? idk what to map here
            case clang::BuiltinType::Int: return TY_UINT32;
            case clang::BuiltinType::UInt: return TY_UINT32;
            case clang::BuiltinType::Char8: return TY_UINT8;
            case clang::BuiltinType::UChar: return TY_UINT8;
            case clang::BuiltinType::UShort: return TY_UINT16;
            case clang::BuiltinType::Short: return TY_UINT16;
            case clang::BuiltinType::Long: return TY_UINT32;
            case clang::BuiltinType::ULong: return TY_UINT32;
            case clang::BuiltinType::LongLong: return TY_UINT64;
            case clang::BuiltinType::ULongLong: return TY_UINT64;
            default: {
                PANIC("Unrecognized builtin type kind %d\n", BTy->getKind());
            }
        }
    } else if (isa<PointerType>(Ty.getTypePtr())) {
        return TY_ADDR;
    } else {
        PANIC("Unrecognized LLVM type class: %s", Ty->getTypeClassName());
    }
}

int hardCilkTypeSize(HardCilkType Ty) {
    switch (Ty) {
        case TY_UINT8: return 1;
        case TY_UINT16: return 2;
        case TY_UINT32: return 4;
        case TY_UINT64: return 8;
        case TY_ADDR: return 8;
        default: return -1;
    }
    return -1;
}

const char* printHardCilkType(HardCilkType Ty) {
    switch (Ty) {
        case TY_UINT8: return "uint8_t";
        case TY_UINT16: return "uint16_t";
        case TY_UINT32: return "uint32_t";
        case TY_UINT64: return "uint64_t";
        case TY_ADDR: return "addr_t";
        default: return nullptr;
    }
}

void HardCilkTarget::analyzeSendArguments() {
    bool SomethingHappened = true;

    while (SomethingHappened) {
        SomethingHappened = false;

        for (auto &[F,Info]: TaskInfos) {
            for (auto &B: *F) {
                for (auto &S: *B) {
                    // 1. A function F spawned with the continuation pointing at the closure of a function G
                    // has G in its send argument list.
                    if (auto *ES = dyn_cast<ESpawnIRStmt>(S.get())) {
                        assert(TaskInfos.find(ES->Fn) != TaskInfos.end());
                        assert(TaskInfos.find(ES->SN->Fn) != TaskInfos.end());
                        auto &ESFInfo = TaskInfos[ES->Fn];
                        auto SizeI = ESFInfo.SendArgList.size();
                        ESFInfo.SendArgList.insert(ES->SN->Fn);
                        SomethingHappened = SomethingHappened || (SizeI != (ESFInfo.SendArgList.size()));
                    }
                }
                SpawnNextIRStmt *SNS = nullptr;
                // 2. A continuation inherits all of its root function's send argument destinations.
                if (B->Term && (SNS = dyn_cast<SpawnNextIRStmt>(B->Term))) {
                    assert(TaskInfos.find(SNS->Fn) != TaskInfos.end());
                    auto &ContInfo = TaskInfos[SNS->Fn];
                    auto SizeI = ContInfo.SendArgList.size();
                    ContInfo.SendArgList.insert(Info.SendArgList.begin(), Info.SendArgList.end());
                    SomethingHappened = SomethingHappened || (SizeI != (ContInfo.SendArgList.size()));
                }
            }
        }
    }
}

HardCilkTarget::HardCilkTarget(IRProgram &P, const std::string &AppName): P(P), AppName(AppName) {
    for (auto &F: P) {
        for (auto &G: F->Info.SpawnList) {
            if (TaskInfos.find(G) == TaskInfos.end()) {
                TaskInfos[G] = HCTaskInfo();
            }
            TaskInfos[G].IsRoot |= !F->Info.IsTask;
        }
        // TODO: not very precise; won't handle the case of a spawn next inside a continuation task that is from the main
        // we need to just mark what tasks main spawns and go from there
        if (F->Info.IsTask) {
            for (auto &G: F->Info.SpawnNextList) {
                if (TaskInfos.find(G) == TaskInfos.end()) {
                    TaskInfos[G] = HCTaskInfo();
                }
                TaskInfos[G].IsCont = true;
            }
        }
    }
    for (auto &[T, Info]: TaskInfos) {
        Info.TaskSize = 8 + (Info.IsCont ? 4 : 0); // extra 8 bytes for continuation and 4 for counter
        for (auto &Var: T->Vars) {
            if (Var.DeclLoc == IRVarDecl::ARG) {
                Info.TaskSize += hardCilkTypeSize(clangTypeToHardCilk(Var.Type));
            }
        }
        Info.RetTy = clangTypeToHardCilk(T->getReturnType());
    }
    analyzeSendArguments();
}

llvm::json::Object getSchedulerSide() {
    llvm::json::Object obj;
    obj["sideType"] = "scheduler";
    obj["numVirtualServers"] = 1;
    obj["capacityVirtualQueue"] = 4096;
    obj["capacityPhysicalQueue"] = 64;
    obj["portWidth"] = 256;
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
    obj["widthTask"] = 256; // TODO
    obj["widthMalloc"] = 0;
    obj["variableSpawn"] = false;
    std::vector<llvm::json::Value> sidesConfigs {getSchedulerSide()};
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
        for (auto G: F->Info.SpawnList) {
            spawnListF.push_back(llvm::json::Value(G->getName()));
        }
        std::vector<llvm::json::Value> spawnNextListF;
        for (auto G: F->Info.SpawnNextList) {
            spawnNextListF.push_back(llvm::json::Value(G->getName()));
        }
        std::vector<llvm::json::Value> sendArgumentListF;
        for (auto G: Info.SendArgList) {
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

const char * DESCRIPTOR_TEMPLATE = R"(#pragma once
#include <cstdint>
#include <stddef.h>
#include <stdint.h>

#define MEM_OUT(mem_port, addr, type, value)                                   \
  *((type(*))((uint8_t *)(mem_port) + (addr))) = (value)
#define MEM_IN(mem_port, addr, type)                                           \
  *((type(*))((uint8_t *)(mem_port) + (addr)))

#define MEM_ARR_OUT(mem_port, addr, idx, type, value)                          \
  *((type(*))((uint8_t *)(mem_port) + (addr) + (idx) * sizeof(type))) = (value)
#define MEM_ARR_IN(mem_port, addr, idx, type)                                  \
  *((type(*))((uint8_t *)(mem_port) + (addr) + (idx) * sizeof(type)))

using namespace std;

using addr_t = uint64_t;



)";

#define TAB "  "

class HardCilkPrinter : public ScopedIRTraverser {
private:
    llvm::raw_ostream &Out;
    IRPrintContext &C;
    HCTaskInfo &Info;
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
    const std::string SpawnNextClsName = "SN_" + SpawnNextFnName + "c";
    Indent() << SpawnNextFnName << "_task " << SpawnNextClsName << ";\n";
    Indent() << SpawnNextClsName << "._cont = args.cont;\n";
    Indent() << SpawnNextClsName << "._counter = ";
    assert(DS->SpawnCount);
    DS->SpawnCount->print(Out, C);
    Out << ";\n";
    Indent() << "addr_t " << SpawnNextClsName << "_k = closureIn.read();\n\n";
    }

    void handleSpawnNext(SpawnNextIRStmt *S, IRFunction *F) {
    const std::string &SpawnNextFnName = S->Fn->getName();
    const std::string SpawnNextName = "SN_" + SpawnNextFnName;
    Indent() << SpawnNextFnName << "_spawn_next " << SpawnNextName << ";\n";
    Indent() << SpawnNextName << ".addr = SN_" << SpawnNextFnName << "c_k;\n";
    Indent() << SpawnNextName << ".data = SN_" << SpawnNextFnName << "c;\n";
    Indent() << SpawnNextName << ".size = ?;\n";
    Indent() << SpawnNextName << ".allow = ?;\n";
    Indent() << "spawnNext.write(" << SpawnNextName << ");\n\n";
    }

    void handleSpawn(ESpawnIRStmt *ES, IRFunction *F) {
    const std::string &SpawnFnName = ES->Fn->getName();
    const std::string SpawnFnArgsName = (SpawnFnName + "_args") + std::to_string(SpawnCtr);
    const std::string &SpawnNextFnName = ES->SN->Fn->getName();
    const std::string SpawnNextContName = "SN_" + SpawnNextFnName + "c_k";
    Indent() << SpawnFnName << "_task " << SpawnFnArgsName << ";\n";
    assert(ES->Local);
    auto *IdentDest = dyn_cast<IdentIRExpr>(ES->Dest.get());
    assert(IdentDest);
    Indent() << SpawnFnArgsName << "._cont = " << SpawnNextContName << " + offsetof(";
    Out << SpawnNextFnName << "_task, " << GetSym(IdentDest->Ident->Name) << ");\n";

    // we expect the functions to be in argument first order
    auto DstArgIt = ES->Fn->Vars.begin();
    for (auto &Arg: ES->Args) {
        auto &DstArg = *DstArgIt;
        assert(DstArg.DeclLoc == IRVarDecl::ARG);
        Indent() << SpawnFnArgsName << "." << GetSym(DstArg.Name);
        Out << " = ";
        Arg->print(Out, C);
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
        static int RvCnt = 0;
        Indent() << "argOut.write(args._cont);\n";
        const char* RetTypeStr = printHardCilkType(clangTypeToHardCilk(F->getReturnType()));
        Indent() << RetTypeStr << "_arg_out a" << RvCnt << ";\n";
        Indent() << "a" << RvCnt << ".addr = args._cont;\n";
        Indent() << "a" << RvCnt << ".data = ";
        RS->RetVal->print(Out, C);
        Out << ";\n";
        Indent() << "a" << RvCnt << ".size = sizeof(" << RetTypeStr << ");\n";
        Indent() << "a" << RvCnt << ".allow = 1;\n";
        Indent() << "argDataOut.write(a" << RvCnt << ");\n";
        RvCnt++;
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

        for (auto &S: *B) {
            visitStmt(S.get(), B);
        }
        if (B->Term) visitStmt(B->Term, B);
    }

public:
    HardCilkPrinter(llvm::raw_ostream &Out, IRPrintContext &C, HCTaskInfo &Info) : Out(Out), C(C), Info(Info) {}
};

void PrintHardCilkTask(llvm::raw_ostream &Out, clang::ASTContext &C, IRFunction *Task, HCTaskInfo &Info) {
    std::vector<std::pair<std::string, std::string>> intfs;
    Out << "void " << Task->getName() << " (\n";
    intfs.push_back(std::make_pair("taskIn", Task->getName() + "_task"));
    if (Task->Info.SpawnList.find(Task) != Task->Info.SpawnList.end()) {
        intfs.push_back(std::make_pair("taskOut", Task->getName() + "_task"));
    }
    if (Info.SendArgList.size() > 0) {
        if (Info.SendArgList.size() > 1) {
            PANIC("UNSUPPORTED: more than one send argmuent destination");
        }
        intfs.push_back(std::make_pair("argOut", "uint64_t"));
        std::string ArgDataOutTy = printHardCilkType(Info.RetTy);
        intfs.push_back(std::make_pair("argDataOut", ArgDataOutTy + "_arg_out"));
    }
    if (Task->Info.SpawnNextList.size() > 0) {
        if (Task->Info.SpawnNextList.size() > 1) {
            PANIC("UNSUPPORTED: more than one spawn next in a function");
        }
        auto &SNDest = *Task->Info.SpawnNextList.begin();
        intfs.push_back(std::make_pair("closureIn", "uint64_t"));
        intfs.push_back(std::make_pair("spawnNext", SNDest->getName() + "_spawn_next"));
    }

    for (auto &[intfName, intfTy] : intfs) {
        Out << "  hls::stream<" << intfTy << "> &" << intfName << ",\n";
    }
    Out << ") {\n\n";

    for (auto &[intfName, _] : intfs) {
        Out << "#pragma HLS INTERFACE mode = axis port = " << intfName << "\n";
    }

    Out << "  " << intfs[0].second << " args = taskIn.read();\n\n";
    
    IRPrintContext IRC = IRPrintContext {
        .ASTCtx = C,
        .NewlineSymbol = "\n",
        .IdentCB = [&] (llvm::raw_ostream &Out, IRVarRef VR) {
            switch (VR->DeclLoc) {
              case IRVarDecl::ARG: {
                Out << "args." << GetSym(VR->Name);
                break;
              }
              case IRVarDecl::LOCAL: {
                Out << GetSym(VR->Name);
                break;
              }
              default: PANIC("unsupported");
            }
          },
    };
    HardCilkPrinter Printer(Out, IRC, Info);
    Printer.traverse(*Task);
    Out << "\n}\n";
}

void HardCilkTarget::PrintHardCilk(llvm::raw_ostream &Out, clang::ASTContext &C) {
    Out << "#include \"hls_stream.h\"\n";
    Out << "#include \"" << AppName << "_defs.h\"\n\n";

    for (auto &[T, Info]: TaskInfos) {
        PrintHardCilkTask(Out, C, T, Info);
    }
}

void HardCilkTarget::PrintDef(llvm::raw_ostream &Out, IRFunction *Task, HCTaskInfo &Info) {
    Out << "struct " << Task->getName() << "_task_inner {\n";
    Out << "  addr_t _cont;\n";
    if (Info.IsCont) {
        Out << "  uint32_t _counter;\n";
    }
    for (auto &Var : Task->Vars) {
        if (Var.DeclLoc == IRVarDecl::ARG) {
            auto HCTy = clangTypeToHardCilk(Var.Type);
            Out << "  " << printHardCilkType(HCTy) 
                << " " << GetSym(Var.Name) << ";\n";
        }
    }
    Out << "};\n\n";

    Out << "struct " << Task->getName() << "_task {\n";
    Out << "  " << Task->getName() << "_task_inner inner;\n";
    size_t Padding = PADDING(Info.TaskSize, 32);
    if (Padding > 0) {
        Out << "  uint8_t _padding[" << Padding << "];\n";
    }
    Out << "};\n\n";

    if (Info.IsCont) {
        size_t SnSize = Info.TaskSize + hardCilkTypeSize(TY_ADDR) + hardCilkTypeSize(TY_UINT32) * 2;
        size_t SnPadding = PADDING(SnSize, 32);
        Out << "struct " << Task->getName() << "_spawn_next {\n";
        Out << "  addr_t addr;\n";
        Out << "  " << Task->getName() << "_task_inner data;\n";
        Out << "  uint32_t size;\n";
        Out << "  uint32_t allow;\n";
        if (SnPadding > 0) {
            Out << "  uint8_t _padding[" << SnPadding << "];\n";
        }
        Out << "};\n\n";
    }

    if (Info.SendArgList.size() != 0) {
        if (!ArgOutImplList[Info.RetTy])  {
            Out << "struct " << printHardCilkType(Info.RetTy) << "_arg_out {\n";
            Out << "  addr_t addr;\n";
            Out << "  " << printHardCilkType(Info.RetTy) << " data;\n";
            Out << "  uint32_t size;\n";
            Out << "  uint32_t allow;\n";
            size_t argOutSize = hardCilkTypeSize(Info.RetTy) + hardCilkTypeSize(TY_UINT64) + hardCilkTypeSize(TY_UINT32) * 2;
            size_t argOutPadding = PADDING(argOutSize, 32);
            if (argOutPadding > 0) {
                Out << "  uint8_t _padding[" << argOutPadding << "];\n";
            }
            Out << "};\n\n";
            ArgOutImplList[Info.RetTy] = true;
        }
    }
}

void HardCilkTarget::PrintDefs(llvm::raw_ostream &Out) {
    Out << DESCRIPTOR_TEMPLATE;
    for (auto &[T, Info]: TaskInfos) {
        PrintDef(Out, T, Info);
    }
}

void HardCilkTarget::PrintDriver(llvm::raw_ostream &Out) {
    
}