#include "HardCilkTarget.hpp"
#include "IR.hpp"

#include <llvm/Support/JSON.h>

void HardCilkTarget::analyzeSendArguments() {
    bool SomethingHappened = true;

    while (SomethingHappened) {
        SomethingHappened = false;

        for (auto &[T1,Info]: TaskInfos) {
            for (auto &B: *T1) {
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
    };
}

HardCilkTarget::HardCilkTarget(IRProgram &P): P(P) {
    for (auto &F: P) {
        for (auto &G: F->Info.SpawnList) {
            if (TaskInfos.find(G) == TaskInfos.end()) {
                TaskInfos[G] = HCTaskInfo();
            }
            TaskInfos[G].isRoot |= !F->Info.IsTask;
        }
        // TODO: not very precise; won't handle the case of a spawn next inside a continuation task that is from the main
        // we need to just mark what tasks main spawns and go from there
        if (F->Info.IsTask) {
            for (auto &G: F->Info.SpawnNextList) {
                if (TaskInfos.find(G) == TaskInfos.end()) {
                    TaskInfos[G] = HCTaskInfo();
                }
                TaskInfos[G].isCont = true;
            }
        }
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
    obj["isRoot"] = TaskInfo.isRoot;
    obj["isCont"] = TaskInfo.isCont;
    obj["dynamicMemAlloc"] = false;
    obj["widthTask"] = 0; // TODO
    obj["widthMalloc"] = 0;
    obj["variableSpawn"] = false;
    std::vector<llvm::json::Value> sidesConfigs {getSchedulerSide()};
    if (TaskInfo.isCont) {
        sidesConfigs.push_back(getArgumentNotifierSide());
        sidesConfigs.push_back(getAllocatorSide());
    }
    obj["sidesConfigs"] = sidesConfigs;
    return obj;
}

void HardCilkTarget::PrintTaskJson(llvm::raw_ostream &Out) {
    llvm::json::Object obj;
    obj["name"] = "???";
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
        //llvm::errs() << F->getName() << " sendArgs:";
        //for (auto *SendArgF: Info.SendArgList) {
        //    llvm::errs() << " " << SendArgF->getName();
        //}
        //llvm::errs() << "\n";
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
    Out << objV;
}

void HardCilkTarget::PrintHardCilk(llvm::raw_ostream &Out, clang::ASTContext &C) {
}