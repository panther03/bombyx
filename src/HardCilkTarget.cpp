#include "HardCilkTarget.hpp"
#include "IR.hpp"

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

void HardCilkTarget::PrintTaskDescriptor(llvm::raw_ostream &Out) {
    llvm::errs() << "Info\n";
    for (auto &[F, Info] : TaskInfos) {
        llvm::errs() << F->getName() << " sendArgs:";
        for (auto *SendArgF: Info.SendArgList) {
            llvm::errs() << " " << SendArgF->getName();
        }
        llvm::errs() << "\n";
    }
}

void HardCilkTarget::PrintHardCilk(llvm::raw_ostream &Out, clang::ASTContext &C) {
}