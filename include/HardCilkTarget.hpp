#pragma once 

#include "IR.hpp"
#include "clang/AST/ASTContext.h"
#include <unordered_map>

struct HCTaskInfo {
    std::set<IRFunction*> SendArgList;
    //bool needsAllocator = false;
    //bool needsArgNotif = false;
    bool isRoot = false;
    bool isCont = false;
    HCTaskInfo() {}
};

class HardCilkTarget {
private:
    IRProgram &P;
    std::unordered_map<IRFunction*, HCTaskInfo> TaskInfos;

    void analyzeSendArguments();
public:
    HardCilkTarget(IRProgram &P);

    void PrintHardCilk(llvm::raw_ostream &out, clang::ASTContext &C);
    void PrintTaskDescriptor(llvm::raw_ostream &out);
};
