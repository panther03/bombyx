#pragma once 

#include "IR.hpp"
#include "clang/AST/ASTContext.h"
#include <unordered_map>

enum HardCilkType {
    TY_UINT8,
    TY_UINT16,
    TY_UINT32,
    TY_UINT64,
    TY_ADDR,
    TY_LAST
};

struct HCTaskInfo {
    std::set<IRFunction*> SendArgList;
    bool IsRoot = false;
    bool IsCont = false;
    size_t TaskSize;
    HardCilkType RetTy;
    HCTaskInfo() {}
};

class HardCilkTarget {
private:
    IRProgram &P;
    const std::string &AppName;
    std::unordered_map<IRFunction*, HCTaskInfo> TaskInfos;
    bool ArgOutImplList[TY_LAST] = {false};

    void analyzeSendArguments();

    void PrintDef(llvm::raw_ostream &Out, IRFunction *Task, HCTaskInfo &Info);
public:
    HardCilkTarget(IRProgram &P, const std::string &AppName);

    void PrintHardCilk(llvm::raw_ostream &out, clang::ASTContext &C);
    void PrintDescJson(llvm::raw_ostream &out);
    void PrintDriver(llvm::raw_ostream &out);
    void PrintDefs(llvm::raw_ostream &out);
};
