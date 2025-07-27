#pragma once

#include "IR.hpp"
#include "clang/AST/ASTContext.h"
#include <unordered_map>

enum HardCilkBaseType {
  TY_UINT8,
  TY_UINT16,
  TY_UINT32,
  TY_UINT64,
  TY_ADDR,
  TY_VOID,
  TY_LAST
};

struct HardCilkRecordType {
  std::string Name;
  std::vector<std::pair<std::string, std::unique_ptr<std::variant<HardCilkBaseType, HardCilkRecordType>>>> Fields;
};

using HardCilkType = std::variant<HardCilkBaseType, HardCilkRecordType>;
using HardCilkRecordField = std::pair<std::string, std::unique_ptr<HardCilkType>>;


struct HCTaskInfo {
  std::set<IRFunction *> SendArgList;
  bool IsRoot = false;
  bool IsCont = false;
  size_t TaskSize;
  size_t TaskPadding;
  std::unique_ptr<HardCilkType> RetTy;
  HCTaskInfo() {}
};

using TaskInfosTy = std::unordered_map<IRFunction *, HCTaskInfo>;

HardCilkType* clangTypeToHardCilk(IRType &Ty);

class HardCilkTarget {
private:
  IRProgram &P;
  const std::string &AppName;
  TaskInfosTy TaskInfos;
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
