#pragma once 

#include "IR.hpp"
#include "clang/AST/ASTContext.h"
#include "clang/Frontend/CompilerInstance.h"

void PrintCilk1Emu(IRProgram &P, llvm::raw_ostream &out, clang::ASTContext &C, clang::CompilerInstance &CI);