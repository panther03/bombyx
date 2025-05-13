#pragma once 
#include <clang/AST/ASTFwd.h>
#include <clang/AST/ASTContext.h>
#include <clang/Basic/SourceManager.h>

#include "IR.hpp"

void OpenCilk2IR(IRProgram &P, clang::ASTContext *Context, SourceManager &SM);