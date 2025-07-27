#pragma once
#include <clang/AST/ASTContext.h>
#include <clang/AST/ASTFwd.h>
#include <clang/Basic/SourceManager.h>

#include "IR.hpp"

extern std::set<std::string> GIgnoreFns;
extern std::vector<clang::RecordDecl *> GRecordDecls;
void OpenCilk2IR(IRProgram &P, clang::ASTContext *Context, SourceManager &SM);