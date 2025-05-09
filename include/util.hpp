#pragma once
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>

#include <stdio.h>
#include <deque>

#define BRED "\e[1;31m"
#define BHGREEN "\e[1;92m"
#define BHBLK "\e[1;90m"
#define COLOR_RESET "\e[0m"
#define PANIC(...)                                                             \
  fprintf(stderr,                                                              \
          "[" BRED "panic" COLOR_RESET "@" BHBLK "%s:%d" COLOR_RESET "(%s)] ", \
          __FILE__, __LINE__, __func__);                                       \
  fprintf(stderr, __VA_ARGS__);                                                \
  fprintf(stderr, "\n");                                                       \
  exit(EXIT_FAILURE);

using namespace clang;