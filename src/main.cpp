#include <cstdlib>
#include <unistd.h>
#include <stdlib.h>
#include <getopt.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/Stmt.h>
#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/SourceManager.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Tooling.h>
#include <iostream>

#include "Cilk1EmuTarget.hpp"
#include "IR.hpp"
#include "MakeExplicit.hpp"
#include "OpenCilk2IR.hpp"
#include "util.hpp"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace clang::driver;

int VERBOSITY = 0;

struct ConvertOpts {
  std::set<int> DumpPasses;
};

class CilkConvert : public clang::ASTConsumer {
private:
  clang::CompilerInstance &CI;

  IRProgram P;
  StringRef OutFilename;

  ConvertOpts Opts;

public:
  explicit CilkConvert(clang::CompilerInstance &CI, StringRef OutFilename)
      : CI(CI), OutFilename(OutFilename) {}

  using PassFn = std::function<void(IRProgram&)>;
  void HandleTranslationUnit(clang::ASTContext &Context) {
    std::error_code EC;
    auto &SM = CI.getSourceManager();

    if (CI.getDiagnostics().getNumErrors() != 0) {
      exit(EXIT_FAILURE);
    }

    std::vector<PassFn> Passes {
      [&](IRProgram& P) -> void {
        OpenCilk2IR(P, &Context, SM);
      },
      [&](IRProgram& P) -> void {
        MakeExplicit(P);
      },
      [&](IRProgram &P) -> void {
        llvm::raw_fd_ostream Cilk1Out(OutFilename, EC, llvm::sys::fs::OF_Text);
        PrintCilk1Emu(P, Cilk1Out, Context, CI);
      }
    };

    IRPrintContext Ctx = IRPrintContext{.ASTCtx = Context, .NewlineSymbol = "\n"};

    for (int i = 0; i < Passes.size(); i++) {
      Passes[i](P);
      if (Opts.DumpPasses.find(i) != Opts.DumpPasses.end()) {
        std::string fname = "ir" + std::to_string(i) + ".dot";
        llvm::raw_fd_ostream DotFile(fname, EC, llvm::sys::fs::OF_Text);
        if (EC) {
          PANIC("could not open file %s", fname.c_str());
        }
        P.dumpGraph(DotFile, Context);
      }
    }
  }
};

// Frontened action to create the custom AST consumer
class CilkConvertAction : public clang::ASTFrontendAction {
public:
  CilkConvertAction(StringRef OutFilename) : OutFilename(OutFilename) {}
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef file) override {
    clang::Preprocessor &PP = CI.getPreprocessor();
    PP.enableIncrementalProcessing();
    if (!PP.getPreprocessingRecord()) {
      PP.createPreprocessingRecord();
    }
    clang::PreprocessingRecord *PPRec = PP.getPreprocessingRecord();

    return std::make_unique<CilkConvert>(CI, OutFilename);
  }

private:
  StringRef OutFilename;
};

int main(int argc, char *argv[]) {
  opterr = 0;
  int c;

  static struct option long_options[] = {
    {"fdump-dot", required_argument, 0, 0 },
  };
  int option_index = -1;
  std::set<int> DumpPasses;
  while ((c = getopt_long(argc, argv, "vV", long_options, &option_index)) != -1) {
    switch (c) {
    case 0: 
      PANIC("NO!");
      if (option_index == 0) {
        char* ps = optarg;
        char* p = optarg;
        do {
          char po = *p;
          if (*p == 0 || *p == ',') {
            *p = 0;
            DumpPasses.insert(atoi(ps));
            ps = p + 1;
          }
          *p = po;
        } while (*(p++) != 0);
      }
      break;
    case 'v':
      VERBOSITY = 1;
      break;
    case 'V':
      VERBOSITY = 2;
      break;
    }
  }

  if (argc - optind < 2) {
    std::cerr << "Expected path to input OpenCilk (C++) file and output file."
              << std::endl;
    return 1;
  }

  std::vector<std::string> compilationFlags = {
    OPENCILK_HOME "/bin/clang",
    "-c",
    "-Wall",
    "-fopencilk",
    "-fsyntax-only",
    };
  compilationFlags.push_back(argv[optind]);

  std::shared_ptr<clang::PCHContainerOperations> PCHContainerOps =
      std::make_shared<clang::PCHContainerOperations>();

  clang::FileSystemOptions FSOpts;
  llvm::IntrusiveRefCntPtr<clang::FileManager> Files(
      new clang::FileManager(FSOpts));

  clang::tooling::ToolInvocation invocation(
      compilationFlags, std::make_unique<CilkConvertAction>(argv[optind+1]), Files.get(),
      PCHContainerOps);
  return !invocation.run();
} 