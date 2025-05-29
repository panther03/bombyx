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
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/TokenKinds.h"
#include "llvm/Support/raw_ostream.h"

#include "HardCilkTarget.hpp"
#include "util.hpp"
#include "IR.hpp"
#include "Cilk1EmuTarget.hpp"
#include "MakeExplicit.hpp"
#include "OpenCilk2IR.hpp"
#include "DAE.hpp"
#include "CountSpawns.hpp"


using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace clang::driver;

int VERBOSITY = 0;

struct ConvertOpts {
  std::set<int> DumpPasses;
  enum {
    TG_CILK1EMU,
    TG_HARDCILK
  } Target = TG_CILK1EMU;
};

ConvertOpts GOpts;

class CilkConvert : public clang::ASTConsumer {
private:
  clang::CompilerInstance &CI;

  IRProgram P;
  StringRef OutFilename;

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
      [&](IRProgram &P) -> void {
        DAE(P);
      },
      [&](IRProgram& P) -> void {
        MakeExplicit(P);
      },
      [&](IRProgram& P) -> void {
        CountSpawns(P, Context);
      },
      [&](IRProgram &P) -> void {
        switch (GOpts.Target) {
          case ConvertOpts::TG_CILK1EMU: {
            llvm::raw_fd_ostream Cilk1Out(OutFilename, EC, llvm::sys::fs::OF_Text);
            PrintCilk1Emu(P, Cilk1Out, Context, CI);
            break;
          };
          case ConvertOpts::TG_HARDCILK: {
            std::string TaskJsonName = OutFilename.str() + "_task.json";
            llvm::raw_fd_ostream TaskJson(TaskJsonName, EC, llvm::sys::fs::OF_Text);
            HardCilkTarget HT(P);
            HT.PrintTaskJson(TaskJson);
            break;
          }
        }
      },
    };

    IRPrintContext Ctx = IRPrintContext{.ASTCtx = Context, .NewlineSymbol = "\n"};

    for (int i = 0; i < Passes.size(); i++) {
      Passes[i](P);
      if (GOpts.DumpPasses.find(i) != GOpts.DumpPasses.end()) {
        std::string fname = "ir" + std::to_string(i) + ".dot";
        llvm::raw_fd_ostream DotFile(fname, EC, llvm::sys::fs::OF_Text);
        if (EC) {
          PANIC("could not open file %s", fname.c_str());
        }
        P.dumpGraph(DotFile, Context);
        DotFile.close();
      }
    }
  }
};

class BombyxPragmaHandler : public clang::PragmaHandler {
  public:
    BombyxPragmaHandler() : PragmaHandler("BOMBYX") {}
  
      void HandlePragma(clang::Preprocessor &PP, 
                       clang::PragmaIntroducer Introducer,
                       clang::Token &FirstToken) override {          

          clang::Token Tok;
          PP.Lex(Tok);
          // Check if we got an identifier (like "DAE")
          if (Tok.is(clang::tok::identifier)) {
              std::string Arg = PP.getSpelling(Tok);

              if (Arg != "DAE") {
                  PANIC("unknown bombyx pragma %s", Arg.c_str());
              }
          }
          else {
              PP.Diag(Tok.getLocation(), clang::diag::err_expected_after) 
                  << "BOMBYX";
          }

          // Consume remaining tokens until end of directive
          PP.Lex(Tok);
          while (!Tok.is(clang::tok::eod)) {
              PP.Lex(Tok);
          }

          // 1. Create the tokens
          Token LabelTok;
          LabelTok.startToken();
          LabelTok.setKind(tok::identifier);
          LabelTok.setIdentifierInfo(PP.getIdentifierInfo("__bombyx_dae_here"));

          Token ColTok;
          ColTok.startToken();
          ColTok.setKind(tok::colon);

          SmallVector<Token, 2> TokenList;
          TokenList.push_back(LabelTok);
          TokenList.push_back(ColTok);

          for(Token& Tok : TokenList)
            Tok.setLocation(FirstToken.getLocation());

          ArrayRef TokenArray = TokenList;
          PP.EnterTokenStream(TokenArray,          
            /*DisableMacroExpansion=*/false,
           /*IsReinject=*/false);
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

    auto H = new BombyxPragmaHandler();
    CI.getPreprocessor().AddPragmaHandler(H);
    if (!PP.getPreprocessingRecord()) {
      PP.createPreprocessingRecord();
    }
    clang::PreprocessingRecord *PPRec = PP.getPreprocessingRecord();

    return std::make_unique<CilkConvert>(CI, OutFilename);
  }

private:
  StringRef OutFilename;
};

void set_target(const char *targ)  {
  if (strcmp(targ, "cilk1emu") == 0) {
    GOpts.Target = ConvertOpts::TG_CILK1EMU;
  } else if (strcmp(targ, "hardcilk") == 0) {
    GOpts.Target = ConvertOpts::TG_HARDCILK;
  } else if (strcmp(targ, "help") == 0) {
    fprintf(stderr, "Available targets: cilk1emu, hardcilk\n");
    exit(EXIT_SUCCESS);
  } else {
    PANIC("unrecognized target %s", targ);
  }
}

int main(int argc, char *argv[]) {
  opterr = 0;
  int c;

  static struct option long_options[] = {
    {"fdump-dot", required_argument, 0, 0 },
    {"target", required_argument, 0, 0 },
  };
  int option_index = -1;
  while ((c = getopt_long(argc, argv, "vVt:", long_options, &option_index)) != -1) {
    switch (c) {
    case 0: 
      switch (option_index) {
        case 0: {
          char* ps = optarg;
          char* p = optarg;
          do {
            char po = *p;
            if (*p == 0 || *p == ',') {
              *p = 0;
              GOpts.DumpPasses.insert(atoi(ps));
              ps = p + 1;
            }
            *p = po;
          } while (*(p++) != 0);
          break;
        }
        case 1: {
          set_target(optarg);
        }
      }
      break;
    case 't':
      set_target(optarg);
      break;
    case 'v':
      VERBOSITY = 1;
      break;
    case 'V':
      VERBOSITY = 2;
      break;
    default: /* '?' */
      fprintf(stderr, 
        "Usage: %s [OPTION]... INFILE OUTFILE\n"
        "   -v                      \t verbose\n"
        "   -V                      \t very verbose\n"
        "       --fdump-dot=<PASSES>\t Indices of passes to dump GraphViz output after, comma separated\n"
        "   -t, --target=<TARGET>\t Output backend. Use TARGET=help to print available\n",
              argv[0]);
      exit(EXIT_FAILURE);
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
    "-Wno-unused-label",
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