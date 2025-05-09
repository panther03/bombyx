#include <clang/AST/ASTConsumer.h>
#include <clang/Basic/SourceManager.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Tooling.h>
#include <clang/AST/Stmt.h>
#include <clang/Basic/SourceLocation.h>

#include <iostream>


#include "Cilk1EmuTarget.hpp"
#include "MakeExplicit.hpp"
#include "OpenCilk2IR.hpp"
#include "IR.hpp"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace clang::driver;

class CilkConvert : public clang::ASTConsumer {
private:
  clang::CompilerInstance &CI;

  IRProgram P;  
  NullStmt Sentinel;
  StringRef OutFilename;

public:
  explicit CilkConvert(clang::CompilerInstance &CI, StringRef OutFilename)
      : CI(CI), Sentinel(SourceLocation()), OutFilename(OutFilename) {}

  void HandleTranslationUnit(clang::ASTContext &Context) {
    std::error_code EC;
    auto &SM = CI.getSourceManager();

    IRPrintContext Ctx = IRPrintContext {
      .ASTCtx = Context,
      .NewlineSymbol = "\n"
    };
    OpenCilk2IR(P, &Context, SM, Sentinel);
    {
      llvm::raw_fd_ostream DotFile("irbefore.dot", EC, llvm::sys::fs::OF_Text);
      if (EC) {
        PANIC("could not open file irbefore.dot");
      }
      P.dumpGraph(DotFile, Context);
    }
    MakeExplicit(P);
    {
      llvm::raw_fd_ostream DotFile2("ir.dot", EC, llvm::sys::fs::OF_Text);
      if (EC) {
        PANIC("could not open file ir.dot");
      }
      P.dumpGraph(DotFile2, Context);
    }
    llvm::raw_fd_ostream Cilk1Out(OutFilename, EC, llvm::sys::fs::OF_Text);
    PrintCilk1Emu(P, Cilk1Out, Context, CI);
/*
    OpenCilk2IR(P, &Context, SM, Sentinel);

    {
      llvm::raw_fd_ostream DotFile("irbefore.dot", EC, llvm::sys::fs::OF_Text);
      if (EC) {
        PANIC("could not open file irbefore.dot");
      }
      P.dumpGraph(DotFile, Context);
    }
    
    {
      llvm::raw_fd_ostream DotFile2("ir.dot", EC, llvm::sys::fs::OF_Text);
      if (EC) {
        PANIC("could not open file ir.dot");
      }
      P.dumpGraph(DotFile2, Context);
    }
    auto *F0 = P.front().get();
    ScopedIRPrinter(&Context).traverse(*F0); 

    
*/
  }
};

// Frontened action to create the custom AST consumer
class CilkConvertAction : public clang::ASTFrontendAction {
public:
  CilkConvertAction(StringRef OutFilename): OutFilename(OutFilename) {}
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

  /*std::string extractFileName() {
    const clang::SourceManager &SM = getCompilerInstance().getSourceManager();

    const FileEntry *MainFileEntry = SM.getFileEntryForID(SM.getMainFileID());

    std::filesystem::path filePath(MainFileEntry->getName().str());
    std::string parentDir = filePath.parent_path().string();
    std::string name = filePath.stem().string();

    return (parentDir + "/" + name + "_cilk.cpp");
  }*/

  void EndSourceFileAction() override {
    /*
    clang::ASTContext &Context = getCompilerInstance().getASTContext();
    std::error_code EC;
    std::string outFilename = extractFileName();
    llvm::raw_fd_ostream outFile(outFilename, EC, llvm::sys::fs::OF_None);

    TheRewriter.getEditBuffer(Context.getSourceManager().getMainFileID())
        .write(outFile);
    outFile.close();
    */
  }
  private: 
  StringRef OutFilename;
};

int main(int argc, const char **argv) {
  if (argc < 3) {
    std::cerr << "Expected path to input OpenCilk (C++) file and output file." << std::endl;
    return 1;
  }

  // std::string inFilename = argv[1];
  // std::filesystem::path filePath(inFilename);
  // std::string parentDir = filePath.parent_path().string();
  // std::string name = filePath.stem().string();
  // std::string outFilename = parentDir + "/" + name + ".json";

  std::vector<std::string> compilationFlags = {
      "/opt/OpenCilk/bin/clang",
      "-c",
      "-w",
      "-fopencilk",
      "-O3",
      "-fsyntax-only",
      "-I/opt/OpenCilk/include",
      "-I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"};

  compilationFlags.push_back(argv[1]);

  std::shared_ptr<clang::PCHContainerOperations> PCHContainerOps =
      std::make_shared<clang::PCHContainerOperations>();

  clang::FileSystemOptions FSOpts;
  llvm::IntrusiveRefCntPtr<clang::FileManager> Files(
      new clang::FileManager(FSOpts));

  clang::tooling::ToolInvocation invocation(
      compilationFlags, std::make_unique<CilkConvertAction>(argv[2]), Files.get(),
      PCHContainerOps);

  return !invocation.run();
}