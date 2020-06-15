#include <iostream>
#include "Lexer.h"
//using namespace wille


#include "llvm/Support/CommandLine.h"

namespace cl = llvm::cl;

static cl::opt::<std::string> inputFilename(cl::Positional, cl::desc("<input wille file>"),
					    cl::init("_"),
					    cl::value_desc("filename"));

namespace {
  enum Action { None, DumpTokens, DumpAST };
}

static cl::opt<enum Action>
emitAction("emit", cl::desc("Select the kind of output desired"),
	   cl::values(clEnumValN(DumbTokens, "token", "output the Lexed Tokens")),
	   cl::values(clEnumValN(DumbAST, "ast", "output the AST dump")));





int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv, "willie compiler\n");

  std::cout << "ParseCommandLine done!" << std::endl;

  return 0;
}
