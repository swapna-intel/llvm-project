#include "AST.h"
//#include "Lexer.h"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <utility>
#include <vector>

namespace wille {

  /// This is a simple recursive descent parser for the Wille language totally borrowed from the
  /// original- Toy language. No semantic checks or symbol resolution is performed. All faults
  /// and all greatness is merely inherited except for code to make wille specific parsing work.
  class Parser {

  public:
    /// Create a Parser for the supplied lexer.
  Parser(Lexer &lexer) : lexer(lexer) {}
    //Parse a full Module. A Module is a list of function definitions.
    std::unique_ptr<ModuleAST> parseModule() {
        lexer.getNextToken(); // prime the lexer

    // Parse functions one at a time and accumulate in this vector.
    std::vector<FunctionAST> functions;
      while (auto f = parseDefinition()) {
	functions.push_back(std::move(*f));
	if (lexer.getCurToken() == tok_eof) break;
      }
      // If we did not reach eof, there was an error while parsing
      if (lexer.getCurToken() != tok_eof) return parseError<ModuleAST>("nothing at end of module");

      return std::make_unique<ModuleAST>(std::move(functions));
    }

  private:
    Lexer &lexer;

    /// Parse a return statement.
    /// return := return ; | return expr :
    std::unique_ptr<ReturnExprAST> parseReturn() {
      auto loc = lexer.getLastLocation();
      lexer.consume(tok_return);
      // return takes an optional argument
      llvm::Optional<std::unique_ptr<ExprAST>> expr;
      if (lexer.getCurToken() != ';') {
	expr = parseExpression();
	if (!expr) return nullptr;
      }
      return std::make_unique<ReturnExprAST>(std::move(loc), std::move(expr));
    }
    
    /// Parse a literal number.
    /// numberexpr ::= number.
    std::unique_ptr<ExprAST> parseNumberExpr() {
      auto loc = lexer.getLastLocation();
      auto result =
	std::make_unique<NumberExprAST>(std::move(loc), lexer.getValue());
      lexer.consume(tok_number);
      return std::move(result);
    }
    
    /// Parse a literal array expression.
    /// tensorLiteral ::= [ literalList ] | number
    /// literalList ::= tensorLiteral 
    std::unique_ptr<ExprAST> parseTensorLiteralExpr() {
      auto loc = lexer.getLastLocation();
      lexer.consume(Token('['));

      // Hold the list of numeric values at this one and only level.
      std::vector<std::unique_ptr<ExprAST>> values;
      do {
	if (lexer.getCurToken()  == tok_number)
	  values.push_back(parseNumberExpr());

	// End of list on ']'
	if (lexer.getCurToken() == ']') break;

	// Elements are seperated by comma
	if (lexer.getCurToken() != ',')
	  return parseError<ExprAST>("] or ,", "in literal expression");
	
	lexer.getNextToken(); // eat
      } while (true);

      if (values.empty())
	return parseError<ExprAST>("<something>", "to fill literal expression");
      lexer.getNextToken(); // eat ]

      /// Set dimension now
      int64_t dim = values.size();
      return std::make_unique<LiteralExprAST>(std::move(loc), std::move(values),
					      std::move(dim));
    }

    /// parenexpr ::= '(' expression ')'
    std::unique_ptr<ExprAST> parseParenExpr() {
      lexer.getNextToken(); // eat (.
      auto v = parseExpression();
      if (!v)
	return nullptr;
      
      if (lexer.getCurToken() != ')')
	return parseError<ExprAST>(")", "to close expression with parentheses");
      lexer.consume(Token(')'));
      return v;
    }
    
    /// identifierexpr
    /// ::= identifier
    /// ::= identifier '(' expression ')'
    std::unique_ptr<ExprAST> parseIdentifierExpr() {
      std::string name(lexer.getId());

      auto loc = lexer.getLastLocation();
      lexer.getNextToken(); // eat identifier.

      if (lexer.getCurToken() != '(') // Simple variable ref.
	return std::make_unique<VariableExprAST>(std::move(loc), name);

      // This is a function call.
      lexer.consume(Token('('));
      std::vector<std::unique_ptr<ExprAST>> args;
      if (lexer.getCurToken() != ')') {
	while (true) {
	  if (auto arg = parseExpression())
	    args.push_back(std::move(arg));
	  else
	    return nullptr;

	  if (lexer.getCurToken() == ')') break;
	  if (lexer.getCurToken() != ',')
	    return parseError<ExprAST>(", or )", "in argument list");
	  lexer.getNextToken();
	}
      }
      lexer.consume(Token(')'));

      // It can be a builtin call to print
      if (name == "print") {
	if (args.size() != 1)
	  return parseError<ExprAST>("single arg>", "as argument to print()");

	return std::make_unique<PrintExprAST>(std::move(loc), std::move(args[0]));
      }
      // Call to a user-defined function
      return std::make_unique<CallExprAST>(std::move(loc), name, std::move(args));
    }
    /// primary
    ///   ::= identifierexpr
    ///   ::= numberexpr
    ///   ::= parenexpr
    ///   ::= tensorliteral
    std::unique_ptr<ExprAST> parsePrimary() {
      switch (lexer.getCurToken()) {
      default:
	llvm::errs() << "unknown token '" << lexer.getCurToken()
		     << "' when expecting an expression\n";
	return nullptr;
      case tok_identifier:
	return parseIdentifierExpr();
      case tok_number:
	return parseNumberExpr();
      case '(':
	return parseParenExpr();
      case '[':
	return parseTensorLiteralExpr();
      case ';':
	return nullptr;
      case '}':
	return nullptr;
      }
    }

    /// Recursively parse the right hand of a binary expression, the ExprPrec
    /// argument indicates the precedence of the current binary operator.
    ///
    /// binoprhs ::= ('+' primary)*
    std::unique_ptr<ExprAST> parseBinOpRHS(int exprPrec,
					   std::unique_ptr<ExprAST> lhs) {
      // If this is a binop, find it's precedence.
      while (true) {
	int tokPrec = getTokPrecedence();

	// If this is a binop that binds at least as tightly as the current binop,
	// consume it, otherwise we are done.
	if (tokPrec < exprPrec)
	  return lhs;

	// Okay, we know this is a binop.
	int binOp = lexer.getCurToken();
	lexer.consume(Token(binOp));
	auto loc = lexer.getLastLocation();

	// Parse the primary expression after the binary operator.
	auto rhs = parsePrimary();
	 if (!rhs)
        return parseError<ExprAST>("expression", "to complete binary operator");

      // If BinOp binds less tightly with rhs than the operator after rhs, let
      // the pending operator take rhs as its lhs.
      int nextPrec = getTokPrecedence();
      if (tokPrec < nextPrec) {
        rhs = parseBinOpRHS(tokPrec + 1, std::move(rhs));
        if (!rhs)
          return nullptr;
      }

      // Merge lhs/RHS.
      lhs = std::make_unique<BinaryExprAST>(std::move(loc), binOp,
                                            std::move(lhs), std::move(rhs));

      }
    }
    /// expression::= primary binop rhs
    std::unique_ptr<ExprAST> parseExpression() {
      auto lhs = parsePrimary();
      if (!lhs)
	return nullptr;
      
      return parseBinOpRHS(0, std::move(lhs));
    }
    
    /// Parse a variable declaration, it starts with a `var` keyword followed by
    /// an identifier.
    /// decl ::= var identifier = expr
    std::unique_ptr<VarDeclExprAST> parseDeclaration() {
      if (lexer.getCurToken() != tok_var)
	return parseError<VarDeclExprAST>("var", "to begin declaration");
      auto loc = lexer.getLastLocation();
      lexer.getNextToken(); // eat var
      
      if (lexer.getCurToken() != tok_identifier)
	return parseError<VarDeclExprAST>("identified",
					  "after 'var' declaration");
      std::string id(lexer.getId());
      lexer.getNextToken(); // eat id
      
      lexer.consume(Token('='));
      auto expr = parseExpression();
      return std::make_unique<VarDeclExprAST>(std::move(loc), std::move(id),
					      std::move(expr));
    }
    
    /// Parse a block: a list of expression separated by semicolons and wrapped in
    /// curly braces.
    ///
    /// block ::= { expression_list }
    /// expression_list ::= block_expr ; expression_list
    /// block_expr ::= decl | "return" | expr
    std::unique_ptr<ExprASTList> parseBlock() {
      if (lexer.getCurToken() != '{')
	return parseError<ExprASTList>("{", "to begin block");
      lexer.consume(Token('{'));
      
      auto exprList = std::make_unique<ExprASTList>();
      
      // Ignore empty expressions: swallow sequences of semicolons.
      while (lexer.getCurToken() == ';')
	lexer.consume(Token(';'));
      
      while (lexer.getCurToken() != '}' && lexer.getCurToken() != tok_eof) {
	if (lexer.getCurToken() == tok_var) {
	  // Variable declaration
	  auto varDecl = parseDeclaration();
	  if (!varDecl)
	    return nullptr;
	  exprList->push_back(std::move(varDecl));
	} else if (lexer.getCurToken() == tok_return) {
	  // Return statement
	  auto ret = parseReturn();
	  if (!ret)
	    return nullptr;
	  exprList->push_back(std::move(ret));
	} else {
	  // General expression
	  auto expr = parseExpression();
	  if (!expr)
	    return nullptr;
	  exprList->push_back(std::move(expr));
	}
	// Ensure that elements are separated by a semicolon.
	if (lexer.getCurToken() != ';')
	  return parseError<ExprASTList>(";", "after expression");
	
	// Ignore empty expressions: swallow sequences of semicolons.
	while (lexer.getCurToken() == ';')
	  lexer.consume(Token(';'));
      }
      
      if (lexer.getCurToken() != '}')
	return parseError<ExprASTList>("}", "to close block");
      
      lexer.consume(Token('}'));
      return exprList;
    }
    /// prototype ::= def id '(' decl_list ')'
    /// decl_list ::= identifier | identifier, decl_list
    std::unique_ptr<PrototypeAST> parsePrototype() {
      auto loc = lexer.getLastLocation();
      
      if (lexer.getCurToken() != tok_def)
	return parseError<PrototypeAST>("def", "in prototype");
      lexer.consume(tok_def);
      
      if (lexer.getCurToken() != tok_identifier)
	return parseError<PrototypeAST>("function name", "in prototype");
      
      std::string fnName(lexer.getId());
      lexer.consume(tok_identifier);
      
      if (lexer.getCurToken() != '(')
	return parseError<PrototypeAST>("(", "in prototype");
      lexer.consume(Token('('));
      
      std::vector<std::unique_ptr<VariableExprAST>> args;
      if (lexer.getCurToken() != ')') {
	do {
	  std::string name(lexer.getId());
	  auto loc = lexer.getLastLocation();
	  lexer.consume(tok_identifier);
	  auto decl = std::make_unique<VariableExprAST>(std::move(loc), name);
	  args.push_back(std::move(decl));
	  if (lexer.getCurToken() != ',')
	    break;
	  lexer.consume(Token(','));
	  if (lexer.getCurToken() != tok_identifier)
	    return parseError<PrototypeAST>(
					    "identifier", "after ',' in function parameter list");
	} while (true);
      }
      if (lexer.getCurToken() != ')')
	return parseError<PrototypeAST>(")", "to end function prototype");
      
      // success.
      lexer.consume(Token(')'));
      return std::make_unique<PrototypeAST>(std::move(loc), fnName,
					    std::move(args));
    }
    /// Parse a function definition, we expect a prototype initiated with the
    /// `def` keyword, followed by a block containing a list of expressions.
    ///
    /// definition ::= prototype block
    std::unique_ptr<FunctionAST> parseDefinition() {
      auto proto = parsePrototype();
      if (!proto)
	return nullptr;
      
      if (auto block = parseBlock())
	return std::make_unique<FunctionAST>(std::move(proto), std::move(block));
      return nullptr;
    }
    
    /// Get the precedence of the pending binary operator token.
    int getTokPrecedence() {
      if (!isascii(lexer.getCurToken()))
	return -1;
      
      // 1 is lowest precedence.
      switch (static_cast<char>(lexer.getCurToken())) {
      case '-':
	return 20;
      case '+':
	return 20;
      case '*':
	return 40;
      default:
	return -1;
      }
    }
    
    /// Helper function to signal errors while parsing, it takes an argument
    /// indicating the expected token and another argument giving more context.
    /// Location is retrieved from the lexer to enrich the error message.
    template <typename R, typename T, typename U = const char *>
      std::unique_ptr<R> parseError(T &&expected, U &&context = "") {
      auto curToken = lexer.getCurToken();
      llvm::errs() << "Parse error (" << lexer.getLastLocation().line << ", "
      << lexer.getLastLocation().col << "): expected '" << expected
      << "' " << context << " but has Token " << curToken;
      if (isprint(curToken))
	llvm::errs() << " '" << (char)curToken << "'";
      llvm::errs() << "\n";
      return nullptr;
    }

  };
  
  
} // namespace wille
