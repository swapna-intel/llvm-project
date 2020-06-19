#include "llvm/ADT/StringRef.h"

#include <memory>
#include <string>

namespace wille {

struct Location {
  std::shared_ptr<std::string> file;
  int line;
  int col;
};

// List of identified Tokens
enum Token : int {
  tok_semicolon = ';',
  tok_paren_open = '(',
  tok_paren_close = ')',
  tok_bracket_open = '{',
  tok_bracket_close = '}',
  tok_sqr_bracket_open = '[',
  tok_sqr_bracket_close = ']',

  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_var = -3,
  tok_return = -4,

  // primary
  tok_number = -5,
  tok_identifier = -6,

};

 class Lexer {
 public:
 Lexer(std::string filename)
   : lastLocation(
		  {std::make_shared<std::string>(std::move(filename)), 0, 0}) {}
   virtual ~Lexer() = default;

   /// Look at curr token in stream.
   Token getCurToken() { return curTok; }
   /// Move to next token in stream and return it.
   Token getNextToken() { return curTok = getTok(); }
   /// Move to the next token in stream, asserting on the curr token
   /// to macth the expectation.
   void consume(Token tok) {
     assert(tok == curTok && "consume: Token mismatch");
     getNextToken();
   }
   /// Return the current number (prereq: getCurToken() == tok_number)
   double getValue() {
     assert(curTok == tok_number);
     return numVal;
   }
   /// Return the current identifier (prereq: getCurToken() == tok_identifer)
   llvm::StringRef getId() {
     assert(curTok == tok_identifier);
     return identifierStr;
   }
   /// Return the location for the beginning of the current token.
   Location getLastLocation() { return lastLocation; }
   // Return curr line in file.
   int getLine() { return curLineNum; }
   // Return curr col in file.
   int getCol() { return curCol; }

 private:
   /// Delegate to a derived class fetching the next line. Returns an empty
   /// string to signal end of file (EOF). Lines are expected to always finish
   /// with "\n"
   virtual llvm::StringRef readNextLine() = 0;
   /// Return the next character from the stream. This manages the buffer for the
   /// current line and request the next line buffer to the derived class as
   /// needed.
   int getNextChar() {
     // The current line buffer should not be empty unless it's eof.
     if (curLineBuffer.empty()) return EOF;
     ++curCol;
     auto nextchar = curLineBuffer.front();
     curLineBuffer = curLineBuffer.drop_front();
     if (curLineBuffer.empty()) curLineBuffer = readNextLine();
     if (nextchar == '\n') {
       ++curLineNum;
       curCol = 0;
     }
     return nextchar;
   }
   /// Return next token from stdard input.
   Token getTok() {
     // skip whitespaces.
     while (isspace(lastChar))
       lastChar = Token(getNextChar());

     // Save the current location before reading the token characters.
     lastLocation.line = curLineNum;
     lastLocation.col = curCol;

     // Identifier: [a-zA-Z][a-zA-Z0-9_]*
     if (isalpha(lastChar)) {
       identifierStr = (char)lastChar;
       while (isalnum((lastChar = Token(getNextChar()))) || lastChar == '_')
	 identifierStr += (char)lastChar;

       if (identifierStr == "return") return tok_return;
       if (identifierStr == "def") return tok_def;
       if (identifierStr == "var") return tok_var;
       return tok_identifier;
     }
     // Number: [0-9.]+
     if (isdigit(lastChar) || lastChar == '.') {
       std:: string numStr;
       do {
	 numStr += lastChar;
	 lastChar = Token(getNextChar());
       }while (isdigit(lastChar) || lastChar == '.');

       numVal = strtod(numStr.c_str(), nullptr);
       return tok_number;
     }

     // Comment until end of line.
     if (lastChar == '#') {
       do {
	 lastChar = Token(getNextChar());
       } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

       if (lastChar != EOF) return getTok();
     }
     // Check for eof. Do not eat the eof.
     if (lastChar == EOF) return tok_eof;
     // Otherwise, just return the character as it's ascii value.
     Token thisChar = Token(lastChar);
     lastChar = Token(getNextChar());
     return thisChar;
   }
   /// The last token read from the input.
   Token curTok = tok_eof;
   /// Location for 'curTok'.
   Location lastLocation;

   /// If the current Token is an identifier, this string contains the value.
   std::string identifierStr;
   /// If the current token is a number, this contains the value.
   double numVal = 0;
   /// The last value returned by getNextChar(). We need to keep it around as we
   /// always need to read ahead one char to decide when to end a token and we
   /// cant put it back in the stream after reading from it.
   Token lastChar = Token(' ');
   /// Keep track of the current line number in the input stream.
   int curLineNum = 0;
   /// Keep track of current column number in the input stream.
   int curCol = 0;

   /// Buffer supplied by the derived class on calls to `readNextLine()`
   llvm::StringRef curLineBuffer = "\n";
   
};

 class LexerBuffer final : public Lexer {
 public:
   LexerBuffer(const char *begin, const char *end, std::string filename)
     : Lexer(std::move(filename)), current(begin), end(end) {}

 private:
   /// Provide one line at a time to the Lexer, return an empty string when
   /// reaching the end of the buffer.
   llvm::StringRef readNextLine() override {
     auto *begin = current;
     while (*current && current <= end && *current != '\n') ++current;
     if (*current && current <= end) ++current;
     llvm::StringRef result{begin, static_cast<size_t>(current - begin)};
     return result;
   }
   const char *current, *end;
 };
 
} // namespace wille

