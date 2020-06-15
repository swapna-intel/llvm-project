

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
