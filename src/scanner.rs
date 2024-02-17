#[derive(Default)]
pub struct Scanner<'a> {
	source: &'a [u8],
	current: usize,
	line: usize,
	tokens: Vec<Token>,
}

impl <'a>Scanner<'a> {
	pub fn new(s: &'a [u8]) -> Self {
		Self { source: s, current: 0, line: 1, tokens: Vec::new() }
	}
	pub fn scan_tokens(&mut self) {
		while !self.at_end() {
			self.scan_token();
		}
	}

	fn at_end(&mut self) -> bool {
		self.current >= self.source.len()
	}

	fn scan_token(&mut self) {
		let c = self.advance();
		match c {
			b'(' => self.add_token(TokenType::LeftParen),
			b')' => self.add_token(TokenType::RightParen),
			b'\n' => self.line += 1,
			_ => println!("ignore now"),
		}
	}

	fn advance(&mut self) -> u8 {
		let tmp = self.source[self.current];
		self.current += 1;
		tmp
	}

	fn add_token(&mut self, t: TokenType) {
		self.tokens.push(Token { token: t, line: self.line });
	}

	pub fn print_token(&self) {
		for token in self.tokens.iter() {
			match token.token {
				TokenType::LeftParen => println!("("),
				TokenType::RightParen => println!(")"),
				_ => println!("."),
			}
		}
	}
}

enum TokenType {
  // Single-character tokens.
  LeftParen, RightParen, //LEFT_BRACE, RIGHT_BRACE,
  //MINUS, PLUS, SLASH, STAR, SEMICOLON,

  // One or two character tokens.
  //NOT, NOT_EQUAL,
  //EQUAL, EQUAL_EQUAL,
  //GREATER, GREATER_EQUAL,
  //LESS, LESS_EQUAL,

  // Literals.
  //STRING(String), INTEGER(i64), FLOAT(f64), TRUE, FALSE,

	//IDENTIFIER(String),

  // Keywords.
  //DEF, FOR, IF, ELSE, WHILE, NIL,
  //RETURN, VAR,
	//AND, OR,
}

struct Token {
	token: TokenType,
	line: usize,
}