use std::str::Chars;
use std::fs::File;
use std::io::{BufRead,BufReader};
use std::iter::Peekable;

pub struct Scanner {
	file_path: String,
	f: BufReader<File>,
	line: usize,
	tokens: Vec<Token>,
}

impl Scanner {
	pub fn new(file_path: &String) -> Self {
		let f = BufReader::new(File::open(file_path).expect("Couldn't read file"));
		let s = Self { file_path: file_path.clone(), f: f, line: 1, tokens: Vec::new()};
		s
	}
	pub fn scan_tokens(&mut self) {
		let mut line = String::new();
		while self.f.read_line(&mut line).expect("Couldn't get line {self.line}") > 0 {
			self.line += 1;
			let mut chars = line.chars().peekable();
			while !self.chars_at_end(&mut chars) {
				self.scan_token(&mut chars);
			}
			line.clear();
		}
	}

	fn chars_at_end(&mut self, chars: &mut Peekable<Chars<'_>>) -> bool {
		chars.peek().is_none()
	}

	fn scan_token(&mut self, chars: &mut Peekable<Chars<'_>>) {
		let c = chars.next();
		match c {
			Some('(') => self.add_token(TokenType::LeftParen),
			Some(')') => self.add_token(TokenType::RightParen),
			_ => println!("ignore now"),
		}
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