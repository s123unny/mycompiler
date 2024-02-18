use std::str::Chars;
use std::str::FromStr;
use std::fs::File;
use std::io::{BufRead,BufReader};
use std::iter::Peekable;
use std::process;
use std::fmt;

pub struct Scanner {
	file_path: String,
	f: BufReader<File>,
	line: usize,
	tokens: Vec<Token>,
}

impl Scanner {
	pub fn new(file_path: &String) -> Self {
		let f = BufReader::new(File::open(file_path).expect("Couldn't read file"));
		let s = Self { file_path: file_path.clone(), f: f, line: 0, tokens: Vec::new()};
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
		chars.peek().is_none() || chars.peek() == Some(&'\n')
	}

	fn scan_token(&mut self, chars: &mut Peekable<Chars<'_>>) {
		let Some(c) = chars.next() else {
			panic!("Application error: Can't scan token");
		};
		match c {
			'(' => self.add_token(TokenType::LeftParen),
			')' => self.add_token(TokenType::RightParen),
			'{' => self.add_token(TokenType::LeftBrace),
			'}' => self.add_token(TokenType::RightBrace),
			'-' => self.add_token(TokenType::Minus),
			'+' => self.add_token(TokenType::Plus),
			'/' => self.add_token(TokenType::Slash),
			'*' => self.add_token(TokenType::Star),
			';' => self.add_token(TokenType::Semicolon),
			'!' => {
				let token = if self.expect(chars, '=') { TokenType::NotEqual } else { TokenType::Not };
				self.add_token(token);
			},
			'=' => {
				let token = if self.expect(chars, '=') { TokenType::EqualEqual } else { TokenType::Equal };
				self.add_token(token);
			},
			'>' => {
				let token = if self.expect(chars, '=') { TokenType::GreaterEqual } else { TokenType::Greater };
				self.add_token(token);
			},
			'<' => {
				let token = if self.expect(chars, '=') { TokenType::LessEqual } else { TokenType::Less };
				self.add_token(token);
			},
			' ' | '\t' | '\r' => {},
			'"' => self.scan_string(chars),
			n @ '0'..='9' => self.scan_number(chars, n),
			_ => println!("todo {:?}", c),
		}
	}

	fn add_token(&mut self, t: TokenType) {
		self.tokens.push(Token { token: t, line: self.line });
	}

	fn expect(&mut self, chars: &mut Peekable<Chars<'_>>, expected: char) -> bool {
		if self.chars_at_end(chars) || chars.peek() != Some(&expected) {
			return false;
		}
		chars.next();
		true
	}

	fn scan_string(&mut self, chars: &mut Peekable<Chars<'_>>) {
		let mut string = String::new();
		while let Some(c) = chars.next() {
			if c == '"' {
				break;
			} else {
				string.push(c)
			}
		}
		if self.chars_at_end(chars) {
			self.error("Unterminated string")
		} else {
			self.add_token(TokenType::String(string));
		}
	}

	fn scan_number(&mut self, chars: &mut Peekable<Chars<'_>>, c: char) {
		let mut string = String::new();
		let mut base = 10;
		if c == '0' && chars.peek() == Some(&'x') {
			chars.next();
			base = 16;
		} else {
			string.push(c);
		}
		let mut is_float = false;
		while let Some(c) = chars.peek() {
			match c {
				c @ '0'..='9'| c @ 'a'..='f'| c @ 'A'..='F' => string.push(*c),
				c @ '.' => {
					if is_float || base == 16 {
						self.error("Unrecognized number literal");
					} else {
						is_float = true;
						string.push(*c);
					}
				},
				_ => break,
			}
			chars.next();
		}
		if is_float {
			let Ok(number) = f64::from_str(&string) else {
				self.error("Unrecognized number literal");
				return;
			};
			self.add_token(TokenType::Float(number));
		} else {
			let Ok(number) = u64::from_str_radix(&string, base) else {
				self.error("Unrecognized number literal");
				return;
			};
			self.add_token(TokenType::Integer(number));
		}
	}

	fn error(&self, m: &str) {
		println!("{}:{} Error: {}", self.file_path, self.line, m);
		process::exit(1);
	}

	pub fn print_tokens(&self) {
		let mut line = 0;
		for token in self.tokens.iter() {
			if token.line > line {
				line = token.line;
				print!("\n{}: ", line);
			}
			print!("{} ", token);
		}
		println!("");
	}
}

enum TokenType {
  // Single-character tokens.
  LeftParen, RightParen, LeftBrace, RightBrace,
  Minus, Plus, Slash, Star, Semicolon,

  // One or two character tokens.
  Not, NotEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  String(String), Integer(u64), Float(f64), True, False,

	Identifier(String),

  // Keywords.
  Def, For, If, Else, While, Nil,
  Return, Var,
	I32, I64, F32, F64, Bool,
}

struct Token {
	token: TokenType,
	line: usize,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self.token {
			TokenType::LeftParen => write!(f, "("),
			TokenType::RightParen => write!(f, ")"),
			TokenType::LeftBrace => write!(f, "{{"),
			TokenType::RightBrace => write!(f, "}}"),
			TokenType::Minus => write!(f, "-"),
			TokenType::Plus => write!(f, "+"),
			TokenType::Not => write!(f, "!"),
			TokenType::NotEqual => write!(f, "!="),
			TokenType::Equal => write!(f, "="),
			TokenType::EqualEqual => write!(f, "=="),
			TokenType::Greater => write!(f, ">"),
			TokenType::GreaterEqual => write!(f, ">="),
			TokenType::Less => write!(f, "<"),
			TokenType::LessEqual => write!(f, "<="),
			TokenType::String(s) => write!(f, "String<{}>", s),
			TokenType::Integer(n) => write!(f, "Integer<{}>", n),
			TokenType::Float(n) => write!(f, "Float<{}>", n),
			_ => write!(f, "."),
		}
	}
}
