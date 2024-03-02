use std::str::Chars;
use std::str::FromStr;
use std::fs::File;
use std::io::{BufRead,BufReader};
use std::iter::Peekable;
use std::process;
use std::fmt;
use std::collections::HashMap;

pub struct Scanner {
	file_path: String,
	f: BufReader<File>,
	line: usize,
	pub tokens: Vec<Token>,
	keywords: HashMap<String, TokenType>,
}

impl Scanner {
	pub fn new(file_path: &String) -> Self {
		let f = BufReader::new(File::open(file_path).expect("Couldn't read file"));
		let mut keywords = HashMap::new();
		let k = [TokenType::Def, TokenType::For, TokenType::If, TokenType::Else,
			TokenType::While, TokenType::Null, TokenType::Return, TokenType::Var,
			TokenType::I32, TokenType::I64, TokenType::F32, TokenType::F64, TokenType::Bool,
			TokenType::True, TokenType::False];
		for t in k.into_iter() {
			keywords.insert(t.to_string(), t);
		}
		let s = Self { file_path: file_path.clone(), f: f, line: 0, tokens: Vec::new(), keywords};
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
			':' => self.add_token(TokenType::Colon),
			',' => self.add_token(TokenType::Comma),
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
			c @ 'a'..='z' | c @ 'A'..='Z' | c @ '_' => self.scan_identifier(chars, c),
			_ => {print!("{}", c); self.error("Unexpected token")}
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

	fn scan_identifier(&mut self, chars: &mut Peekable<Chars<'_>>, c: char) {
		let mut string = String::new();
		string.push(c);
		while let Some(c @ 'a'..='z' | c @ 'A'..='Z' | c @ '_' | c @ '0'..='9' ) = chars.peek() {
			string.push(*c);
			chars.next();
		}
		match self.keywords.get(&string) {
			Some(t) =>	self.add_token(t.clone()),
			_ => self.add_token(TokenType::Identifier(string)),
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
			print!("{} ", token.token);
		}
		println!("");
	}
}

#[derive(Clone)]
#[derive(Debug)]
pub enum TokenType {
  // Single-character tokens.
  LeftParen, RightParen, LeftBrace, RightBrace,
  Minus, Plus, Slash, Star, Semicolon, Colon, Comma,

  // One or two character tokens.
  Not, NotEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  String(String), Integer(u64), Float(f64), True, False,

	Identifier(String),

  // Keywords.
  Def, For, If, Else, While, Null,
  Return, Var,
	I32, I64, F32, F64, Bool,
}
#[derive(Debug)]
pub struct Token {
	pub token: TokenType,
	pub line: usize,
}

impl fmt::Display for TokenType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self {
			TokenType::LeftParen => write!(f, "("),
			TokenType::RightParen => write!(f, ")"),
			TokenType::LeftBrace => write!(f, "{{"),
			TokenType::RightBrace => write!(f, "}}"),
			TokenType::Minus => write!(f, "-"),
			TokenType::Plus => write!(f, "+"),
			TokenType::Slash => write!(f, "/"),
			TokenType::Star => write!(f, "*"),
			TokenType::Semicolon => write!(f, ";"),
			TokenType::Colon => write!(f, ":"),
			TokenType::Comma => write!(f, ","),
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
			TokenType::True => write!(f, "true"),
			TokenType::False => write!(f, "false"),
			TokenType::Identifier(s) => write!(f, "Identifier<{}>", s),
			TokenType::Def => write!(f, "def"),
			TokenType::For => write!(f, "for"),
			TokenType::If => write!(f, "if"),
			TokenType::Else => write!(f, "else"),
			TokenType::While => write!(f, "while"),
			TokenType::Null => write!(f, "null"),
			TokenType::Return => write!(f, "return"),
			TokenType::Var => write!(f, "var"),
			TokenType::I32 => write!(f, "i32"),
			TokenType::I64 => write!(f, "i64"),
			TokenType::F32 => write!(f, "f32"),
			TokenType::F64 => write!(f, "f64"),
			TokenType::Bool => write!(f, "bool"),
			_ => write!(f, "."),
		}
	}
}
