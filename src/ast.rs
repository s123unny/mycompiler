use crate::scanner::{Token,TokenType};
use crate::ast::ParsingResult::{Good,Bad};
use std::fmt;
use std::process;

#[derive(Debug)]
pub enum Expression {
	LiteralExpr(Token),
	VaraibleExpr(Token),
	BinaryExpr{left: Box<Expression>, operator: Token, right: Box<Expression>},
	UnaryExpr{operator: Token, right: Box<Expression>},
}
#[derive(Debug)]
pub enum Statement {
	ExprStmt(Expression),
	Block{stmts: Vec<Statement>},
	VarDecl{variable: Token, vtype: Option<TokenType>, expr: Option<Expression>},
	AssignStmt{variable: Token, operator: Token, expr: Expression},
	SelectionStmt{cond: Expression, if_stmt: Box<Statement>, else_stmt: Option<Box<Statement>>}
}

pub struct Function {
	pub prototype: Prototype,
  pub body: Statement
}

pub struct Prototype {
	pub name: String,
	pub args: Vec<String>,
	pub atypes: Vec<TokenType>,
}

pub enum AstNode {
	FunctionNode(Function),
}

pub fn parse_ast(tokens: &mut Vec<Token>) -> Vec<AstNode> {
	tokens.reverse();

	let mut ast: Vec<AstNode> = Vec::new();
	loop {
		let result = match tokens.last() {
			Some(Token{ token: TokenType::Def, .. }) => parse_function(tokens),
			None => break,
			_ => {tokens.pop(); continue}
		};
		match result {
			Good(function) => ast.push(AstNode::FunctionNode(function)),
			Bad => process::exit(1),
		}
	}
	ast
}

enum ParsingResult<T> {
	Good(T),
	Bad
}

fn error<T>(m: &str, found: Option<Token>) -> ParsingResult<T> {
	match found {
		Some(t) => println!("file:{}:{}: {}, found {}", t.source.line, t.source.col, m, t.token),
		None => println!("file: {}", m),
	}
	Bad
}

macro_rules! expect {
	($tokens: expr, $error:expr, [$($token:pat, $source:ident, $result:stmt);+]) => (
		match $tokens.pop() {
			$(
				Some(Token{ token:$token, source: $source }) => {
					let _ = $source; // To avoid unused variable warning
					$result
				},
			)+
			Some(y) => {
				println!("{:?}", y);
				return error($error, Some(y))
			}
			None => return error($error, None)
		}
	);
}

macro_rules! parse_try {
	($function:ident, $tokens:ident) => {
		match $function($tokens) {
			Good(ast) => ast,
			Bad => return Bad
		}
	}
}

macro_rules! peek {
	($tokens:expr, $($token:pat),+) => {
		match $tokens.last() {
			$(Some(Token{ token: $token, .. }) => true,)+
			_ => false
		}
	}
}

macro_rules! peek_next {
	($tokens:expr, $($token:pat),+) => {
		if $tokens.len() > 2 {
			match $tokens[$tokens.len()-2] {
				$(Token{ token: $token, .. } => true,)+
				_ => false
			}
		} else {
			false
		}
	}
}

fn parse_function(tokens: &mut Vec<Token>) -> ParsingResult<Function> {
	tokens.pop(); // pop Def token
	let prototype = parse_try!(parse_prototype, tokens);
	let body = parse_try!(parse_block, tokens);
	println!("{}", body);
	Good(Function{ prototype, body})
}

fn parse_prototype(tokens: &mut Vec<Token>) -> ParsingResult<Prototype> {
	let name = expect!(tokens, "expected function name in prototype", [TokenType::Identifier(name), source, name]);
	expect!(tokens, "expected '(' in prototype", [TokenType::LeftParen, source, true]);
	let mut args = Vec::new();
	let mut atypes = Vec::new();
	if peek!(tokens, TokenType::RightParen) {
		tokens.pop();
		return Good(Prototype{ name, args, atypes });
	}
	loop {
		expect!(tokens, "expected identifier in prototype", [
			TokenType::Identifier(arg), source, args.push(arg.clone())
		]);
		expect!(tokens, "expected ':' in prototype", [TokenType::Colon, source, {}]);
		expect!(tokens, "expected type in prototype", [
			TokenType::I32, source, atypes.push(TokenType::I32);
			TokenType::I64, source, atypes.push(TokenType::I64);
			TokenType::F32, source, atypes.push(TokenType::F32);
			TokenType::F64, source, atypes.push(TokenType::F64)
		]);
		expect!(tokens, "expected ')' in prototype", [
			TokenType::Comma, source, continue;
			TokenType::RightParen, source, break
		]);
	}
	Good(Prototype{ name, args, atypes })
}

fn parse_block(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	expect!(tokens, "expected '{' after prototype", [TokenType::LeftBrace, source, {}]);
	let mut stmts = Vec::new();
	while !peek!(tokens, TokenType::RightBrace) {
		let result = parse_try!(parse_stmt, tokens);
		stmts.push(result);
	}
	expect!(tokens, "expected '}' after prototype", [TokenType::RightBrace, source, {}]);
	Good( Statement::Block{stmts} )
}

fn parse_selection(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	expect!(tokens, "expected 'if'", [TokenType::If, source, {}]);
	let cond = parse_try!(parse_expr, tokens);
	let if_stmt = Box::new(parse_try!(parse_block, tokens));
	let else_stmt = if peek!(tokens, TokenType::Else) {
		expect!(tokens, "expected 'else'", [TokenType::Else, source, {}]);
		Some(Box::new(parse_try!(parse_block, tokens)))
	} else {
		None
	};
	Good( Statement::SelectionStmt{ cond, if_stmt, else_stmt } )
}

fn parse_var_decl(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	expect!(tokens, "expected 'var'", [TokenType::Var, source, {}]);
	let variable = expect!(tokens, "expected variable", [
		TokenType::Identifier(variable), source, Token{token: TokenType::Identifier(variable), source}
	]);
	let vtype = if peek!(tokens, TokenType::Colon) {
		expect!(tokens, "expected ':'", [TokenType::Colon, source, {}]);
		let v = expect!(tokens, "expected type after ':", [
			TokenType::I32, source, TokenType::I32;
			TokenType::I64, source, TokenType::I64;
			TokenType::F32, source, TokenType::F32;
			TokenType::F64, source, TokenType::F64
		]);
		Some(v)
	} else {
		None
	};
	let expr = if peek!(tokens, TokenType::Equal) {
		expect!(tokens, "expected '='", [TokenType::Equal, source, {}]);
		let e = parse_try!(parse_expr, tokens);
		Some(e)
	} else {
		None
	};
	expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, source, {}]);
	Good( Statement::VarDecl{ variable, vtype, expr } )
}

fn parse_stmt(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	let stmt = match tokens.last() {
		Some(Token{ token: TokenType::LeftBrace, .. }) => parse_try!(parse_block, tokens),
		Some(Token{ token: TokenType::If, .. }) => parse_try!(parse_selection, tokens),
		Some(Token{ token: TokenType::Var, .. }) => parse_try!(parse_var_decl, tokens),
		Some(Token{ token: TokenType::Identifier(_), .. }) => {
			if peek_next!(tokens, TokenType::Equal) {
				let variable = tokens.pop().expect("expected token");
				let operator = tokens.pop().expect("expected token");
				let expr = parse_try!(parse_expr, tokens);
				expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, source, {}]);
				Statement::AssignStmt{ variable, operator, expr }
			} else {
				let expr = parse_try!(parse_expr, tokens);
				expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, source, {}]);
				Statement::ExprStmt(expr)
			}
		}
		_ => {
			let expr = parse_try!(parse_expr, tokens);
			expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, source, {}]);
			Statement::ExprStmt(expr)
		}
	};
	Good(stmt)
}

fn parse_expr(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	return parse_equality(tokens);
}

macro_rules! parse_expr_stage {
	($next_fn:ident, $tokens:ident, $($op:pat),+) => {
		let mut expr = parse_try!($next_fn, $tokens);

		while peek!($tokens, $($op),+) {
			let operator = $tokens.pop().expect("expected token");
			let right = parse_try!($next_fn, $tokens);
			expr = Expression::BinaryExpr{left: Box::new(expr), operator, right: Box::new(right)};
		}
	
		return Good(expr);		
	}
}

fn parse_equality(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	parse_expr_stage!(parse_comparison, tokens, TokenType::NotEqual, TokenType::EqualEqual);
}

fn parse_comparison(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	parse_expr_stage!(parse_term, tokens, TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual);
}

fn parse_term(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	parse_expr_stage!(parse_factor, tokens, TokenType::Plus, TokenType::Minus);
}

fn parse_factor(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	parse_expr_stage!(parse_unary, tokens, TokenType::Slash, TokenType::Star);
}

fn parse_unary(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	if peek!(tokens, TokenType::Not, TokenType::Minus) {
		let operator = tokens.pop().expect("expected token");
		let right = parse_try!(parse_unary, tokens);
		return Good(Expression::UnaryExpr{operator, right: Box::new(right)});
	}

	return Good(parse_try!(parse_primary, tokens));
}

fn parse_primary(tokens: &mut Vec<Token>) -> ParsingResult<Expression> {
	let expr = expect!(tokens, "expected expression", [
		TokenType::String(t), source, Expression::LiteralExpr(Token{token: TokenType::String(t), source});
		TokenType::Integer(t), source, Expression::LiteralExpr(Token{token: TokenType::Integer(t), source});
		TokenType::Float(t), source, Expression::LiteralExpr(Token{token: TokenType::Float(t), source});
		TokenType::True, source, Expression::LiteralExpr(Token{token: TokenType::True, source});
		TokenType::False, source, Expression::LiteralExpr(Token{token: TokenType::False, source});
		TokenType::Null, source, Expression::LiteralExpr(Token{token: TokenType::Null, source});
		TokenType::Identifier(t), source, Expression::VaraibleExpr(Token{token: TokenType::Identifier(t), source});
		TokenType::LeftBrace, source, {
			let expr = parse_try!(parse_expr, tokens);
			expect!(tokens, "expected ')' after expression", [TokenType::RightBrace, source, {}]);
			expr
		}
	]);
	println!("primary {:?}", expr);
	Good(expr)
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self {
			Expression::LiteralExpr(t) | Expression::VaraibleExpr(t) => write!(f, "{}", t.token),
			Expression::BinaryExpr{left, operator, right} => write!(f, "({} {} {})", operator.token, left, right),
			Expression::UnaryExpr{operator, right} => write!(f, "({} {})", operator.token, right),
		}
	}
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self {
			Statement::ExprStmt(e) => write!(f, "{e}"),
			Statement::Block{stmts} => {
				write!(f, "{{\n")?;
				for s in stmts {
					write!(f, "{s}")?;
				}
				write!(f, "}}\n")
			},
			Statement::VarDecl{variable, vtype, expr} => {
				write!(f, "{}", variable.token)?;
				if let Some(vt) = vtype {
					write!(f, " : {}", vt)?;
				}
				if let Some(e) = expr {
					write!(f, " = {}", e)?;
				}
				write!(f, "\n")
			},
			Statement::AssignStmt{variable, operator, expr} => {
				write!(f, "{} {} {}\n", variable.token, operator.token, expr)
			},
			Statement::SelectionStmt{cond, if_stmt, else_stmt} => {
				write!(f, "if {} {}", cond, if_stmt)?;
				if let Some(e) = else_stmt {
					write!(f, "else {}", e)?;
				}
				Ok(())
			},
		}
	}
}