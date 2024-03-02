use crate::scanner::{Token,TokenType};
use crate::ast::ParsingResult::{Good,Bad};
use std::fmt;

#[derive(Debug)]
pub enum Expression {
	LiteralExpr(TokenType),
	VaraibleExpr(TokenType),
	BinaryExpr{left: Box<Expression>, operator: Token, right: Box<Expression>},
	UnaryExpr{operator: Token, right: Box<Expression>},
}
#[derive(Debug)]
pub enum Statement {
	ExprStmt(Expression),
	Block{stmts: Vec<Statement>},
	VarDecl{variable: String, vtype: Option<TokenType>, expr: Option<Expression>},
	AssignStmt{variable: String, operator: TokenType, expr: Expression},
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

enum AstNode {
	FunctionNode(Function),
}

pub fn parse_ast(tokens: &mut Vec<Token>) {
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
			Bad => break
		}
	}
}

enum ParsingResult<T> {
	Good(T),
	Bad
}

fn error<T>(m: &str) -> ParsingResult<T> {
	println!("Error: {}", m);
	Bad
}

macro_rules! expect {
	($tokens: expr, $error:expr, [$($token:pat, $result:stmt);+]) => (
		match $tokens.pop() {
			$(
				Some(Token{ token:$token, .. }) => {
					$result
				},
			)+
			y => {
				println!("{:?}", y);
				return error($error)
			}
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
	let name = expect!(tokens, "expected function name in prototype", [TokenType::Identifier(name), name]);
	expect!(tokens, "expected '(' in prototype", [TokenType::LeftParen, true]);
	let mut args = Vec::new();
	let mut atypes = Vec::new();
	if peek!(tokens, TokenType::RightParen) {
		tokens.pop();
		return Good(Prototype{ name, args, atypes });
	}
	loop {
		expect!(tokens, "expected identifier in prototype", [
			TokenType::Identifier(arg), args.push(arg.clone())
		]);
		expect!(tokens, "expected ':' in prototype", [TokenType::Colon, {}]);
		expect!(tokens, "expected type in prototype", [
			TokenType::I32, atypes.push(TokenType::I32);
			TokenType::I64, atypes.push(TokenType::I64);
			TokenType::F32, atypes.push(TokenType::F32);
			TokenType::F64, atypes.push(TokenType::F64)
		]);
		expect!(tokens, "expected ')' in prototype", [
			TokenType::Comma, continue;
			TokenType::RightParen, break
		]);
	}
	Good(Prototype{ name, args, atypes })
}

fn parse_block(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	expect!(tokens, "expected '{' after prototype", [TokenType::LeftBrace, {}]);
	let mut stmts = Vec::new();
	while !peek!(tokens, TokenType::RightBrace) {
		let result = parse_try!(parse_stmt, tokens);
		stmts.push(result);
	}
	expect!(tokens, "expected '}' after prototype", [TokenType::RightBrace, {}]);
	Good( Statement::Block{stmts} )
}

fn parse_selection(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	expect!(tokens, "expected 'if'", [TokenType::If, {}]);
	let cond = parse_try!(parse_expr, tokens);
	let if_stmt = Box::new(parse_try!(parse_block, tokens));
	let else_stmt = if peek!(tokens, TokenType::Else) {
		expect!(tokens, "expected 'else'", [TokenType::Else, {}]);
		Some(Box::new(parse_try!(parse_block, tokens)))
	} else {
		None
	};
	Good( Statement::SelectionStmt{ cond, if_stmt, else_stmt } )
}

fn parse_var_decl(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	expect!(tokens, "expected 'var'", [TokenType::Var, {}]);
	let variable = expect!(tokens, "expected variable", [TokenType::Identifier(variable), variable]);
	let vtype = if peek!(tokens, TokenType::Colon) {
		expect!(tokens, "expected ':'", [TokenType::Colon, {}]);
		let v = expect!(tokens, "expected type after ':", [
			TokenType::I32, TokenType::I32;
			TokenType::I64, TokenType::I64;
			TokenType::F32, TokenType::F32;
			TokenType::F64, TokenType::F64
		]);
		Some(v)
	} else {
		None
	};
	let expr = if peek!(tokens, TokenType::Equal) {
		expect!(tokens, "expected '='", [TokenType::Equal, {}]);
		let e = parse_try!(parse_expr, tokens);
		Some(e)
	} else {
		None
	};
	expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, {}]);
	Good( Statement::VarDecl{ variable, vtype, expr } )
}

fn parse_stmt(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	let stmt = match tokens.last() {
		Some(Token{ token: TokenType::LeftBrace, .. }) => parse_try!(parse_block, tokens),
		Some(Token{ token: TokenType::If, .. }) => parse_try!(parse_selection, tokens),
		Some(Token{ token: TokenType::Var, .. }) => parse_try!(parse_var_decl, tokens),
		Some(Token{ token: TokenType::Identifier(_), .. }) => {
			if peek_next!(tokens, TokenType::Equal) {
				let variable = expect!(tokens, "expected variable", [TokenType::Identifier(variable), variable]);
				let operator = expect!(tokens, "expected '='", [TokenType::Equal, TokenType::Equal]);
				let expr = parse_try!(parse_expr, tokens);
				expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, {}]);
				Statement::AssignStmt{ variable, operator, expr }
			} else {
				let expr = parse_try!(parse_expr, tokens);
				expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, {}]);
				Statement::ExprStmt(expr)
			}
		}
		_ => {
			let expr = parse_try!(parse_expr, tokens);
			expect!(tokens, "expected ';' at the end of statement", [TokenType::Semicolon, {}]);
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
		TokenType::String(t), Expression::LiteralExpr(TokenType::String(t));
		TokenType::Integer(t), Expression::LiteralExpr(TokenType::Integer(t));
		TokenType::Float(t), Expression::LiteralExpr(TokenType::Float(t));
		TokenType::True, Expression::LiteralExpr(TokenType::True);
		TokenType::False, Expression::LiteralExpr(TokenType::False);
		TokenType::Null, Expression::LiteralExpr(TokenType::Null);
		TokenType::Identifier(t), Expression::VaraibleExpr(TokenType::Identifier(t));
		TokenType::LeftBrace, {
			let expr = parse_try!(parse_expr, tokens);
			expect!(tokens, "expected ')' after expression", [TokenType::RightBrace, {}]);
			expr
		}
	]);
	println!("primary {:?}", expr);
	Good(expr)
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self {
			Expression::LiteralExpr(t) | Expression::VaraibleExpr(t) => write!(f, "{}", t),
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
				write!(f, "{}", variable)?;
				if let Some(vt) = vtype {
					write!(f, " : {}", vt)?;
				}
				if let Some(e) = expr {
					write!(f, " = {}", e)?;
				}
				write!(f, "\n")
			},
			Statement::AssignStmt{variable, operator, expr} => {
				write!(f, "{} {} {}\n", variable, operator, expr)
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