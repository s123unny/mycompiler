use crate::scanner::{Token,TokenType};
use crate::ast::ParsingResult::{Good,Bad};

pub enum Expression {
	LiteralExpr(Token),
	VaraibleExpr(Token),
	BinaryExpr{left: Box<Expression>, operator: Token, right: Box<Expression>},
	UnaryExpr{operator: Token, right: Box<Expression>},
}

pub enum Statement {
	ExprStmt(Expression),
	Block{stmts: Vec<Statement>},
	AssignStmt{variable: Token, operator: Token, expr: Box<Expression>},
	SelectionStmt{cond: Expression, if_stmt: Box<Statement>, else_stmt: Box<Statement>}
}

pub struct Function {
	pub prototype: Prototype,
  pub body: Vec<Statement>
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
		let cur = match tokens.last() {
			Some(token) => token.clone(),
			None => break
		};
		let result = match cur.token {
			TokenType::Def => parse_function(tokens),
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
			_ => return error($error)
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

fn parse_function(tokens: &mut Vec<Token>) -> ParsingResult<Function> {
	tokens.pop(); // pop Def token
	let prototype = parse_try!(parse_prototype, tokens);
	let body = parse_try!(parse_block, tokens);
	Good(Function{ prototype, body})
}

fn parse_prototype(tokens: &mut Vec<Token>) -> ParsingResult<Prototype> {
	let name = expect!(tokens, "expected function name in prototype", [TokenType::Identifier(name), name]);
	expect!(tokens, "expected '(' in prototype", [TokenType::LeftParen, true]);
	let mut args = Vec::new();
	let mut atypes = Vec::new();
	if peek(tokens, TokenType::RightParen) {
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

fn parse_block(tokens: &mut Vec<Token>) -> ParsingResult<Vec<Statement>> {
	expect!(tokens, "expected '{' after prototype", [TokenType::LeftBrace, {}]);
	let mut stmts = Vec::new();
	while !peek(tokens, TokenType::RightBrace) {
		let result = parse_try!(parse_stmt, tokens);
		stmts.push(result);
	}
	expect!(tokens, "expected '}' after prototype", [TokenType::RightBrace, {}]);
	Good(stmts)
}

fn parse_stmt(tokens: &mut Vec<Token>) -> ParsingResult<Statement> {
	Good(Statement::ExprStmt(Expression::LiteralExpr(Token{token: TokenType::Def, line: 1})))
}

fn peek(tokens: &mut Vec<Token>, token: TokenType) -> bool {
	let top = tokens.last();
	match top {
		Some(Token{ token: token, .. }) => true,
		_ => false
	}
}

