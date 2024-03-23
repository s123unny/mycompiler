use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue,PointerValue,BasicValueEnum};
use inkwell::types::BasicMetadataTypeEnum;
use crate::scanner::TokenType;
use crate::ast::{AstNode,Function,Statement,Expression};

pub struct Compiler<'a> {
	context: &'a Context,
	builder: Builder<'a>,
  module: Module<'a>,
	// variables: HashMap<String, PointerValue<'a>>,
	variables: VarEnv<'a>,
	fn_val: Option<FunctionValue<'a>>,
}

// macro_rules! build_expr {
// 	($opr:ident, $ttype:expr, $($args:expr),+) => {
// 		match $ttype {
// 			TokenType::Integer(_) => self.builder.build_int_$opr($($args),+)
// 		}
		
// 	}
// }

impl<'a> Compiler<'a> {
	pub fn compile(context: &Context, ast: Vec<AstNode>) {
		let mut compiler = Compiler {
			context,
			builder: context.create_builder(),
			module: context.create_module("my_module"),
			// variables: HashMap::new(),
			variables: VarEnv::new(),
			fn_val: None
		};
		for node in ast {
			let AstNode::FunctionNode(func) = node;
			compiler.visit_func(&func);
		}
		compiler.module.get_functions().map(|f| f.print_to_stderr());
	}
	fn visit_func(&mut self, func: &Function) -> Result<FunctionValue, &str> {
		let args_types = func.prototype.atypes.iter().map(|v| { match v {
			TokenType::I32 | TokenType::I64 => self.context.i64_type().into(),
			TokenType::F32 | TokenType::F64 => self.context.f64_type().into(),
			TokenType::Bool => self.context.bool_type().into(),
			_ => self.context.bool_type().into(), //TODO
		}}).collect::<Vec<BasicMetadataTypeEnum>>();
		let args_types = args_types.as_slice();
		let fn_type = self.context.void_type().fn_type(args_types, false);
		let fn_val = self.module.add_function(&func.prototype.name, fn_type, None);

		// set arguments names
		for (i, arg) in fn_val.get_param_iter().enumerate() {
			arg.set_name(&func.prototype.args[i]);
		}

		let entry = self.context.append_basic_block(fn_val, "entry");
		self.builder.position_at_end(entry);

		self.fn_val = Some(fn_val);

		for (i, arg) in fn_val.get_param_iter().enumerate() {
			let arg_name = func.prototype.args[i].as_str();
			let alloca = self.create_entry_block_alloca(arg_name);

			self.builder.build_store(alloca, arg).unwrap();
			// let alloca = self.context.f64_type().ptr_type(AddressSpace::default()).const_zero();

			self.variables.insert(func.prototype.args[i].clone(), alloca);
			// let VarEnv::Vars(ref mut cur, _) = self.variables else {
			// 	panic!("VarEnv is Nil");
			// };
			// cur.insert(func.prototype.args[i].clone(), alloca);
		}

		let _body = self.visit_stmt(&func.body);

		Ok(fn_val)
	}

	/// Creates a new stack allocation instruction in the entry block of the function.
	fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'a> {
		let builder = self.context.create_builder();

		let entry = self.fn_val.unwrap().get_first_basic_block().unwrap();

		match entry.get_first_instruction() {
				Some(first_instr) => builder.position_before(&first_instr),
				None => builder.position_at_end(entry),
		}

		builder.build_alloca(self.context.f64_type(), name).unwrap()
	}

	fn visit_stmt(&self, stmt: &Statement) -> Result<(), &str> {
		match stmt {
			Statement::ExprStmt(expr) => self.visit_expr(expr, None),
			_ => panic!("todo visit_stmt"),
		};
		Ok(())
	}

	fn visit_expr(&self, expr: &Expression, _expect_type: Option<TokenType>) -> Result<BasicValueEnum, &str> {
		match expr {
			Expression::LiteralExpr(token) => {
				let l = match token.token {
					TokenType::Integer(n) => self.context.i64_type().const_int(n, false).into(),
					TokenType::Float(f) => self.context.f64_type().const_float(f).into(),
					TokenType::String(ref s) => self.context.const_string(s.as_bytes(), true).into(),
					TokenType::True => self.context.bool_type().const_int(1, false).into(),
					TokenType::False => self.context.bool_type().const_int(0, false).into(),
					_ => panic!("not literal tokens"),
				};
				Ok(l)
			},
			Expression::VaraibleExpr(token) => {
				let TokenType::Identifier(ref name) = token.token else {
					panic!("not identifier");
				};
				match self.variables.get(name) {
					// Some(v) => Ok(self.build_load(*v, &name)),
					Some(v) => Ok(self.builder.build_load(v.get_type(), *v, name).unwrap()),
					None => Err("Could not find a matching variable"),
				}
			},
			// Expression::BinaryExpr(left, operator, right) => {
			// 	let lhs = self.visit_expr(left)?;
			// 	let rhs = self.visit_expr(right)?;
			// 	match operator.token {
			// 		TokenType::Plus => 
			// 	}
			// }
			_ => {Err("Todo")}
		}
	}

	fn build_load(&self, ptr: PointerValue<'a>, name: &str) -> BasicValueEnum<'a> {
		self.builder.build_load(ptr.get_type(), ptr, name).unwrap()
	}

	// fn get_vtype(self, ttype: TokenType, expect_type: Option<TokenType>) -> Result<TokenType, Error> {
	// 	if let None = expect_type {
	// 		ttype
	// 	} else {

	// 	}
	// }
}

#[derive(PartialEq)]
enum VarEnv<'a> {
	Vars(HashMap<String, PointerValue<'a>>, Box<VarEnv<'a>>),
	Nil,
}

impl VarEnv<'_> {
	fn new() -> VarEnv<'static> {
		VarEnv::Vars(HashMap::new(), Box::new(VarEnv::Nil))
	}
	fn insert(&mut self, name: String, value: PointerValue) {
		let VarEnv::Vars(ref mut cur, _) = self else {
			panic!("VarEnv is Nil");
		};
		cur.insert(name, value);
	}

	fn get(&self, name: &str) -> Option<&PointerValue> {
		match self {
			VarEnv::Vars(cur, ref parent) => {
				match cur.get(name) {
					Some(p) => Some(p),
					_ => parent.get(name),
				}
			},
			VarEnv::Nil => None,
		}
	}
}