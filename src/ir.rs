use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue,PointerValue,BasicValueEnum};
use inkwell::types::{BasicMetadataTypeEnum,BasicTypeEnum};
use inkwell::FloatPredicate;
use crate::scanner::TokenType;
use crate::ast::{AstNode,Function,Statement,Expression};

pub struct Compiler<'a> {
	context: &'a Context,
	builder: Builder<'a>,
  module: Module<'a>,
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

impl <'a>Compiler<'a> {
	pub fn new(context: &'a Context) -> Compiler<'a> {
		Compiler {
			context,
			builder: context.create_builder(),
			module: context.create_module("my_module"),
			variables: VarEnv::new(),
			fn_val: None
		}
	}
	pub fn compile(&mut self, ast: Vec<AstNode>) {
		for node in ast {
			let AstNode::FunctionNode(func) = node;
			let _ = self.visit_func(&func);
		}
		println!("complete compile");
		self.module.print_to_stderr();
	}
	fn visit_func(&mut self, func: &Function) -> Result<FunctionValue<'a>, &'static str> {
		let args_types = func.prototype.atypes.iter()
			.map(|v| self.get_basic_metadata_enum(v))
			.collect::<Vec<BasicMetadataTypeEnum>>();
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

			self.variables.insert(func.prototype.args[i].clone(), alloca);
		}

		let _body = self.visit_stmt(&func.body);

		Ok(fn_val)
	}

	fn get_basic_metadata_enum(&self, v: &TokenType) -> BasicMetadataTypeEnum<'a> {
		match v {
			TokenType::I32 => self.context.i32_type().into(),
			TokenType::I64 => self.context.i64_type().into(),
			TokenType::F32 => self.context.f32_type().into(),
			TokenType::F64 => self.context.f64_type().into(),
			TokenType::Bool => self.context.bool_type().into(),
			_ => panic!("Not type token"),
		}
	}

	fn get_basic_type_enum(&self, v: &TokenType) -> BasicTypeEnum<'a> {
		match v {
			TokenType::I32 => self.context.i32_type().into(),
			TokenType::I64 => self.context.i64_type().into(),
			TokenType::F32 => self.context.f32_type().into(),
			TokenType::F64 => self.context.f64_type().into(),
			TokenType::Bool => self.context.bool_type().into(),
			_ => panic!("Not type token"),
		}
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

	fn visit_stmt(&mut self, stmt: &Statement) -> Result<(), &'static str> {
		match stmt {
			Statement::ExprStmt(expr) => {
				let _ = self.visit_expr(expr);
			},
			Statement::Block{stmts} => {
				let _ = stmts.iter().for_each(|s| self.visit_stmt(s).expect("visit block"));
			},
			Statement::VarDecl{variable, vtype, expr} => {
				let var_val = match expr {
					Some(e) => {
						let r = self.visit_expr(e).expect("expect result");
						self.type_check(r, vtype)?
					},
					None => {
						let v = vtype.as_ref().expect("Missing type in variable declaration");
						self.get_basic_type_enum(&v).const_zero()
					},
				};
				let TokenType::Identifier(ref var_name) = variable.token else {
					panic!("Not an identifier in VarDecl");
				};
				let alloca = self.create_entry_block_alloca(var_name.as_str());

				self.builder.build_store(alloca, var_val).unwrap();

				self.variables.insert(var_name.clone(), alloca);
			},
			Statement::AssignStmt{variable, operator: _, expr} => {
				let r = self.visit_expr(expr).expect("expect result");
				let TokenType::Identifier(ref var_name) = variable.token else {
					panic!("not an identifier");
				};
				let var = self.variables.get(&var_name).ok_or("Undefined variable.")?;
				self.builder.build_store(*var, r).unwrap();
			}
			Statement::SelectionStmt{cond, if_stmt, else_stmt} => {
				let parent = self.fn_val.expect("expect FunctionValue");
				let zero_const = self.context.f64_type().const_float(0.0);

				let cond = self.visit_expr(cond)?.into_float_value();
				let cond = self.builder.build_float_compare(FloatPredicate::ONE, cond, zero_const, "id_cond").unwrap();

				let then_bb = self.context.append_basic_block(parent, "then");
				let else_bb = self.context.append_basic_block(parent, "else");
				let cont_bb = self.context.append_basic_block(parent, "ifcont");

				self.builder.build_conditional_branch(cond, then_bb, else_bb).unwrap();

				// build then block
				self.builder.position_at_end(then_bb);
				self.visit_stmt(if_stmt)?;
				self.builder.build_unconditional_branch(cont_bb).unwrap();

				let _then_bb = self.builder.get_insert_block().unwrap();

				// build else block
				self.builder.position_at_end(else_bb);
				if let Some(stmt) = else_stmt {
					self.visit_stmt(stmt)?;
				}
				self.builder.build_unconditional_branch(cont_bb).unwrap();

				let _else_bb = self.builder.get_insert_block().unwrap();

				// emit merge block
				self.builder.position_at_end(cont_bb);
			},
		};
		Ok(())
	}

	fn visit_expr(&self, expr: &Expression) -> Result<BasicValueEnum<'a>, &'static str> {
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
				match self.variables.get(&name) {
					Some(v) => Ok(self.build_load(*v, name.as_str())),
					// Some(v) => Ok(self.builder.build_load(v.get_type(), *v, name.as_str()).unwrap()),
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

	fn type_check(&self, val: BasicValueEnum<'a>, expect_type: &Option<TokenType>) -> Result<BasicValueEnum<'a>, &'static str> {
		match expect_type {
			None => Ok(val),
			Some(TokenType::I32) => {
				if val.get_type() == BasicTypeEnum::IntType(self.context.i32_type()) {
					Ok(val)
				} else {
					Err("expected i32")
				}
			},
			Some(TokenType::I64) => {
				if val.get_type() == BasicTypeEnum::IntType(self.context.i64_type()) {
					Ok(val)
				} else {
					Err("expected i64")
				}
			},
			Some(TokenType::F32) => {
				if val.get_type() == BasicTypeEnum::FloatType(self.context.f32_type()) {
					Ok(val)
				} else {
					Err("expected f32")
				}
			},
			Some(TokenType::F64) => {
				if val.get_type() == BasicTypeEnum::FloatType(self.context.f64_type()) {
					Ok(val)
				} else {
					Err("expected f64")
				}
			},
			Some(TokenType::Bool) => {
				if val.get_type() == BasicTypeEnum::IntType(self.context.bool_type()) {
					Ok(val)
				} else {
					Err("expected bool")
				}
			},
			_ => panic!("Not type token"),
		}
	}
}

#[derive(PartialEq)]
enum VarEnv<'a> {
	Vars(HashMap<String, PointerValue<'a>>, Box<VarEnv<'a>>),
	Nil,
}

impl <'a>VarEnv<'a> {
	fn new() -> VarEnv<'a> {
		VarEnv::Vars(HashMap::new(), Box::new(VarEnv::Nil))
	}

	fn insert(&mut self, name: String, value: PointerValue<'a>) {
		let VarEnv::Vars(ref mut cur, _) = self else {
			panic!("VarEnv is Nil");
		};
		cur.insert(name, value);
	}

	fn get(&self, name: &str) -> Option<&PointerValue<'a>> {
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