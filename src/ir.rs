use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue,PointerValue,BasicValueEnum};
use inkwell::types::{BasicMetadataTypeEnum,BasicTypeEnum,BasicType};
use inkwell::{IntPredicate,FloatPredicate};
use crate::scanner::{Token,TokenType};
use crate::ast::{AstNode,Function,Statement,Expression};
use crate::ir::GenResult::{Good,Bad};

pub struct Compiler<'a> {
	context: &'a Context,
	builder: Builder<'a>,
  pub module: Module<'a>,
	variables: VarEnv<'a>,
	fn_val: Option<FunctionValue<'a>>,
}

enum GenResult<T> {
	Good(T),
	Bad
}

fn error<T>(m: &str, found: Option<Token>) -> GenResult<T> {
	match found {
		Some(t) => println!("file:{}:{}: {}", t.source.line, t.source.col, m),
		None => println!("file: {}", m),
	}
	Bad
}

macro_rules! gen_try {
	($self:ident, $function:ident, $($arg:ident),+) => {
		match $self.$function($($arg),+) {
			Good(ir) => ir,
			Bad => return Bad
		}
	}
}

macro_rules! type_equal {
	($val:expr, $basic_type:expr, $error:expr) => {
		if $val.get_type() == $basic_type {
			Good($val)
		} else {
			return error($error, None)
		}
	}
}

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
	pub fn compile(&mut self, ast: Vec<AstNode>) -> Result<(),  &'static str> {
		for node in ast {
			let AstNode::FunctionNode(func) = node;
			match self.visit_func(&func) {
				Bad => { return Err("compile error"); },
				_ => {}
			}
		}
		println!("complete compile");
		self.module.print_to_stderr();
		Ok(())
	}
	fn visit_func(&mut self, func: &Function) -> GenResult<FunctionValue<'a>> {
		let args_types = func.prototype.atypes.iter()
			.map(|v| self.get_basic_metadata_enum(v))
			.collect::<Vec<BasicMetadataTypeEnum>>();
		let args_types = args_types.as_slice();
		let fn_type = match &func.prototype.ret {
			Some(t) => self.get_basic_type_enum(&t).fn_type(args_types, false),
			None => self.context.void_type().fn_type(args_types, false),
		};
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

			self.variables.insert(func.prototype.args[i].clone(), alloca, arg.get_type());
		}

		let body = &func.body;
		gen_try!(self, visit_stmt, body);

		Good(fn_val)
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

	fn visit_stmt(&mut self, stmt: &Statement) -> GenResult<()> {
		match stmt {
			Statement::ExprStmt(expr) => {
				gen_try!(self, visit_expr, expr);
			},
			Statement::Block{stmts} => {
				for s in stmts.iter() {
					gen_try!(self, visit_stmt, s);
				}
			},
			Statement::ReturnStmt(expr) => {
				let r = gen_try!(self, visit_expr, expr);
				let ret_type = self.fn_val.expect("expect FunctionValue").get_type().get_return_type();
				match ret_type {
					None => return error("mismatched type: return type is void", None),
					Some(t) => {type_equal!(r, t, "return type not match");}
				}
				self.builder.build_return(Some(&r)).unwrap();
			}
			Statement::VarDecl{variable, vtype, expr} => {
				let var_val = match expr {
					Some(e) => {
						let r = gen_try!(self, visit_expr, e);
						gen_try!(self, type_check, r, vtype)
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

				self.variables.insert(var_name.clone(), alloca, var_val.get_type());
			},
			Statement::AssignStmt{variable, operator: _, expr} => {
				let r = gen_try!(self, visit_expr, expr);
				let TokenType::Identifier(ref var_name) = variable.token else {
					panic!("not an identifier");
				};
				let var = self.variables.get(&var_name).expect("Undefined variable."); // todo: catch error
				type_equal!(r, var.vtype, "expect type equal in AssignStmt");
				self.builder.build_store(var.pos, r).unwrap();
			}
			Statement::SelectionStmt{cond, if_stmt, else_stmt} => {
				let parent = self.fn_val.expect("expect FunctionValue");

				let zero_const = self.context.i64_type().const_int(0, false);
				let cond = gen_try!(self, visit_expr, cond).into_int_value();
				let cond = self.builder.build_int_compare(IntPredicate::NE, cond, zero_const, "id_cond").unwrap();

				let then_bb = self.context.append_basic_block(parent, "then");
				let else_bb = self.context.append_basic_block(parent, "else");
				let cont_bb = self.context.append_basic_block(parent, "ifcont");

				self.builder.build_conditional_branch(cond, then_bb, else_bb).unwrap();

				// build then block
				self.builder.position_at_end(then_bb);
				gen_try!(self, visit_stmt, if_stmt);
				self.builder.build_unconditional_branch(cont_bb).unwrap();

				let _then_bb = self.builder.get_insert_block().unwrap();

				// build else block
				self.builder.position_at_end(else_bb);
				if let Some(stmt) = else_stmt {
					gen_try!(self, visit_stmt, stmt);
				}
				self.builder.build_unconditional_branch(cont_bb).unwrap();

				let _else_bb = self.builder.get_insert_block().unwrap();

				// emit merge block
				self.builder.position_at_end(cont_bb);
			},
		};
		Good(())
	}

	fn visit_expr(&self, expr: &Expression) -> GenResult<BasicValueEnum<'a>> {
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
				Good(l)
			},
			Expression::VaraibleExpr(token) => {
				let TokenType::Identifier(ref name) = token.token else {
					panic!("not identifier");
				};
				match self.variables.get(&name) {
					Some(v) => Good(self.build_load(v.vtype, v.pos, name.as_str())),
					None => error("Could not find a matching variable", None),
				}
			},
			Expression::BinaryExpr{left, operator, right} => {
				self.visit_binary_expr(left, operator, right)
			},
			Expression::UnaryExpr{operator, right} => {
				let rhs = gen_try!(self, visit_expr, right);
				let vtype = rhs.get_type();
				match vtype {
					BasicTypeEnum::IntType(_) => {
						let rhs = rhs.into_int_value();
						match operator.token {
							TokenType::Not => Good(self.builder.build_not(rhs, "not").unwrap().into()),
							TokenType::Minus => Good(self.builder.build_int_neg(rhs, "neg").unwrap().into()),
							_ => {panic!("Not supported unary opertor")},
						}
					},
					BasicTypeEnum::FloatType(_) => {
						error("TODO unary float type", None)
					},
					_ => panic!("Not supported type"),
				}
			}
		}
	}

	fn visit_binary_expr(&self, left: &Expression, operator: &Token, right: &Expression) -> GenResult<BasicValueEnum<'a>> {
		let lhs = gen_try!(self, visit_expr, left);
		let rhs = gen_try!(self, visit_expr, right);
		let vtype = lhs.get_type();
		match vtype {
			BasicTypeEnum::IntType(_) => {
				let lhs = lhs.into_int_value();
				let rhs = rhs.into_int_value();
				macro_rules! build_int_compare {
					($op:expr, $name:expr) => {
						Good({
							let cmp = self.builder.build_int_compare($op, lhs, rhs, $name).unwrap();
							self.builder.build_int_cast(cmp, self.context.i64_type(), "tmpcast").unwrap().into()
						})
					}
				}
				match operator.token {
					TokenType::Plus => Good(self.builder.build_int_add(lhs, rhs, "add").unwrap().into()),
					TokenType::Minus => Good(self.builder.build_int_sub(lhs, rhs, "sub").unwrap().into()),
					TokenType::Star => Good(self.builder.build_int_mul(lhs, rhs, "mul").unwrap().into()),
					TokenType::Slash => Good(self.builder.build_int_signed_div(lhs, rhs, "div").unwrap().into()),
					TokenType::NotEqual => build_int_compare!(IntPredicate::NE, "neq"),
					TokenType::EqualEqual => build_int_compare!(IntPredicate::EQ, "eq"),
					TokenType::Greater => build_int_compare!(IntPredicate::SGT, "gt"),
					TokenType::GreaterEqual => build_int_compare!(IntPredicate::SGE, "ge"),
					TokenType::Less => build_int_compare!(IntPredicate::SLT, "lt"),
					TokenType::LessEqual => build_int_compare!(IntPredicate::SLE, "le"),
					_ => panic!("Not supported type"),
				}
			},
			BasicTypeEnum::FloatType(_) => {
				let lhs = lhs.into_float_value();
				let rhs = rhs.into_float_value();
				macro_rules! build_float_compare {
					($op:expr, $name:expr) => {
						Good({
							let cmp = self.builder.build_float_compare($op, lhs, rhs, $name).unwrap();
							self.builder.build_int_cast(cmp, self.context.i64_type(), "tmpcast").unwrap().into()
						})
					}
				}
				match operator.token {
					TokenType::Plus => Good(self.builder.build_float_add(lhs, rhs, "add").unwrap().into()),
					TokenType::Minus => Good(self.builder.build_float_sub(lhs, rhs, "sub").unwrap().into()),
					TokenType::Star => Good(self.builder.build_float_mul(lhs, rhs, "mul").unwrap().into()),
					TokenType::Slash => Good(self.builder.build_float_div(lhs, rhs, "div").unwrap().into()),
					TokenType::NotEqual => build_float_compare!(FloatPredicate::UNE, "neq"),
					TokenType::EqualEqual => build_float_compare!(FloatPredicate::UEQ, "eq"),
					TokenType::Greater => build_float_compare!(FloatPredicate::UGT, "gt"),
					TokenType::GreaterEqual => build_float_compare!(FloatPredicate::UGE, "ge"),
					TokenType::Less => build_float_compare!(FloatPredicate::ULT, "lt"),
					TokenType::LessEqual => build_float_compare!(FloatPredicate::ULE, "le"),
					_ => panic!("Not supported type"),
				}
			},
			_ => panic!("Not supported type"),
		}
	}

	fn build_load(&self, vtype: BasicTypeEnum<'a>, ptr: PointerValue<'a>, name: &str) -> BasicValueEnum<'a> {
		self.builder.build_load(vtype, ptr, name).unwrap()
	}

	fn type_check(&self, val: BasicValueEnum<'a>, expect_type: &Option<TokenType>) -> GenResult<BasicValueEnum<'a>> {
		match expect_type {
			None => Good(val),
			Some(TokenType::I32) => type_equal!(val, BasicTypeEnum::IntType(self.context.i32_type()), "expected i32"),
			Some(TokenType::I64) => type_equal!(val, BasicTypeEnum::IntType(self.context.i64_type()), "expected i64"),
			Some(TokenType::F32) => type_equal!(val, BasicTypeEnum::FloatType(self.context.f32_type()), "expected f32"),
			Some(TokenType::F64) => type_equal!(val, BasicTypeEnum::FloatType(self.context.f64_type()), "expected f64"),
			Some(TokenType::Bool) => type_equal!(val, BasicTypeEnum::IntType(self.context.bool_type()), "expected bool"),
			_ => error("type_check: Not type token", None),
		}
	}
}

struct VarInfo<'a> {
	pos: PointerValue<'a>,
	vtype: BasicTypeEnum<'a>
}

enum VarEnv<'a> {
	Vars(HashMap<String, VarInfo<'a>>, Box<VarEnv<'a>>),
	Nil,
}

impl <'a>VarEnv<'a> {
	fn new() -> VarEnv<'a> {
		VarEnv::Vars(HashMap::new(), Box::new(VarEnv::Nil))
	}

	fn insert(&mut self, name: String, pos: PointerValue<'a>, vtype: BasicTypeEnum<'a>) {
		let VarEnv::Vars(ref mut cur, _) = self else {
			panic!("VarEnv is Nil");
		};
		cur.insert(name, VarInfo{pos, vtype});
	}

	fn get(&self, name: &str) -> Option<&VarInfo<'a>> {
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