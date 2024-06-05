mod scanner;
mod ast;
mod ir;

use std::env;
use std::process;
use std::error::Error;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use inkwell::{
	passes::PassBuilderOptions,
	targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
};

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = parse_config(&args).unwrap_or_else(|err| {
			println!("Problem parsing arguments: {err}");
			process::exit(1);
		});

    println!("file {}", config.file_path);

		if let Err(e) = compile(config) {
			println!("Application error: {e}");
			process::exit(1);
		}
}

struct Config {
	file_path: String,
}

fn parse_config(args: &[String]) -> Result<Config, &'static str>{
	if args.len() < 2 {
		return Err("Not enough arguments");
	}
	let file_path = args[1].clone();
	Ok(Config { file_path })
}

fn run_passes_on(module: &Module) {
	Target::initialize_all(&InitializationConfig::default());
	let target_triple = TargetMachine::get_default_triple();
	let target = Target::from_triple(&target_triple).unwrap();
	let target_machine = target
			.create_target_machine(
					&target_triple,
					"generic",
					"",
					OptimizationLevel::None,
					RelocMode::PIC,
					CodeModel::Default,
			)
			.unwrap();

	let passes: &[&str] = &[
			"instcombine",
			"reassociate",
			"gvn",
			"simplifycfg",
			"mem2reg",
	];

	module
		.run_passes(passes.join(",").as_str(), &target_machine, PassBuilderOptions::create())
		.unwrap();
}

fn compile(config: Config) -> Result<(), Box<dyn Error>> {
	let mut scanner = scanner::Scanner::new(&config.file_path);
	scanner.scan_tokens();
	scanner.print_tokens();

	let ast = ast::parse_ast(&mut scanner.tokens);
	let context = Context::create();
	let mut compiler = ir::Compiler::new(&context);
	compiler.compile(ast)?;
	compiler.module.verify()?;
	run_passes_on(&compiler.module);

	println!("After passes");
	compiler.module.print_to_stderr();

	let ee = compiler.module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
	let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i64>("main") };
	let compiled_fn = match maybe_fn {
			Ok(f) => f,
			Err(err) => {
					println!("!> Error during execution: {:?}", err);
					panic!("error");
			},
	};

	unsafe {
			println!("=> {}", compiled_fn.call());
	}
	Ok(())
}