mod scanner;
mod ast;

use std::env;
use std::process;
use std::error::Error;

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

fn compile(config: Config) -> Result<(), Box<dyn Error>> {
	let mut scanner = scanner::Scanner::new(&config.file_path);
	scanner.scan_tokens();
	scanner.print_tokens();

	ast::parse_ast(&mut scanner.tokens);
	Ok(())
}