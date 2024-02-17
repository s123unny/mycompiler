mod scanner;

use std::env;
use std::fs;
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
	let contents = fs::read_to_string(config.file_path)?;
	let contents = contents.as_bytes();

	println!("Read:\n{contents:?}");

	let mut scanner = scanner::Scanner::new(contents);
	scanner.scan_tokens();
	scanner.print_token();

	Ok(())
}