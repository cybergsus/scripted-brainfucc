mod lib;

use lib::common::WithPosition;
use lib::compiler::{compile, BFSource};
use lib::parser::*;
use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::Read;
use std::io::Write;

#[derive(Debug)]
enum CompileResult {
    IO(std::io::Error),
    Parser(WithPosition<ParseError>),
}

fn compile_src(src: &str) -> Result<BFSource, WithPosition<ParseError>> {
    let mut parser = Parser::new(src);
    let mut ast = parser.parse_ast()?;
    Ok(compile(&mut ast))
}

fn compile_file(name: &str) -> Result<BFSource, CompileResult> {
    let mut file = File::open(name).map_err(CompileResult::IO)?;
    let mut src = String::new();

    // inject code to print the file name.
    let name_len = name.chars().collect::<Vec<_>>().len();
    let mut custom_src = String::from("msg \"");
    for _ in 0..name_len + b"executing file: \"\"".len() {
        custom_src.push('-');
    }
    custom_src.push_str("\\n");
    custom_src.push_str(&format!("executing file: \\\"{}\\\"\\n", name));
    for _ in 0..name_len + b"executing file: \"\"".len() {
        custom_src.push('-');
    }
    custom_src.push_str("\\n\\n\"\n");
    src.push_str(custom_src.as_ref());

    // now the actual code.
    let mut file_src = String::new();
    file.read_to_string(&mut file_src)
        .map_err(CompileResult::IO)?;
    src.push_str(file_src.as_ref());

    compile_src(src.as_ref()).map_err(CompileResult::Parser)
}

fn main() {
    let mut errors = Vec::new();

    for name in args().skip(1) {
        match compile_file(name.as_ref()) {
            Err(e) => errors.push((name, e)),
            Ok(c) => print!("{}", c),
        }
    }
    std::io::stdout().lock().flush().unwrap();

    for (file_name, err) in errors {
        // TODO: error display
        eprintln!("Error in file {:?}: {:?}", file_name, err);
    }
}
