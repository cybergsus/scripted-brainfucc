use super::common::*;
use super::lexer::Lexer;

#[derive(Debug)]
pub enum ASTNode {
    Instruction(Instruction),
    // TODO: expand `ASTNode`
}

#[derive(Debug)]
pub enum Instruction {
    Msg { messages: Vec<RValue> },
}

#[derive(Debug)]
pub enum RValue {
    Constant(u8),
    Literal(String),
    Reference(LValue),
}

impl From<String> for RValue {
    fn from(st: String) -> Self {
        Self::Literal(st)
    }
}

#[derive(Debug)]
pub struct LValue {
    name: String,
    offset: Box<RValue>,
}
