use super::common;
use super::parser::Parser;

use super::ast::*;
use std::collections::BTreeMap;

#[derive(Debug)]
#[repr(transparent)]
pub struct BFSource {
    commands: Vec<(BFCommand, usize)>,
}

impl std::fmt::Display for BFSource {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (comm, amount) in self.commands.iter().cloned() {
            for _ in 0..amount {
                write!(fmt, "{}", comm as u8 as char)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum BFCommand {
    GoRight = b'>',
    GoLeft = b'<',
    Print = b'.',
    Read = b',',
    BeginLoop = b'[',
    EndLoop = b']',
    Increment = b'+',
    Decrement = b'-',
}

pub struct Compiler {
    bindings: BTreeMap<String, usize>,
    temporaries: Vec<usize>,
    size: usize,
    current_address: usize,
    output: Vec<(BFCommand, usize)>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            bindings: BTreeMap::new(),
            temporaries: Vec::new(),
            size: 0,
            current_address: 0,
            output: Vec::new(),
        }
    }
}

impl Compiler {
    pub fn go_to(&mut self, addr: usize) {
        if addr == self.current_address {
            return;
        }
        let (direction, diff) = if addr > self.current_address {
            (BFCommand::GoRight, addr - self.current_address)
        } else {
            (BFCommand::GoLeft, self.current_address - addr)
        };

        self.push_output(direction, diff);

        self.current_address = addr;
    }

    pub fn loop_with<F, T>(&mut self, f: F) -> T
    where
        F: Fn(&mut Self) -> T,
    {
        self.push_output(BFCommand::BeginLoop, 1);
        let result = f(self);
        self.push_output(BFCommand::EndLoop, 1);
        result
    }

    pub fn push_output(&mut self, command: BFCommand, amount: usize) {
        if amount == 0 {
            return;
        }
        if let Some(last) = self.output.last_mut().filter(|(x, _)| x == &command) {
            last.1 += amount;
        } else {
            self.output.push((command, amount));
        }
    }

    fn dealloc_temps(&mut self, n: usize) {
        for _ in 0..n {
            self.temporaries.pop();
        }
    }

    fn alloc_temps(&mut self, n: usize) {
        for i in 0..n {
            self.temporaries.push(self.size + i);
        }
        self.size += n;
    }

    pub fn zero_current_cell(&mut self) {
        self.loop_with(|ctx| {
            ctx.push_output(BFCommand::Decrement, 1);
        })
    }

    pub fn get_temp(&self, index: usize) -> Option<usize> {
        let index = self.temporaries.len().checked_sub(index + 1)?;
        self.temporaries.get(index).cloned()
    }

    pub fn increment_current_cell(&mut self, n: u8) {
        self.push_output(BFCommand::Increment, n as usize);
    }

    pub fn decrement_current_cell(&mut self, n: u8) {
        self.push_output(BFCommand::Decrement, n as usize);
    }

    pub fn with_temps<F, T>(&mut self, temps: usize, f: F) -> T
    where
        F: Fn(&mut Self) -> T,
    {
        self.alloc_temps(temps);
        let result = f(self);
        self.dealloc_temps(temps);
        result
    }

    /// Compiles a program that outputs `st`.
    fn compile_print_text(&mut self, st: &str) {
        self.with_temps(1, move |ctx| {
            let addr = ctx.get_temp(0).unwrap();
            ctx.go_to(addr);
            ctx.zero_current_cell();
            let mut last_value = 0;
            for ch in st.bytes() {
                if ch > last_value {
                    ctx.increment_current_cell(ch - last_value);
                } else {
                    ctx.decrement_current_cell(last_value - ch);
                }
                ctx.print_current();
                last_value = ch;
            }
        })
    }

    fn print_current(&mut self) {
        self.push_output(BFCommand::Print, 1);
    }

    fn compile_msg(&mut self, messages: Vec<RValue>) {
        for msg in messages {
            match msg {
                RValue::Literal(st) => self.compile_print_text(st.as_ref()),
                RValue::Constant(_) => todo!("message constants"),
                RValue::Reference(_) => todo!("message variables"),
            }
        }
    }

    fn compile_inst(&mut self, inst: Instruction) {
        match inst {
            Instruction::Msg { messages } => self.compile_msg(messages),
        }
    }

    // TODO: make this return error??
    pub fn compile_ast(&mut self, ast: ASTNode) {
        match ast {
            ASTNode::Instruction(inst) => self.compile_inst(inst),
        }
    }

    // TODO: make this return `Result`.
    pub fn release(self) -> BFSource {
        BFSource {
            commands: self.output,
        }
    }
}

pub fn compile(ast: &mut Vec<ASTNode>) -> BFSource {
    let mut comp = Compiler::default();
    for node in ast.drain(..) {
        comp.compile_ast(node);
    }
    comp.release()
}

#[cfg(test)]
mod tests {
    use super::*;
}
