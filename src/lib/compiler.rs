use super::common;
use super::parser::Parser;

use super::ast::*;
use std::collections::BTreeMap;

#[derive(Debug)]
#[repr(transparent)]
pub struct BFSource {
    commands: Vec<(u8, usize)>,
}

impl std::fmt::Display for BFSource {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (comm, amount) in self.commands.iter().cloned() {
            for _ in 0..amount {
                write!(fmt, "{}", comm as char)?;
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
    output: Vec<(u8, usize)>,
    addr_stack: Vec<usize>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            bindings: BTreeMap::new(),
            temporaries: Vec::new(),
            size: 0,
            current_address: 0,
            output: Vec::new(),
            addr_stack: Vec::new(),
        }
    }
}

impl Compiler {
    fn save_addr(&mut self) {
        self.addr_stack.push(self.current_address);
    }

    fn load_addr(&mut self) {
        let addr = self.addr_stack.pop().unwrap_or(0);
        self.go_to(addr);
    }

    fn dismiss_addr(&mut self) {
        self.addr_stack.pop();
    }

    /// Saves the current address and tries to perform the action.
    /// If it returns `Some`, it will dismiss the saved address
    /// and continue normally, but if it receives `None`, it will
    /// backtrack to the saved address.
    ///
    /// NOTE: would be beneficial to use the [Try API](https://doc.rust-lang.org/std/ops/trait.Try.html)
    /// once it's stable.
    pub fn with_saved<F, T>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut Self) -> Option<T>,
    {
        self.save_addr();
        let res = f(self);
        if res.is_some() {
            self.dismiss_addr();
        } else {
            self.load_addr();
        }
        res
    }

    /// Goes to `addr` and performs `action`.
    /// Then goes back to the previous spot.
    pub fn with_cell<F, T>(&mut self, addr: usize, action: F) -> T
    where
        F: Fn(&mut Self) -> T,
    {
        self.save_addr();
        self.go_to(addr);
        let result = action(self);
        self.load_addr();
        result
    }

    pub fn add_comment(&mut self, comm: &str) {
        self.with_temps(1, |ctx| {
            let addr = ctx.get_temp(0).unwrap();
            ctx.with_saved(|ctx| -> Option<()> {
                ctx.newline();
                ctx.go_to(addr);
                ctx.zero_current_cell();
                ctx.loop_with(|ctx| {
                    for byte in comm.bytes() {
                        ctx.output.push((byte, 1));
                    }
                });
                ctx.newline();
                None
            });
        })
    }

    /// appends a newline into the output.
    fn newline(&mut self) {
        if self.output.last().filter(|(x, _)| x == &b'\n').is_none() {
            self.output.push((b'\n', 1));
        }
    }

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
        let command = command as u8;
        if let Some(last) = self.output.last_mut().filter(|(x, _)| x == &command) {
            last.1 += amount;
        } else {
            self.output.push((command, amount));
        }
    }

    fn dealloc_temps(&mut self, n: usize) {
        for _ in 0..=n {
            self.temporaries.pop();
        }
        self.size -= n;
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
        self.add_comment(&format!("print text: {:?}", st));
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
        for (i, msg) in messages.iter().enumerate() {
            match msg {
                RValue::Literal(st) => self.compile_print_text(st.as_ref()),
                RValue::Constant(x) => self.compile_print_text(x.to_string().as_ref()),
                RValue::Reference(_) => todo!("message variables"),
            }
            if i != messages.len() - 1 {
                self.compile_print_text(" ");
            }
        }
    }

    fn compile_inst(&mut self, inst: Instruction) {
        self.add_comment(&format!("compile instruction: {}", inst));
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

pub fn compile_with_filename(file_name: &str, mut ast: Vec<ASTNode>) -> BFSource {
    let mut compiler = Compiler::default();
    compiler.add_comment(&format!("BEGIN FILE {:?}", file_name));
    for node in ast.drain(..) {
        compiler.compile_ast(node);
    }
    compiler.add_comment(&format!("END FILE {:?}", file_name));
    compiler.release()
}

#[cfg(test)]
mod tests {
    use super::*;
}
