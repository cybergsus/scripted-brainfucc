#[derive(Debug)]
pub enum ASTNode {
    Instruction(Instruction),
    // TODO: expand `ASTNode`
}

#[derive(Debug)]
pub enum Instruction {
    Msg { messages: Vec<RValue> },
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Msg { messages } => {
                write!(fmt, "msg ")?;
                for (i, m) in messages.iter().enumerate() {
                    if i > 0 {
                        write!(fmt, " ")?;
                    }
                    write!(fmt, "{}", m)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum RValue {
    Constant(usize),
    Literal(String),
    Reference(LValue),
}

impl std::fmt::Display for RValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Constant(x) => write!(fmt, "{:x}", x),
            Self::Literal(l) => write!(fmt, "{:?}", l),
            Self::Reference(l) => write!(fmt, "{}", l),
        }
    }
}

impl From<usize> for RValue {
    fn from(c: usize) -> Self {
        Self::Constant(c)
    }
}

impl From<String> for RValue {
    fn from(st: String) -> Self {
        Self::Literal(st)
    }
}

#[derive(Debug)]
pub struct LValue {
    name: String,
    offset: Option<usize>,
}

impl LValue {
    pub fn offset(&mut self) -> usize {
        *self.offset.get_or_insert(0)
    }
}

impl std::fmt::Display for LValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.name)?;
        if let Some(offset) = &self.offset {
            write!(fmt, "[{}]", offset)
        } else {
            Ok(())
        }
    }
}
