use std::convert::TryFrom;
#[derive(Debug)]
pub struct WithPosition<T> {
    pub data: T,
    pub position: Position,
}

impl<T> WithPosition<T> {
    pub fn map<F, U>(self, func: F) -> WithPosition<U>
    where
        F: Fn(T) -> U,
    {
        WithPosition {
            data: func(self.data),
            position: self.position,
        }
    }

    pub fn into_data(self) -> T {
        self.data
    }
}

impl<T: Clone> Clone for WithPosition<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            position: self.position,
        }
    }
}

impl<T: Copy> Copy for WithPosition<T> {}

impl<T: PartialEq> PartialEq for WithPosition<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T, E> WithPosition<Result<T, E>> {
    pub fn into_err(self) -> Result<T, WithPosition<E>> {
        let position = self.position;
        self.data.map_err(|data| WithPosition { data, position })
    }

    pub fn into_ok(self) -> Result<WithPosition<T>, E> {
        let position = self.position;
        self.data.map(|data| WithPosition { data, position })
    }

    pub fn diverge(self) -> Result<WithPosition<T>, WithPosition<E>> {
        let position = self.position;
        self.data.map_or_else(
            |data| Err(WithPosition { data, position }),
            |data| Ok(WithPosition { data, position }),
        )
    }

    pub fn from_ok(x: WithPosition<T>) -> Self {
        let data = Ok(x.data);
        let position = x.position;
        WithPosition { data, position }
    }

    pub fn from_err(e: WithPosition<E>) -> Self {
        let data = Err(e.data);
        let position = e.position;
        WithPosition { data, position }
    }
}

impl<T, E> From<Result<WithPosition<T>, WithPosition<E>>> for WithPosition<Result<T, E>> {
    fn from(res: Result<WithPosition<T>, WithPosition<E>>) -> Self {
        res.map_or_else(Self::from_err, Self::from_ok)
    }
}

impl<T> From<Option<WithPosition<T>>> for WithPosition<Option<T>> {
    fn from(opt: Option<WithPosition<T>>) -> Self {
        match opt {
            Some(p) => {
                let data = Some(p.data);
                let position = p.position;
                WithPosition { data, position }
            }
            None => WithPosition {
                data: None,
                position: Position::default(),
            },
        }
    }
}

impl<T> AsRef<T> for WithPosition<T> {
    fn as_ref(&self) -> &T {
        &self.data
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Literal(String),
}

impl Token {
    pub fn as_keyword(&self) -> Option<Keyword> {
        match self {
            Self::Keyword(kw) => Some(*kw),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&String> {
        match self {
            Self::Literal(st) => Some(st),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExpectedToken {
    Keyword(Keyword),
    Literal,
    AnyToken,
}

impl Into<ExpectedToken> for &Token {
    fn into(self) -> ExpectedToken {
        match self {
            Token::Keyword(kw) => ExpectedToken::Keyword(*kw),
            Token::Literal(_) => ExpectedToken::Literal,
        }
    }
}

impl PartialEq for ExpectedToken {
    fn eq(&self, rhs: &ExpectedToken) -> bool {
        match (self, rhs) {
            (Self::Keyword(xk), Self::Keyword(yk)) => xk == yk,
            (Self::Literal, Self::Literal) => true,
            // this way I can impl expect_token(AnyToken) and
            // let it pass.
            (Self::AnyToken, _) | (_, Self::AnyToken) => true,
            _ => false,
        }
    }
}

impl Eq for ExpectedToken {}

impl PartialEq<ExpectedToken> for &Token {
    fn eq(&self, rhs: &ExpectedToken) -> bool {
        <&Token as Into<ExpectedToken>>::into(self) == *rhs
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Msg,
}
#[macro_export]
macro_rules! impl_todo {
    ($st:ident) => {
        todo!(concat!("impl ", stringify!($st)))
    };
}

impl TryFrom<&str> for Keyword {
    type Error = ();
    fn try_from(st: &str) -> Result<Self, Self::Error> {
        match st {
            "msg" => Ok(Self::Msg),
            "set" => impl_todo!(set),
            "inc" => impl_todo!(inc),
            "dec" => impl_todo!(dec),
            "add" => impl_todo!(add),
            "mul" => impl_todo!(mul),
            "divmod" => impl_todo!(divmod),
            "div" => impl_todo!(div),
            "mod" => impl_todo!(mod),
            "cmp" => impl_todo!(cmp),
            "a2b" => impl_todo!(a2b),
            "b2a" => impl_todo!(b2a),
            "lset" => impl_todo!(lset),
            "lget" => impl_todo!(lget),
            "ifeq" => impl_todo!(ifeq),
            "ifneq" => impl_todo!(ifneq),
            "wneq" => impl_todo!(wneq),
            "proc" => impl_todo!(proc),
            "end" => impl_todo!(blocc),
            "call" => impl_todo!(call),
            "read" => impl_todo!(read),
            "rem" => impl_todo!(rem),
            _ => Err(()),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Default for Position {
    #[inline(always)]
    fn default() -> Self {
        Self { line: 1, col: 0 }
    }
}
impl Position {
    #[inline(always)]
    pub fn newline(&mut self) {
        self.line += 1;
        self.col = 0;
    }

    #[inline(always)]
    pub fn increment(&mut self) {
        self.col += 1;
    }
}
