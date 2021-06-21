use super::ast::*;
use super::common::*;
use super::lexer::Lexer;
use std::fmt;
use std::str::FromStr;
use std::usize;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Lexical(super::lexer::LexError),
    UnexpectedEOF,
    ExpectedToken(ExpectedToken),
    FailedNumParse(<usize as FromStr>::Err, String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lexical(lerr) => write!(fmt, "lexical error: {}", lerr),
            Self::ExpectedToken(e) => write!(fmt, "expected token: {}", e),
            Self::UnexpectedEOF => write!(fmt, "Unexpected EOF"),
            Self::FailedNumParse(e, s) => write!(fmt, "Failed to parse {:?} into usize: {}", s, e),
        }
    }
}

use super::lexer::LexError;
impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        match err {
            LexError::UnexpectedEOF => Self::UnexpectedEOF,
            _ => Self::Lexical(err),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    tok_stack: Vec<WithPosition<Token>>,
    pos: usize,
    pos_stack: Vec<usize>,
    lexer: Lexer<'a>,
    at_eof: bool,
}

pub type ParseResult<T> = Result<T, WithPosition<ParseError>>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tok_stack: Vec::new(),
            pos_stack: Vec::new(),
            pos: 0,
            at_eof: false,
            lexer: Lexer::new(input),
        }
    }

    fn peek_token(&mut self) -> ParseResult<WithPosition<Option<&Token>>> {
        if self.pos == self.tok_stack.len() {
            let next_tok = {
                let opt_tok: WithPosition<Option<Token>> = self
                    .lexer
                    .next_token()
                    .map_err(|e| e.map(ParseError::from))?;
                match opt_tok.data {
                    Some(data) => WithPosition {
                        position: opt_tok.position,
                        data,
                    },
                    None => {
                        self.at_eof = true;
                        return Ok(WithPosition::from(None));
                    }
                }
            };
            self.tok_stack.push(next_tok);
            self.peek_token()
        } else {
            let pos_tok = {
                let WithPosition { data, position } = &self.tok_stack[self.pos];
                WithPosition {
                    data: Some(data),
                    position: *position,
                }
            };
            Ok(pos_tok)
        }
    }

    #[inline]
    fn get_last_pos(&mut self) -> usize {
        self.pos_stack.pop().unwrap_or(0)
    }

    fn next(&mut self) {
        if !self.at_eof {
            self.pos += 1;
        }
    }

    fn save_pos(&mut self) {
        self.pos_stack.push(self.pos);
    }

    fn load_pos(&mut self) {
        self.pos = self.pos_stack.pop().unwrap_or(0);
    }

    fn dismiss_pos(&mut self) {
        self.pos_stack.pop();
    }

    fn maybe_token(
        &mut self,
        expected: ExpectedToken,
    ) -> ParseResult<WithPosition<Option<&Token>>> {
        self.peek_token()
            .map(|opt| opt.map(|opt| opt.filter(|&v| v == expected)))
    }

    fn expect_token(&mut self, expected: ExpectedToken) -> ParseResult<WithPosition<&Token>> {
        let res = self.peek_token()?;
        res.map(|x| {
            x.map_or_else(
                || Err(ParseError::UnexpectedEOF),
                |tok| {
                    if tok != expected {
                        Err(ParseError::ExpectedToken(expected))
                    } else {
                        Ok(tok)
                    }
                },
            )
        })
        .diverge()
    }

    fn maybe_literal(&mut self) -> ParseResult<Option<&String>> {
        self.maybe_token(ExpectedToken::Literal).map(|opt_tok| {
            opt_tok
                .map(|opt| opt.and_then(Token::as_literal))
                .into_data()
        })
    }

    fn expect_literal(&mut self) -> ParseResult<String> {
        self.expect_token(ExpectedToken::Literal)
            .map(|tok| tok.into_data().as_literal().unwrap().clone())
    }

    fn has_keyword(&mut self, kw: Keyword) -> ParseResult<bool> {
        self.maybe_token(ExpectedToken::Keyword(kw)).map(|opt_tok| {
            opt_tok
                .map(|opt| opt.and_then(Token::as_keyword).is_some())
                .into_data()
        })
    }

    fn expect_keyword(&mut self, kw: Keyword) -> ParseResult<()> {
        self.expect_token(ExpectedToken::Keyword(kw)).map(|_| ())
    }

    fn last_position(&mut self) -> ParseResult<Position> {
        let tok = self.peek_token()?;
        Ok(if tok.data.is_some() {
            tok.position
        } else if self.pos == 0 {
            Position::default()
        } else {
            self.pos -= 1;
            let pos = self.peek_token()?.position;
            self.pos += 1;
            pos
        })
    }

    // expects one of keywords
    fn expect_keywords(&mut self, kws: &[Keyword]) -> ParseResult<Keyword> {
        for kw in kws.iter().cloned() {
            if self.has_keyword(kw)? {
                return Ok(kw);
            }
        }
        let position = self.last_position()?;

        Err(WithPosition {
            data: ParseError::ExpectedToken(ExpectedToken::Keyword(kws[0])),
            position,
        })
    }

    fn expect_constant(&mut self) -> ParseResult<String> {
        self.expect_token(ExpectedToken::Constant)
            .map(|tok| tok.into_data().as_constant().unwrap().clone())
    }

    fn parse_num(&mut self) -> ParseResult<usize> {
        let num_st = self.expect_constant()?;
        num_st.parse::<usize>().or_else(|x| {
            let data = ParseError::FailedNumParse(x, num_st);
            let position = self.last_position()?;
            Err(WithPosition { data, position })
        })
    }

    fn parse_rvalue(&mut self) -> ParseResult<RValue> {
        let parse_literal = |parser: &mut Self| parser.expect_literal().map(RValue::from);
        let parse_const = |parser: &mut Self| parser.parse_num().map(RValue::from);

        let res = self.try_one(parse_literal);
        if res.is_err() {
            parse_const(self)
        } else {
            res
        }
    }

    fn parse_msg(&mut self) -> ParseResult<Instruction> {
        self.expect_keyword(Keyword::Msg)?;

        self.next();

        let messages = self.one_or_more(Self::parse_rvalue)?;

        Ok(Instruction::Msg { messages })
    }

    /// Runs a parser. If the parser fails,
    /// it acts as it didn't consume input.
    /// returns the result of the parser.
    fn try_one<F, T>(&mut self, parser: F) -> ParseResult<T>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        self.save_pos();
        let res = parser(self);
        if res.is_err() {
            self.load_pos();
        } else {
            self.dismiss_pos();
        }
        res
    }

    fn one_or_more<F, T>(&mut self, f: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
        T: std::fmt::Debug,
    {
        let mut vec = vec![f(self)?];
        self.next();
        vec.extend(std::iter::from_fn(|| {
            self.optionally(|ctx| {
                f(ctx).map(|x| {
                    ctx.next();
                    x
                })
            })
        }));
        Ok(vec)
    }

    /// Returns Ok(None) if the parser failed but didn't consume any input.
    /// Otherwise it returns the result.
    ///
    fn optionally<F, T>(&mut self, parser: F) -> Option<T>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
        T: std::fmt::Debug,
    {
        self.try_one(parser).ok()
    }

    fn parse_instruction(&mut self) -> ParseResult<Instruction> {
        let parsed = match self.expect_keywords(&[Keyword::Msg])? {
            Keyword::Msg => self.parse_msg(),
        };
        self.lexer.eol().map_err(|x| x.map(ParseError::from))?;
        //        self.lexer.skip_till_eol();
        parsed
    }

    fn parse_ast_node(&mut self) -> ParseResult<ASTNode> {
        self.parse_instruction().map(ASTNode::Instruction)
    }

    pub fn parse_ast(mut self) -> ParseResult<Vec<ASTNode>> {
        let mut vec = Vec::new();

        while self.peek_token()?.into_data().is_some() {
            let node = self.parse_ast_node()?;
            vec.push(node);
        }
        Ok(vec)
    }
}

// FIXME(#10): Error in file "test.bfs": parser error: lexical error: Unexpected character: '\n' @ L1:94
// > cat test.bfs
// < msg "Solution to everything is: " 42
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_token() -> ParseResult<()> {
        let mut parser = Parser::new("msg \"hi\" xd");
        assert_eq!(
            parser.peek_token()?.data,
            Some(&Token::Keyword(Keyword::Msg))
        );
        parser.next();
        assert_eq!(
            parser.peek_token()?.data,
            Some(&Token::Literal("hi".into()))
        );
        parser.next();
        Ok(())
    }

    #[test]
    #[should_panic(expected = "implement names")]
    fn peek_token_todo() {
        let mut parser = Parser::new("hi");
        parser.peek_token().unwrap();
    }

    #[test]
    fn expect() -> ParseResult<()> {
        let mut parser = Parser::new("msg \"hello, world!\\n\"");
        parser.expect_keyword(Keyword::Msg)?;
        parser.next();
        let st = parser.expect_literal()?;
        assert_eq!(st, "hello, world!\n");
        Ok(())
    }
    #[test]
    fn parse_rvalue() -> ParseResult<()> {
        let mut parser = Parser::new("42 \"hello\"");
        assert_eq!(parser.parse_rvalue()?, RValue::from(42));
        parser.next();
        assert_eq!(parser.parse_rvalue()?, RValue::Literal("hello".into()));
        Ok(())
    }

    #[test]
    //#[ignore = "halts"]
    fn msg() -> ParseResult<()> {
        let parser = Parser::new("msg \"the answer is: \" 42 \"\\n\"");
        assert_eq!(
            parser.parse_ast()?,
            vec![ASTNode::Instruction(Instruction::Msg {
                messages: vec![
                    RValue::from(String::from("the answer is: ")),
                    RValue::from(42),
                    RValue::from(String::from("\n"))
                ]
            })]
        );
        let parser = Parser::new("msg \"The solution for everything is: \" 42");
        assert_eq!(
            parser.parse_ast()?,
            vec![ASTNode::Instruction(Instruction::Msg {
                messages: vec![
                    RValue::from(String::from("The solution for everything is: ")),
                    RValue::from(42),
                ]
            })]
        );
        Ok(())
    }
}
