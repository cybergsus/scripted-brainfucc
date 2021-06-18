use super::ast::*;
use super::common::*;
use super::lexer::Lexer;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Lexical(super::lexer::LexError),
    UnexpectedEOF,
    ExpectedToken(ExpectedToken),
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
                    if <&Token as Into<ExpectedToken>>::into(tok) != expected {
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
        self.expect_token(ExpectedToken::Literal).map(|tok| {
            tok.map(|t| match t {
                Token::Literal(st) => st,
                _ => unreachable!(),
            })
            .into_data()
            .clone()
        })
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
        } else {
            if self.pos == 0 {
                Position::default()
            } else {
                self.pos -= 1;
                let pos = self.peek_token()?.position;
                self.pos += 1;
                pos
            }
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

    fn parse_rvalue(&mut self) -> ParseResult<RValue> {
        // TODO: multiple parsers
        let literal = self.expect_literal()?;
        Ok(RValue::from(literal))
    }

    // TODO: parse rvalue

    fn parse_msg(&mut self) -> ParseResult<Instruction> {
        self.expect_keyword(Keyword::Msg)?;

        self.next();

        let messages = self.one_or_more(Self::parse_rvalue)?;

        Ok(Instruction::Msg { messages })
    }

    fn with_saved<F, T>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut Self) -> Option<T>,
    {
        self.save_pos();
        let res = f(self);
        if res.is_some() {
            self.dismiss_pos();
        } else {
            self.load_pos();
        }
        res
    }

    fn one_or_more<F, T>(&mut self, f: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
        T: std::fmt::Debug,
    {
        let mut vec = Vec::new();
        vec.push(f(self)?);
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

    fn optionally<F, T>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        self.with_saved(|lexer| {
            let result = f(lexer);
            result.map_or(None, Some)
        })
    }

    fn parse_instruction(&mut self) -> ParseResult<Instruction> {
        let parsed = match self.expect_keywords(&[Keyword::Msg])? {
            Keyword::Msg => self.parse_msg(),
        };
        //        self.lexer.skip_till_eol();
        parsed
    }

    fn parse_ast_node(&mut self) -> ParseResult<ASTNode> {
        self.parse_instruction().map(ASTNode::Instruction)
    }

    pub fn parse_ast(&mut self) -> ParseResult<Vec<ASTNode>> {
        let mut vec = Vec::new();
        while let Some(tok) = self.peek_token()?.into_data() {
            let node = self.parse_ast_node()?;
            vec.push(node);
        }
        Ok(vec)
    }
}

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
        assert_eq!(
            parser.peek_token().err().map(WithPosition::into_data),
            Some(ParseError::Lexical(LexError::Other(
                "names are yet to be implemented".into()
            )))
        );
        Ok(())
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
}
