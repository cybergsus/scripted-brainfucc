use super::common::*;
use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum LexError {
    UnexpectedEOF,
    Other(String), // TODO(#1): expand some lexer errors to their own useful enum.
}

impl fmt::Display for LexError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // NOTE: unreachable when lexer is not availble from lib,
            // as only one who can access it is `Parser`, who converts the error
            Self::UnexpectedEOF => write!(fmt, "Unexpected EOF"),
            Self::Other(message) => write!(fmt, "{}", message),
        }
    }
}

// pub type LexResult<T> = Result<WithPosition<T>, WithPosition<LexError>>;
pub type LexResult<T> = Result<WithPosition<T>, WithPosition<LexError>>;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    current_pos: Position,
    last_pointers: Vec<(&'a str, Position)>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            current_pos: Position::default(),
            last_pointers: vec![(input, Position::default())],
        }
    }

    fn save_pointer(&mut self) {
        self.last_pointers.push((self.input, self.current_pos));
    }

    fn load_pointer(&mut self) {
        // reserve always the last one
        let (input, pos) = if self.last_pointers.len() == 1 {
            self.last_pointers[0]
        } else {
            self.last_pointers
                .pop()
                .unwrap_or_else(|| self.last_pointers[0])
        };
        self.input = &input;
        self.current_pos = pos;
    }

    fn dismiss_pointer(&mut self) {
        if self.last_pointers.len() > 1 {
            self.last_pointers.pop();
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().next()
    }

    fn advance_input(&mut self, ch: char) {
        // advance the current character.
        self.input = &self.input[ch.len_utf8()..];
        match ch {
            // update the position
            '\n' => self.current_pos.newline(),
            // CRLF exception
            '\r' if matches!(self.peek_char(), Some('\n')) => self.advance_input('\n'),
            _ => self.current_pos.increment(),
        }
    }

    fn skip_current(&mut self) {
        self.peek_char().map(|x| self.advance_input(x));
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char().filter(|&c| c.is_whitespace()) {
            self.advance_input(c);
        }
    }

    fn expect(&self, pred: bool, explain: String) -> Result<(), LexError> {
        if !pred {
            Err(LexError::Other(explain))
        } else {
            Ok(())
        }
    }

    pub fn skip_till_eol(&mut self) {
        while self.skip_if(|&c| c != '\n').is_some() {}
    }

    fn expect_some_char(&self) -> Result<char, LexError> {
        self.peek_char().ok_or(LexError::UnexpectedEOF)
    }

    /// Uses a predicate to match the next character.
    /// If an explanation is given, it will use it. Otherwise,
    /// it will use a default message with the name of the provided
    /// function.
    fn expect_char_with_predicate<F>(
        &self,
        pred: F,
        explanation: Option<String>,
    ) -> Result<char, LexError>
    where
        F: FnOnce(&char) -> bool,
    {
        self.expect_some_char().and_then(|x| {
            if !pred(&x) {
                Err(LexError::Other(match explanation {
                    Some(e) => e,
                    // use the name of the function.
                    None => format!(
                        "Predicate `{}` didn't match for char {:?}",
                        core::any::type_name::<F>(),
                        x
                    ),
                }))
            } else {
                Ok(x)
            }
        })
    }

    /// Expects the next character to be `ch`.
    fn expect_char(&self, ch: char) -> Result<char, LexError> {
        self.expect_some_char().and_then(|x| {
            if x != ch {
                Err(LexError::Other(format!(
                    "Expected char {}, found instead {}",
                    ch, x
                )))
            } else {
                Ok(x)
            }
        })
    }

    pub fn expect_whitespace(&self) -> Result<(), WithPosition<LexError>> {
        self.expect_char(' ')
            .map(|_| ())
            .map_err(|data| WithPosition {
                data,
                position: self.current_pos,
            })
    }

    fn skip_if<F>(&mut self, pred: F) -> Option<char>
    where
        F: FnOnce(&char) -> bool,
    {
        self.peek_char().filter(pred).map(|x| {
            self.skip_current();
            x
        })
    }

    fn with_pos<F, T>(&mut self, f: F) -> WithPosition<T>
    where
        F: Fn(&mut Self) -> T,
    {
        let position = self.current_pos;
        let data = f(self);
        WithPosition { data, position }
    }

    /// Saves the current input pointer and executes the function.
    /// If the function returns a result, it dismisses the pointer.
    ///
    pub fn with_saved<F, T>(&mut self, fnc: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> Option<T>,
    {
        self.save_pointer();
        let result = fnc(self);
        match result {
            Some(_) => self.dismiss_pointer(),
            None => self.load_pointer(),
        }
        result
    }

    /// Lexes a string literal.
    fn lex_string(&mut self) -> Result<String, LexError> {
        self.expect_char('"').map(|x| self.advance_input(x))?;

        let mut st = String::new();

        while let Some(c) = self.skip_if(|&c| c != '"') {
            let c = match c {
                // escapes.
                '\\' => self.expect_some_char().and_then(|x| match x {
                    '"' => {
                        self.skip_current();
                        Ok('"')
                    }
                    '\\' => {
                        self.skip_current();
                        Ok('\\')
                    }
                    't' => {
                        self.skip_current();
                        Ok('\t')
                    }
                    'n' => {
                        self.skip_current();
                        Ok('\n')
                    }
                    'e' => {
                        self.skip_current();
                        Ok('\x1b')
                    }
                    'x' => {
                        self.skip_current();
                        let mut expect_hex = || {
                            self.expect_char_with_predicate(
                                char::is_ascii_hexdigit,
                                Some("Hex escape needs hex characters".into()),
                            )
                            .map(|x| {
                                self.skip_current();
                                hex_to_int(x)
                            })
                        };
                        let a = expect_hex()?;
                        let b = expect_hex()?;
                        let mut char_code = a << 4 | b;
                        if let Some(c) = self.skip_if(char::is_ascii_hexdigit) {
                            char_code = char_code << 4 | hex_to_int(c);

                            if let Some(d) = self.skip_if(char::is_ascii_hexdigit) {
                                char_code = char_code << 4 | hex_to_int(d);
                            }
                        }
                        char::from_u32(char_code).ok_or_else(|| {
                            LexError::Other(format!(
                                "Hex escape does not lead to UTF-8 character: {}",
                                char_code
                            ))
                        })
                    }
                    _ => Err(LexError::Other(format!("Bad escape character: {}", x))),
                }),

                c => Ok(c),
            }?;
            st.push(c);
        }

        self.expect_char('"').map(|x| self.advance_input(x))?;
        Ok(st)
    }

    fn get_while<F>(&mut self, predicate: F) -> String
    where
        F: Fn(&char) -> bool,
    {
        let mut st = String::new();
        while let Some(c) = self.skip_if(&predicate) {
            st.push(c);
        }
        st
    }

    /// Tries to lex a keyword. Backtracks
    /// if it can't find it.
    fn lex_kw(&mut self) -> Option<Keyword> {
        self.with_saved(|lexer| {
            match Keyword::try_from(lexer.get_while(char::is_ascii_alphabetic).as_str()) {
                Ok(kw) => Some(kw),
                Err(_) => None,
            }
        })
    }

    fn skip_literal(&mut self, lit: &str) -> bool {
        let chars = lit.chars();
        self.with_saved(|lexer| {
            for ch in chars {
                if !lexer.skip_if(|&x| x == ch).is_some() {
                    return None;
                }
            }
            return Some(());
        })
        .is_some()
    }

    // TODO: numbers
    fn lex_hex_num(&mut self) -> Result<Option<String>, LexError> {
        if !self.skip_literal("0x") {
            return Ok(None);
        }
        let st = self.get_while(char::is_ascii_hexdigit);
        if st.len() == 0 {
            Err(LexError::Other(format!("Unterminated hex constant")))
        } else {
            Ok(Some("0x".to_owned() + &st))
        }
    }

    fn lex_num(&mut self) -> Result<String, LexError> {
        if let Some(st) = self.lex_hex_num()? {
            Ok(st)
        } else {
            self.expect_char_with_predicate(
                |x| x.is_ascii_digit() && x > &'1',
                Some("Numeric literals need at least one non-zero decimal digit".into()),
            )?;
            Ok(self.get_while(char::is_ascii_digit))
        }
    }

    fn _next_token(&mut self) -> Result<Option<Token>, LexError> {
        self.skip_whitespace();
        let next_char = match self.peek_char() {
            Some(c) => c,
            None => return Ok(None),
        };
        match next_char {
            '"' => self.lex_string().map(|x| Some(Token::Literal(x))),
            c if c.is_ascii_alphabetic() => {
                if let Some(kw) = self.lex_kw() {
                    Ok(Some(Token::Keyword(kw)))
                } else {
                    // TODO: names
                    Err(LexError::Other("names are yet to be implemented".into()))
                }
            }
            c if c.is_ascii_digit() => self.lex_num().map(|x| Some(Token::Constant(x))),
            _ => Err(LexError::Other(format!(
                "Unexpected character: {:?}",
                next_char
            ))),
        }
    }

    pub fn next_token(&mut self) -> LexResult<Option<Token>> {
        self.with_pos(|lexer| lexer._next_token()).diverge()
    }
}

fn hex_to_int(c: char) -> u32 {
    match c {
        '0'..='9' => c as u32 - 0x30,
        'a'..='f' => c as u32 + 10 - ('a' as u32),
        'A'..='F' => c as u32 + 10 - ('A' as u32),
        _ => unreachable!(),
    }
}

impl<'a> AsRef<str> for Lexer<'a> {
    fn as_ref(&self) -> &'a str {
        self.input
    }
}

#[cfg(test)]
mod test {
    // use std::io::Result as IORes;
    use super::*;
    #[test]
    fn advance_input() {
        let mut lexer = Lexer::new("Hello!");
        lexer.skip_current();
        assert_eq!(lexer.input, "ello!");
    }
    #[test]
    fn newlines() {
        let mut lexer = Lexer::new("\r\nxd");
        lexer.skip_current();
        assert_eq!(lexer.input, "xd");
        assert_eq!(lexer.current_pos, Position { line: 2, col: 0 });
    }
    #[test]
    fn skip_whitespace() {
        let mut lexer = Lexer::new("                                        hello!");
        lexer.skip_whitespace();
        assert_eq!(lexer.input, "hello!");
    }
    #[test]
    fn skip_if() {
        let mut lexer = Lexer::new("hello!");
        lexer.skip_if(|_| true);
        assert_eq!(lexer.input, "ello!");
    }
    #[test]
    fn lex_string() -> Result<(), LexError> {
        let mut lexer = Lexer::new("\"hey what's up!\\n\\t\\x30 I'm just testing the string!\"hi");
        let string = lexer.lex_string()?;
        assert_eq!(string, "hey what's up!\n\t0 I'm just testing the string!");
        assert_eq!(lexer.input, "hi");
        Ok(())
    }
    #[test]
    fn lex_kw() -> Result<(), WithPosition<LexError>> {
        let mut lexer = Lexer::new("msg \"hello, world!\\n\"");
        assert_eq!(lexer.next_token()?.data, Some(Token::Keyword(Keyword::Msg)));
        assert_eq!(
            lexer.next_token()?.data,
            Some(Token::Literal("hello, world!\n".into()))
        );
        Ok(())
    }
    #[test]
    fn lex_num() -> Result<(), WithPosition<LexError>> {
        let mut lexer = Lexer::new("42 432 0x00 0x34");
        assert_eq!(lexer.next_token()?.data, Some(Token::Constant("42".into())));
        assert_eq!(
            lexer.next_token()?.data,
            Some(Token::Constant("432".into()))
        );
        assert_eq!(
            lexer.next_token()?.data,
            Some(Token::Constant("0x00".into()))
        );
        assert_eq!(
            lexer.next_token()?.data,
            Some(Token::Constant("0x34".into()))
        );
        Ok(())
    }
}
