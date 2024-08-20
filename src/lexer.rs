use std::collections::HashMap;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenTyp 
{
  TokenLet,
  TokenName,
  TokenNumLiteral,
  TokenLParenthesis, // (
  TokenRParenthesis, // )
  TokenEq,
  TokenAssign,
  TokenBitOr,
  TokenChar,
  TokenAdd,
  TokenSub,
  TokenMul,
  TokenDiv,
  TokenStatementEnd,
  TokenRustlAnnotation, // @$name
  TokenFnDecl,
  TokenComma, // ,
  TokenColon, // :
  TokenStringLiteral, // ".*"
}

#[derive(PartialEq,Debug)]
pub struct Token 
{
  pub token_t: TokenTyp,
  pub token_value: String,
}

lazy_static!  {
  static ref KEYWORD_MAP : HashMap<String, TokenTyp> = 
  HashMap::from(
    [
    (String::from("let") , TokenTyp::TokenLet),
    (String::from("fn") , TokenTyp::TokenFnDecl),
    ]
  );
}

fn get_string_keyword_typ(string: &String) -> Option<&TokenTyp>
{
  KEYWORD_MAP.get(string)
}

impl Token 
{
  pub fn new(tt : TokenTyp, tv: String) -> Token {
    Self { token_t : tt,  token_value: tv  }
  }

  pub fn parse_from_string(buffer:& [u8]) -> Result<(Token, usize), ()> {
    if buffer.len() <= 0 {
      return Err(());  
    }

    let first_char = buffer[0];

    // parse token name
    if first_char.is_ascii_alphabetic() || first_char == b'_' 
    {
      return Self::parse_name(buffer);
    }

    // parse number literal
    if first_char.is_ascii_digit() {
      return Self::parse_numeric(buffer); 
    }

    if first_char == b'\"' {
      let (token, end) = Self::parse_string_literal(buffer)?;
      let string_literal_value = token.token_value.splitn(3, "\"").skip(1).next().unwrap().to_string();
      return Ok((Token{token_value: string_literal_value, ..token }, end)) ; 
    }

    // parse char literal
    if first_char == b'\'' {
      if let Some(token_with_size) = Self::try_parse_char_literal(buffer){
        return Ok(token_with_size);
      }
    }

    // ctrl char | operator
    match first_char {
      b'='  => Self::one_char_token(TokenTyp::TokenAssign, first_char), 
      b'|'  => Self::one_char_token(TokenTyp::TokenBitOr, first_char),
      b'*'  => Self::one_char_token(TokenTyp::TokenMul, first_char),
      b'/'  => Self::one_char_token(TokenTyp::TokenDiv, first_char),
      b'+'  => Self::one_char_token(TokenTyp::TokenAdd, first_char),
      b'-'  => Self::one_char_token(TokenTyp::TokenSub, first_char),
      b';'  => Self::one_char_token(TokenTyp::TokenStatementEnd, first_char),
      b'('  => Self::one_char_token(TokenTyp::TokenLParenthesis, first_char),
      b')'  => Self::one_char_token(TokenTyp::TokenRParenthesis, first_char),
      b','  => Self::one_char_token(TokenTyp::TokenComma, first_char),
      b':'  => Self::one_char_token(TokenTyp::TokenColon, first_char),
      b'@'  => Self::parse_rustl_annotation(buffer),
      _ => Err(())
    }
  }

  fn one_char_token(tt: TokenTyp, c: u8) -> Result<(Token, usize), ()> {
    Ok((Token::new(tt, unsafe { String::from_utf8_unchecked(vec![c]) }), 1))
  }

  fn char_is_token_name(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
  }

  fn try_parse_char_literal(buffer:& [u8]) -> Option<(Token, usize)> {
    if buffer.len() < 3 {
      return None;
    }

    let mut read_cursor = 2;
    loop {

      if read_cursor >= buffer.len() 
      {
        break;
      }

      if buffer[read_cursor] == b'\'' {
        break;
      }

      read_cursor += 1;
    }

    if read_cursor >= buffer.len() || buffer[read_cursor] != b'\'' {
      return None;
    }

    match Self::split_buffer_to_token(buffer, read_cursor + 1, TokenTyp::TokenChar)  {
      Ok(token_with_size) => Some(token_with_size),
      Err(_) => None
    }
  }

  fn parse_string_literal(buffer:& [u8]) -> Result<(Token, usize), ()>  {
    let is_string_literal_end = |c: u8| { c == b'\"' };
    match Self::parse_till(buffer, 1, is_string_literal_end)  {
      None => Err(()),
      Some(end) => 
        Self::split_buffer_to_token(buffer, end + 1, TokenTyp::TokenStringLiteral)
    }
  }

  fn parse_name(buffer:& [u8]) -> Result<(Token, usize), ()> {
    let mut read_cursor = 1;
    loop 
    {
      if read_cursor >= buffer.len() 
      {
        break;
      }

      let cur_char = buffer[read_cursor];
      if cur_char.is_ascii_whitespace() || !Self::char_is_token_name(cur_char)
      {
        break;
      }

      read_cursor = read_cursor + 1;
    }

    match String::from_utf8(buffer.split_at(read_cursor).0.to_vec()) {
      Ok(token_name) => { 
        if let Some(typ) = get_string_keyword_typ(&token_name) {
          Ok((Token::new(*typ, token_name), read_cursor))
        }
        else 
        {
          Ok((Token::new(TokenTyp::TokenName, token_name), read_cursor))
        }
      } 
      Err(_) => Err(())
    }
  }

  fn parse_rustl_annotation(buffer:& [u8]) -> Result<(Token, usize), ()> 
  {
    let not_match_name = |c: u8| { c.is_ascii_whitespace() || !Self::char_is_token_name(c) };
    match Self::parse_till(buffer, 1, not_match_name)  {
      None => Err(()),
      Some(end) => 
        Self::split_buffer_to_token(buffer, end, TokenTyp::TokenRustlAnnotation)
    }
  }

  fn parse_till<F>(buffer:& [u8], start: usize, fillter: F) -> Option<usize>
  where F: Fn(u8) -> bool {
    let mut read_cursor = start;
    loop 
    {
      if read_cursor >= buffer.len() 
      {
        break;
      }

      let cur_char = buffer[read_cursor];
      if fillter(cur_char)
      {
        break;
      }

      read_cursor = read_cursor + 1;
    }

    if read_cursor == start { None } 
    else { Some(read_cursor) }
  }

  fn parse_numeric(buffer:& [u8]) -> Result<(Token, usize), ()> {
    let mut read_cursor = 1;

    let read_numeric = |r_cursor : usize| -> usize
    {

      let mut cursor = r_cursor;
      loop 
      {
        if cursor >= buffer.len() 
        {
          break;
        }

        let cur_char = buffer[cursor];
        if !cur_char.is_ascii_digit()
        {
          break;
        }

        cursor = cursor + 1;
      }

      cursor
    };

    read_cursor =  read_numeric(read_cursor);

    if read_cursor < buffer.len() && buffer[read_cursor] == b'.'
    {
      read_cursor = read_numeric(read_cursor + 1)
    }

    Self::split_buffer_to_token(buffer, read_cursor, TokenTyp::TokenNumLiteral)
  }

  fn split_buffer_to_token(buffer:& [u8], end: usize, tt: TokenTyp) -> Result<(Token, usize), ()> {
    match String::from_utf8(buffer.split_at(end).0.to_vec()) {
      Ok(token_name) => Ok((Token::new(tt, token_name), end)), 
      Err(_) => Err(())
    }
  }
}
