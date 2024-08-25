use std::io::BufRead;

use crate::lexer::{self, Token, TokenTyp, Location};


// --------------------------------- rustl sytax ----------------------------- 
// declaration : let $name = $expression;
// expression : $binary_operation
// binary_operation : 
//    $expression_one | 
//    $binary_operation + $expression_one | 
//    $binary_operation - $expression_one   
// expression_one : 
//    ( $expression ) | 
//    $expression_one_token | 
//    $expression_one_token * $expression_one
//    $expression_one_token / $expression_one
//    $expression_one_token
// expression_one_token : $literal | $name | $call_expression
// call_expression : $name( $expression_list )
// expression_list : $expression | $expression , $expression_list
// function_declaration: [$rustl_annotation] fn $name ( $decl_arg_list );
// decl_arg_list : 
//    $name |
//    $name , $decl_arg_list
// rustl_annotation : @$name
// funtion_definition : fn name ( $decl_arg_list ) { $function_statement_list }
// function_statement : $declaration ; | $expression ; 
// function_statement_list : 
//      $function_statement |
//      $function_statement $function_statement_list
// object_literal: { $name: $expression ... }
// object_field_get: $name.$field_name
// object_field_set: $name.$field.name = $expression;
// ---------------------------------------------------------------------------- 
// --------------------------------- example ---------------------------------- 
// let name = "string_literal";
// function(2 + 3);
// let numeric_expression = 3.1415 * 9 * 8 / 2;
// @builtin
// fn print(arg);
// fn definition(arg, arg1) {
//    let m = "a";
// }

#[derive(Debug, Clone)]
pub enum AstKind {
  ProgramAst(Vec<Box<Expr>>),
  NumericLiteral(String),
  StringLiteral(String),
  CharLiteral(String),
  Let(String, Box<Expr>),
  AstName(String),
  BinaryOp(String, Box<Expr>, Box<Expr>),
  Unary(String, Box<Expr>),
  FnDeclAst(Option<Box<Expr>>, String, Box<Expr>),
  FnDeclArgs(Vec<Box<Expr>>),
  RustlAnnotation(String),
  FnCall(String, Vec<Box<Expr>>),
  FnDefine(String, Box<Expr>, Vec<Box<Expr>>),
  ReturnExpr(Box<Expr>),
  ObjectLiteral(Vec<(String,Box<Expr>)>),
  FieldSet(Box<Expr>,String, Box<Expr>),
  FieldGet(Box<Expr>, String),
  LiteralTrue,
  LiteralFalse,
  AstNull
}

#[derive(Debug, Clone)]
pub struct Expr {
  pub kind: AstKind,
  pub loc: lexer::Location,
}

impl Expr {
  pub fn ast_null() -> Box< Expr > {
    Box::new(Expr{ kind: AstKind::AstNull, loc: Default::default() })
  }

  pub fn new(kind: AstKind ) -> Expr {
    Expr{ kind, loc: Default::default() }
  }

  pub fn new_with_location(kind: AstKind, loc: lexer::Location) -> Expr {
    Expr{kind, loc }
  }

  pub fn start(&mut self, start_pos: usize) -> () {
    self.loc.start = start_pos;
  }

  pub fn end(&mut self, end_pos: usize) -> () {
    self.loc.end = end_pos;
  }
}

pub struct Parser
{
  pub root: Option<Expr>,
}

type ParsingErr = (String, lexer::Location);

#[derive(Debug)]
pub struct ParsingCtx <'a> {
  token_cursor: usize,
  tokens: &'a Vec<lexer::Token>
}

impl <'a> ParsingCtx<'a> {
  pub fn new<'b> (tokens: &'b Vec<lexer::Token>) -> ParsingCtx<'b> {
    ParsingCtx{ token_cursor: 0, tokens}
  }

  fn get_cur_token(&self) -> Option<&Token> {
    if self.token_cursor < self.tokens.len() {
      return self.tokens.get(self.token_cursor);
    }

    None
  }

  fn consume_cur_token(&mut self) -> &mut Self {
    self.token_cursor += 1;
    self
  }

  fn has_token(&self) -> bool {
    self.token_cursor < self.tokens.len()
  }

  fn get_closed_token_position(&self) -> Option<Location>  {
    if self.token_cursor < self.tokens.len() {
      return Some(self.tokens.get(self.token_cursor).unwrap().location);
    }

    self.tokens.last().map(|t| t.location)
  }

  fn expect_cur_token(&mut self, token_typ: TokenTyp) -> Result<&Token, ParsingErr> {
    let Some(cur_token) = self.get_cur_token() else {
      return Err((
        format!("cur token not exists on token expect {token_typ:#?}"), 
        self.get_closed_token_position().unwrap_or_default()))
    };

    if cur_token.token_t == token_typ { Ok(cur_token) } 
    else { 
      Err((format!(" token is not expect ({cur_token:#?}), real token typ {token_typ:#?}"), cur_token.location)) 
    }
  }

  fn next_token_typ(&mut self, tt:&[TokenTyp]) -> bool {
    for t_idx in 0..tt.len() {
      let token_is_not_right_type = 
        !self.tokens
          .get(t_idx + self.token_cursor)
          .map_or(false, |t| { t.token_t == tt[t_idx]});
      if token_is_not_right_type {
        return false;
      }
    }
    true
  }
}


type ParsingResult = Result<Box<Expr>, ParsingErr> ;

pub fn log_err(prefix: &str,code: &[u8], start: usize, end: usize, msg: &String) {
  let mut nstart = start;
  let mut eol = false;
  loop {
    if nstart == 0 {
      break;
    }

    if code[nstart] == b'\n' {
      if eol {
        break;
      }
      eol = true;
    }

    nstart -= 1;
  }

  eol = false;
  let mut nend = end;
  loop {
    if nend >= code.len() {
      break;
    }

    if code[nend] == b'\n' {
      if eol {
        break;
      }
      eol = true;
    }

    nend += 1;
  }

  let err_code = &code[nstart..nend];
  let error_line = err_code
    .lines()
    .into_iter();

  println!("{} {},{}",prefix,start, end);
  let mut position = 0;
  let prefix_len = start - nstart;
  for line in error_line {
    let Ok(line_string) = line else {
      break;
    };
    println!("{}", line_string);
    if position < prefix_len && position + line_string.len() >= prefix_len {
      println!("{}^{}", " ".repeat(prefix_len - position), msg);
    }
    position += line_string.len();
  }
  println!("*********************************************************");
}

impl Parser
{
  pub fn new() -> Parser
  {
    Parser{root: None }
  }

  fn boxed_expression(kind: AstKind, loc: lexer::Location) -> Box<Expr> {
    Box::new(Expr::new_with_location(kind, loc))
  }

  pub fn parse(&mut self , ctx: &mut ParsingCtx, code: &[u8]) -> Result<(),()>
  {
    let mut statements: Vec<Box<Expr>> = vec![];

    loop {
      if !ctx.has_token() {
        break;
      }
      match self.top_level_parse(ctx) {
        Ok(ast) => 
          statements.push(ast),
        Err((_msg, loc)) => {
          log_err("[rustl][error] parsing error in",code, loc.start, loc.end, &_msg);
          // let start = 
          //   if loc.start > 20 { loc.start - 20 } 
          //   else { loc.start };
          
          // let end = 
          //   if loc.end + 20  < code.len() { loc.end + 20 } 
          //   else { loc.end };
          // let err_code = &code[start..end];
          // let error_line = err_code
          //   .lines()
          //   .into_iter();

          // println!("[rustl][error] parsing error in {}-{}\n", loc.start, loc.end);
          // println!("*********************************************************");
          // let mut position = 0;
          // for line in error_line {
          //   let Ok(line_string) = line else {
          //     break;
          //   };
          //   println!("{}", line_string);
          //   if position < 20 && position + line_string.len() >= 20 {
          //     println!("{}^{}", " ".repeat(20 - position), _msg);
          //   }
          //   position += line_string.len();
          // }
          // println!("*********************************************************");
          return Err(())
        }
      }
    }

    self.root = Some(Expr::new(AstKind::ProgramAst(statements) ));

    Ok(())
  }

  fn err(msg: &String, loc: lexer::Location) -> ParsingResult {
    Err((msg.clone(), loc))
  }

  // top level statement
  // 1. declaration
  // 2. call expression
  // 3. binary expression
  // 4. function declaration
  // 5. annotation
  fn top_level_parse(& mut self , ctx: &mut ParsingCtx) -> ParsingResult 
  {
    let Some(token) = ctx.get_cur_token() else {
      let loc = ctx.get_closed_token_position();
      return Self::err(&"token is empty".to_string(), loc.unwrap_or_default() );
    };

    let token_typ = token.token_t;
    let res =  match token_typ {
      lexer::TokenTyp::TokenLet => self.parse_decl(ctx),
      lexer::TokenTyp::TokenFnDecl => self.parse_fn_decl(ctx),
      lexer::TokenTyp::TokenRustlAnnotation => self.parse_rustl_annotation(ctx),
      _ => {
        if ctx.next_token_typ(&[TokenTyp::TokenName, TokenTyp::TokenLParenthesis]) {
          // parse to call
          self.parse_fn_call(ctx)
        }else {
          let prefix_res = self.parse_expression(ctx);

          if ctx.next_token_typ(&[TokenTyp::TokenDot]) {
            self.parse_field_set(ctx, prefix_res?)
          }
          else { prefix_res }
        }
      },
    }?;

    let need_statement_end =  {
      if token_typ == lexer::TokenTyp::TokenRustlAnnotation {
        false
      } else {
        match res.kind {
          AstKind::FnDefine(_, _, _) => false,
          _ => true,
        }
      }
    };

    if need_statement_end {
      let _ = ctx.expect_cur_token(TokenTyp::TokenStatementEnd)?;
      ctx.consume_cur_token();
    }

    Ok(res) 
  }

  fn parse_binary_expression(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let mut lhs = self.parse_expression_one(ctx)?;

    let is_fit_token = 
      |tk: &Token| {
        match tk.token_t {
          lexer::TokenTyp::TokenSub |
          lexer::TokenTyp::TokenAdd => true,
          _ => false
        }
      };

    loop {
      if !ctx.has_token() {
        break;
      }

      let cur_token = ctx.get_cur_token().unwrap();
      if !is_fit_token(cur_token) {
        break;
      }

      let op_string = cur_token.token_value.to_string();

      ctx.consume_cur_token();

      let rhs = self.parse_expression_one(ctx)?;
      lhs = Box::new(Expr::new(AstKind::BinaryOp(op_string, lhs, rhs)) );
    }

    Ok(lhs)
  }

  // && || 
  fn parse_bool_binary(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let mut lhs = self.parse_comp(ctx)?;

    let is_fit_token = 
      |tk: &Token| {
        match tk.token_t {
          lexer::TokenTyp::TokenBoolOR |
          lexer::TokenTyp::TokenBoolAND => true,
          _ => false
        }
      };


    loop {
      if !ctx.has_token() {
        break;
      }

      let cur_token = ctx.get_cur_token().unwrap();
      if !is_fit_token(cur_token) {
        break;
      }

      let token_typ = cur_token.token_t;
      let location = cur_token.location.clone();
      ctx.consume_cur_token();

      match token_typ {
        lexer::TokenTyp::TokenBoolAND => {
          let rhs = self.parse_comp(ctx)?;
          lhs = Self::boxed_expression(AstKind::BinaryOp("&&".to_string(), lhs, rhs), location);
        },
        lexer::TokenTyp::TokenBoolOR  => {
          let rhs = self.parse_bool_binary(ctx)?;
          lhs = Self::boxed_expression(AstKind::BinaryOp("||".to_string(), lhs, rhs), location);
        },
        _ => return Self::err(&format!("bool binary not support this operator {:?}", token_typ), location)
      }
    }

    Ok(lhs)
  }

  fn parse_return_expression(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let location = ctx.expect_cur_token(TokenTyp::TokenReturn)?.location;
    ctx.consume_cur_token();
    let return_expression = self.parse_expression(ctx)?;
    Ok(Self::boxed_expression( AstKind::ReturnExpr(return_expression), location ))
  }

  fn parse_expression_one(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    // todo: ( $expression )

    let mut lhs = self.parse_expression_one_token(ctx)?;

    let is_fit_token = 
      |tk: &Token| {
        match tk.token_t {
          lexer::TokenTyp::TokenDiv |
          lexer::TokenTyp::TokenMul => true,
          _ => false
        }
      };

    loop {
      if !ctx.has_token() {
        break;
      }
      let cur_token = ctx.get_cur_token().unwrap();
      if !is_fit_token(cur_token) {
        break;
      }

      let op_string = cur_token.token_value.to_string();
      let location = cur_token.location;
      ctx.consume_cur_token();

      let rhs = self.parse_expression_one_token(ctx)?;
      lhs = Self::boxed_expression( AstKind::BinaryOp(op_string, lhs, rhs), location);
    }

    Ok(lhs)
  }

  // $name
  // $expression.$name
  // $numeric_literal
  // $char_literal
  // not 
  // $string_literal
  // $bool _literal : true | false
  // ($expression)
  fn parse_expression_one_token(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let Some(token) = ctx.get_cur_token() else {
      return Self::err(&"first token not exists on parsing one token".to_string(), ctx.get_closed_token_position().unwrap_or_default());
    };

    let copy_token: Token = token.clone();
    let token_typ = copy_token.token_t;
    let ast  = match token_typ {
      lexer::TokenTyp::TokenName => {
        if ctx.next_token_typ(&[TokenTyp::TokenName, TokenTyp::TokenLParenthesis]) {
        // parse to call
          return self.parse_fn_call(ctx)
        }

        ctx.consume_cur_token();
        let mut expr = AstKind::AstName(copy_token.token_value.clone());
        loop {
          if !ctx.has_token() {
            break;
          }

          // $expression . $name = $expression
          if ctx.next_token_typ(&[TokenTyp::TokenDot, TokenTyp::TokenName, TokenTyp::TokenAssign]) {
            // it's field set
            break;
          }

          let Ok(token) = ctx.expect_cur_token(TokenTyp::TokenDot) else {
            break;
          };
          let loc = token.location;
          ctx.consume_cur_token();

          let Ok(token) = ctx.expect_cur_token(TokenTyp::TokenName) else {
            return Self::err(&"parseing field get error".to_string(), loc)
          };
          let token_name = token.token_value.clone();
          ctx.consume_cur_token();

          expr = AstKind::FieldGet(Self::boxed_expression(expr, loc) , token_name)
        }

        Ok(expr) 
      }, 
      lexer::TokenTyp::TokenNumLiteral => 
        Ok(AstKind::NumericLiteral(token.token_value.clone())) , 
      lexer::TokenTyp::TokenChar => 
        Ok(AstKind::CharLiteral(token.token_value.clone())) , 
      lexer::TokenTyp::TokenBoolNOT => {
        ctx.consume_cur_token();
        Ok(AstKind::Unary("!".to_string(), self.parse_expression_one_token(ctx)?))
      },
      lexer::TokenTyp::TokenStringLiteral => 
        Ok(AstKind::StringLiteral(token.token_value.clone())) , 
      // true | false
      lexer::TokenTyp::TokenTrue => 
        Ok(AstKind::LiteralTrue),
      lexer::TokenTyp::TokenFalse => 
        Ok(AstKind::LiteralFalse),
      // ($bool_binary_expression)
      lexer::TokenTyp::TokenLParenthesis => {
        ctx.consume_cur_token();
        let expr = self.parse_bool_binary(ctx)?;
        ctx.expect_cur_token(TokenTyp::TokenRParenthesis)?;
        Ok(expr.kind)
      },
      _ =>{
        Err((format!( "parsing expression one token not match any token {copy_token:#?}"), copy_token.location))
      } ,
    }?;

    if match token_typ {
      lexer::TokenTyp::TokenBoolNOT |
      lexer::TokenTyp::TokenName => false, 
      _ => true
    } {
      ctx.consume_cur_token();
    }

    Ok(Self::boxed_expression( ast , copy_token.location))
  }

  // let $name = $expression;
  fn parse_decl(& mut self , ctx: &mut ParsingCtx) -> ParsingResult
  {
    let location = ctx.expect_cur_token(TokenTyp::TokenLet)?.location;
    ctx.consume_cur_token();
    let variable_name = ctx.expect_cur_token(TokenTyp::TokenName)?.token_value.clone();
    ctx.consume_cur_token();

    let _assign_operation = ctx.expect_cur_token(TokenTyp::TokenAssign)?;

    ctx.consume_cur_token();
    let assigned_expression = self.parse_expression(ctx)?;
    Ok(Self::boxed_expression(AstKind::Let(variable_name, assigned_expression), location))
  }

  // @$name
  fn parse_rustl_annotation(& mut self , ctx: &mut ParsingCtx) -> ParsingResult {
    // println!("parse rustl annotation");
    let annotation = ctx.expect_cur_token(TokenTyp::TokenRustlAnnotation)?;
    let annotation_ast = Self::boxed_expression(AstKind::CharLiteral(annotation.token_value.clone()), annotation.location);
    ctx.consume_cur_token();
    Ok(annotation_ast)
  }

  fn parse_expression(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let Ok(_) = ctx.expect_cur_token(TokenTyp::TokenLBrace) else {
      return self.parse_bool_binary(ctx);
    };

    self.parse_object_literal(ctx)
  }

  // fn $name ( $args )
  fn parse_fn_decl(& mut self , ctx: &mut ParsingCtx) -> ParsingResult {
    let _keyword_fn_location /* fn */= ctx.expect_cur_token(TokenTyp::TokenFnDecl)?.location;
    ctx.consume_cur_token();
    let fn_name = ctx.expect_cur_token(TokenTyp::TokenName)?.token_value.clone();
    ctx.consume_cur_token();

    let arg_list_ast = self.parse_fn_decl_arg(ctx)?;

    // try to parse fn body
    if !ctx.next_token_typ(&[TokenTyp::TokenLBrace]) {
      let new_ast = Self::boxed_expression(AstKind::FnDeclAst(None, fn_name.to_string(),  arg_list_ast ), _keyword_fn_location);
      return Ok(new_ast);
    }

    ctx.consume_cur_token();

    let mut fn_statements : Vec<Box<Expr>> = vec![];
    loop {
      if !ctx.has_token() {
        break;
      }

      if ctx.next_token_typ(&[TokenTyp::TokenRBrace]) {
        break;
      }

      fn_statements.push(self.parse_statement_in_fn(ctx)?);

      if ctx.next_token_typ(&[TokenTyp::TokenRBrace]) {
        break;
      }

    }

    ctx.expect_cur_token(TokenTyp::TokenRBrace)?;
    ctx.consume_cur_token();

    let new_ast = Self::boxed_expression( AstKind::FnDefine(fn_name.to_string(),  arg_list_ast, fn_statements), _keyword_fn_location);
    Ok(new_ast)
  }

  fn parse_statement_in_fn(& mut self , ctx: &mut ParsingCtx) -> ParsingResult {
    
    let Some(token) = ctx.get_cur_token() else {
      return Self::err(&"token is empty".to_string(), ctx.get_closed_token_position().unwrap_or_default());
    };

    let token_typ = token.token_t;
    let res =  match token_typ {
      lexer::TokenTyp::TokenLet => self.parse_decl(ctx),
      lexer::TokenTyp::TokenReturn => 
        self.parse_return_expression(ctx),
      lexer::TokenTyp::TokenFnDecl => 
        Err(("fn declaration should not in fn definition".to_string(), token.location.clone())),
      lexer::TokenTyp::TokenRustlAnnotation => 
        Err(("annotation should not in fn definition".to_string(), token.location.clone())),
      _ => {
        if ctx.next_token_typ(&[TokenTyp::TokenName, TokenTyp::TokenLParenthesis]) {
          // parse to call
          self.parse_fn_call(ctx)
        }else {
          let prefix_res = self.parse_expression(ctx);
          if ctx.next_token_typ(&[TokenTyp::TokenDot]) {
            self.parse_field_set(ctx, prefix_res?)
          }
          else { prefix_res }
        }
      },
    }?;

    ctx.expect_cur_token(TokenTyp::TokenStatementEnd)?;
    ctx.consume_cur_token();

    Ok(res) 
  }

  //$args : ($name, ...)
  fn parse_fn_decl_arg(&mut self , ctx: &mut ParsingCtx)  -> ParsingResult {
    let location = ctx.expect_cur_token(TokenTyp::TokenLParenthesis)?.location;
    ctx.consume_cur_token();

    let mut arg_decl_list: Vec<Box<Expr>> = vec![];

    loop {
      if !ctx.has_token() {
        break;
      }

      let Ok(token ) = ctx.expect_cur_token(TokenTyp::TokenName) else {
        break;
      };
      let arg_name = token.token_value.clone();
      let location = token.location;
      ctx.consume_cur_token();

      arg_decl_list.push(Self::boxed_expression( AstKind::AstName(arg_name), location));
      if !ctx.has_token() {
        break;
      }

      if ctx.get_cur_token().unwrap().token_t != TokenTyp::TokenComma {
        break;
      }
    }

    ctx.expect_cur_token(TokenTyp::TokenRParenthesis)?;

    ctx.consume_cur_token();

    Ok( Self::boxed_expression( AstKind::FnDeclArgs(arg_decl_list), location) )
  }

  // $name ( $args )
  fn parse_fn_call(&mut self , ctx: &mut ParsingCtx) -> ParsingResult {
    let call_identifier = ctx.get_cur_token().unwrap().token_value.clone();
    let location = ctx.get_cur_token().unwrap().location;
    ctx.consume_cur_token();
    ctx.expect_cur_token(TokenTyp::TokenLParenthesis)?;
    ctx.consume_cur_token();
    let mut call_args:Vec<Box<Expr>> = vec![];

    loop {
      if !ctx.has_token() {
        break;
      }

      if ctx.next_token_typ(&[TokenTyp::TokenRParenthesis]) {
        break;
      }

      let in_arg = self.parse_bool_binary(ctx)?;
      call_args.push(in_arg);

      if !ctx.next_token_typ(&[TokenTyp::TokenComma]) {
        break;
      }

      ctx.consume_cur_token();
    }

    if !ctx.next_token_typ(&[TokenTyp::TokenRParenthesis]) {
      return Err(("function call parsing has no right parenthesis. example: function()".to_string(), ctx.get_closed_token_position().unwrap_or_default()))
    }

    ctx.consume_cur_token();

    Ok(Self::boxed_expression( AstKind::FnCall(call_identifier, call_args), location))
  }

  // {$name: $expression, ... }
  fn parse_object_literal(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let start_location = ctx.expect_cur_token(TokenTyp::TokenLBrace)?.location.start;
    ctx.consume_cur_token();

    let mut fields = vec![];
    loop {
      if !ctx.has_token() {
        break;
      }

      if ctx.next_token_typ(&[TokenTyp::TokenRBrace]){
        break;
      }

      let field_name = ctx.expect_cur_token(TokenTyp::TokenName)?.token_value.clone();
      ctx.consume_cur_token();
      ctx.expect_cur_token(TokenTyp::TokenColon)?;
      ctx.consume_cur_token();

      let value = self.parse_expression(ctx)?;
      fields.push((field_name, value));

      if ctx.next_token_typ(&[TokenTyp::TokenRBrace]){
        break;
      }

      ctx.expect_cur_token(TokenTyp::TokenComma)?;
      ctx.consume_cur_token();
    }

    let end_location = ctx.expect_cur_token(TokenTyp::TokenRBrace)?.location.end;
    ctx.consume_cur_token();
    Ok(Self::boxed_expression( AstKind::ObjectLiteral(fields), Location{ start: start_location, end: end_location}))
  }

  // $expression . $name = $expression
  fn parse_field_set(&mut self, ctx: &mut ParsingCtx, prefix: Box<Expr>) -> ParsingResult {
    let location = ctx.expect_cur_token(TokenTyp::TokenDot)?.location;
    ctx.consume_cur_token();
    let field_name = ctx.expect_cur_token(TokenTyp::TokenName)?.token_value.clone();
    ctx.consume_cur_token();

    ctx.expect_cur_token(TokenTyp::TokenAssign)?;
    ctx.consume_cur_token();

    let expr = self.parse_expression(ctx)?;
    Ok(Self::boxed_expression(AstKind::FieldSet(prefix, field_name, expr), location))
  }

  // todo: parsing == , <= , >=
  fn parse_comp(&mut self, ctx:&mut ParsingCtx) -> ParsingResult {
    let mut lhs = self.parse_binary_expression(ctx)?;

    let is_comp_token = |tt: TokenTyp| 
      match tt {
        TokenTyp::TokenEq |
        TokenTyp::TokenLe |
        TokenTyp::TokenLt |
        TokenTyp::TokenGe |
        TokenTyp::TokenGt   => true,
        _ => false
      };

    if !ctx.has_token() {
      return Ok(lhs);
    }

    // compare operator
    let token = ctx.get_cur_token().unwrap();
    let operator = token.token_value.clone();
    let location = token.location;
    if !is_comp_token(token.token_t) {
      return Ok(lhs);
    }
    ctx.consume_cur_token();

    let rhs = self.parse_binary_expression(ctx)?;
    lhs = Self::boxed_expression(AstKind::BinaryOp(operator, lhs, rhs), location);

    Ok(lhs)
  }
}
