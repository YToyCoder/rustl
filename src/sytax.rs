use std::vec;

use crate::lexer::{self, Token, TokenTyp};


// --------------------------------- rustl sytax ----------------------------- 
// delaration : let $name = $expression;
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
// ---------------------------------------------------------------------------- 
// --------------------------------- example ---------------------------------- 
// let name = "string_literal";
// function(2 + 3);
// let numeric_expression = 3.1415 * 9 * 8 / 2;
// @builtin
// fn print(arg);

#[derive(Debug)]
pub enum AstKind {
  ProgramAst(Vec<Box<Expr>>),
  NumericLiteral(String),
  CharLiteral(String),
  Let(String, Box<Expr>),
  AstName(String),
  BinaryOp(String, Box<Expr>, Box<Expr>),
  FnDeclAst(Option<Box<Expr>>, String, Box<Expr>),
  RustlAnnotation(String),
  AstNull
}

#[derive(Debug)]
pub struct Expr {
  pub kind: AstKind,
}

impl Expr {
  pub fn ast_null() -> Box< Expr > {
    Box::new(Expr{ kind: AstKind::AstNull })
  }
}

pub struct Parser
{
  pub root: Option<Expr>,
}

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

  fn expect_cur_token(&mut self, token_typ: TokenTyp) -> Result<&Token, String> {
    let Some(cur_token) = self.get_cur_token() else {
      return Err(format!("cur token not exists on token expect {token_typ:#?}"))
    };

    if cur_token.token_t == token_typ { Ok(cur_token) } 
    else { 
      Err(format!(" token is not expect ({cur_token:#?}), real token typ {token_typ:#?}")) 
    }
  }
}

type ParsingResult = Result<Box<Expr>, String> ;

impl Parser
{
  pub fn new() -> Parser
  {
    Parser{root: None }
  }

  pub fn parse(&mut self , ctx: &mut ParsingCtx) -> ()
  {
    let mut statements: Vec<Box<Expr>> = vec![];

    loop {
      match self.top_level_parse(ctx) {
        Ok(ast) => 
          statements.push(ast),
        Err(_msg) => {
          println!("{}", _msg);
          break
        }
      }
    }

    self.root = Some(Expr{ kind:AstKind::ProgramAst(statements)} );

    ()
  }

  fn top_level_parse(& mut self , ctx: &mut ParsingCtx) -> ParsingResult 
  {
    let Some(token) = ctx.get_cur_token() else {
      return Err("token is empty".to_string());
    };

    let token_typ = token.token_t;
    let res =  match token_typ {
      lexer::TokenTyp::TokenLet => self.parse_decl(ctx),
      lexer::TokenTyp::TokenRustlAnnotation => self.parse_rustl_annotation(ctx),
      _ => self.parse_binary_expression(ctx),
    }?;

    if token_typ != lexer::TokenTyp::TokenRustlAnnotation {
      let _ = ctx.expect_cur_token(TokenTyp::TokenStatementEnd)?;
      ctx.consume_cur_token();
    }else {
      println!("annotation {res:#?}");
    }

    Ok(res) 
  }

  fn parse_binary_expression(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let mut lhs = self.parse_expression_one(ctx)?;

    let is_fit_token = 
      |tk: &Token| {
        match tk.token_t {
          lexer::TokenTyp::TokenSub => true,
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
      lhs = Box::new(Expr{ kind: AstKind::BinaryOp(op_string, lhs, rhs)} );
    }

    Ok(lhs)
  }

  fn parse_expression_one(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    // todo: ( $expression )

    let mut lhs = self.parse_expression_one_token(ctx)?;

    let is_fit_token = 
      |tk: &Token| {
        match tk.token_t {
          lexer::TokenTyp::TokenDiv => true,
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
      ctx.consume_cur_token();

      let rhs = self.parse_expression_one_token(ctx)?;
      lhs = Box::new(Expr{ kind: AstKind::BinaryOp(op_string, lhs, rhs)});
    }

    Ok(lhs)
  }

  fn parse_expression_one_token(&mut self, ctx: &mut ParsingCtx) -> ParsingResult {
    let Some(token) = ctx.get_cur_token() else {
      return Err("first token not exists on parsing one token".to_string());
    };

    let ast = match token.token_t {
      lexer::TokenTyp::TokenName => 
        Ok(AstKind::AstName(token.token_value.clone())) , 
      lexer::TokenTyp::TokenNumLiteral => 
        Ok(AstKind::NumericLiteral(token.token_value.clone())) , 
      lexer::TokenTyp::TokenChar => 
        Ok(AstKind::CharLiteral(token.token_value.clone())) , 
      _ => Err("")
    }?;

    ctx.consume_cur_token();

    Ok(Box::new(Expr{ kind: ast }))
  }

  fn parse_decl(& mut self , ctx: &mut ParsingCtx) -> ParsingResult
  {
    ctx.expect_cur_token(TokenTyp::TokenLet)?;
    ctx.consume_cur_token();
    let variable_name = ctx.expect_cur_token(TokenTyp::TokenName)?.token_value.clone();
    ctx.consume_cur_token();

    let _assign_operation = ctx.expect_cur_token(TokenTyp::TokenAssign)?;

    ctx.consume_cur_token();
    let assigned_expression = self.parse_binary_expression(ctx)?;
    Ok(Box::new(Expr{kind:AstKind::Let(variable_name, assigned_expression)}))
  }

  fn parse_rustl_annotation(& mut self , ctx: &mut ParsingCtx) -> ParsingResult {
    println!("parse rustl annotation");
    let annotation = ctx.expect_cur_token(TokenTyp::TokenRustlAnnotation)?;
    let annotation_ast =Box::new(Expr{kind:AstKind::CharLiteral(annotation.token_value.clone())});
    ctx.consume_cur_token();
    Ok(annotation_ast)
  }

  fn parse_fn_decl(& mut self , ctx: &mut ParsingCtx) -> ParsingResult {
    let _keyword_fn /* fn */= ctx.expect_cur_token(TokenTyp::TokenFnDecl)?;
    ctx.consume_cur_token();
    let fn_name = ctx.expect_cur_token(TokenTyp::TokenName)?.token_value.clone();
    ctx.consume_cur_token();

    let arg_list_ast = self.parse_fn_decl_arg(ctx)?;

    let new_ast = Box::new(Expr{kind: AstKind::FnDeclAst(None, fn_name.to_string(),  arg_list_ast )});
    Ok(new_ast)
  }

  fn parse_fn_decl_arg(& mut self , ctx: &mut ParsingCtx)  -> ParsingResult {
    let left_parent = ctx.expect_cur_token(TokenTyp::TokenLParenthesis)?;
    ctx.consume_cur_token();
    Err(":".to_string())
  }

}
