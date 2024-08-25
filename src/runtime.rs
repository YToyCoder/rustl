use std::{cell::RefCell, collections::HashMap, fmt::{Debug, Display}, rc::Rc};

use crate::sytax::{AstKind, Expr};

#[derive(Debug, Clone)]
struct RustlObj 
{
  fields: HashMap<String, RustlV>
}

impl Display for RustlObj {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut fmt: &mut std::fmt::DebugStruct = &mut f.debug_struct("Object");
    for (name, value) in &self.fields {
      fmt = fmt.field(&name, &value)
    }

    fmt.finish()
  }
}

impl RustlObj {
  pub fn get_field(& self,field_name: impl Into<String>) -> Option<&RustlV> {
    self.fields.get(&field_name.into())
  }

  pub fn set_field(&mut self, field_name: impl Into<String>, value: RustlV) -> () {
    self.fields.insert(field_name.into(), value);
    ()
  }
}

impl RustlObj {
  pub fn empty() -> RustlObj {
    RustlObj{ fields: HashMap::new() }
  }
}

macro_rules! numeric_op_define {
  ($op:tt $name:tt) => {
    pub fn $name(&self, other:& Self) -> Option< Self >
    {
      if !self.is_numeric() || !other.is_numeric() {
        return None
      };

      let new_v = 
        if self.is_float() || other.is_float() {
          Self::new_float(
            self.to_float()? + other.to_float()?)
        }
        else {
          Self::new_int(
            self.to_int_value()? +other.to_int_value()?)
        };
  
      Some(new_v)
    }
  };
}

macro_rules! bool_op_define {
  ($op:tt $name:tt) => {
    pub fn $name(&self, other:& Self) -> Option<Self> {
      Some(Self::new_bool(self.to_bool()? $op other.to_bool()?))
    }
  };
}

#[derive(Debug, Clone)]
enum RustlV {
  RustlInt(i32),
  RustlFloat(f32),
  RustlChar(char),
  RustlBool(bool),
  RustlString(String),
  RustlObject(Rc<RefCell<RustlObj>>),
}

impl RustlV {
  pub fn new_int(i:i32) -> RustlV { RustlV::RustlInt(i) }
  pub fn new_float(f: f32) -> RustlV { RustlV::RustlFloat(f) }
  pub fn new_bool(b: bool) -> RustlV { RustlV::RustlBool(b) }
  pub fn new_string(str: &String) -> RustlV { 
    RustlV::RustlString(str.clone())
  }

  pub fn new_char(c: char) -> RustlV {
    RustlV::RustlChar(c)
  }

  pub fn new_obj() -> RustlV {
    RustlV::RustlObject(Rc::new(RefCell::new(RustlObj::empty())))
  }

  pub fn is_support_add_op(&self) -> bool {
    match self {
      RustlV::RustlInt(_) | 
      RustlV::RustlFloat(_) | 
      RustlV::RustlChar(_) | 
      RustlV::RustlString(_) => true,
      _ => false
    }
  }

  
  pub fn is_char(&self) -> bool { 
    match self {
      RustlV::RustlChar(_) => true,
      _ => false
    }
  }

  pub fn is_string(&self) -> bool { 
    match self {
      RustlV::RustlString(_) => true,
      _ => false
    } 
  }

  pub fn is_bool(&self) -> bool { 
    match self {
      RustlV::RustlBool(_) => true,
      _ => false
    }
  }
  pub fn is_numeric(&self) -> bool {
    match self {
      RustlV::RustlInt(_) | 
      RustlV::RustlFloat(_)  => true,
      _ => false
    }
  }

  pub fn is_float(&self) -> bool {
    match self {
      RustlV::RustlFloat(_)  => true,
      _ => false
    }
  }

  pub fn is_int(&self) -> bool {
    match self {
      RustlV::RustlInt(_)   => true,
      _ => false
    }
  }

  pub fn to_int_value(&self) -> Option<i32>  {
    match self {
      RustlV::RustlInt(i)  => Some(*i),
      RustlV::RustlFloat(f)  => 
        Some(*f as i32),
      RustlV::RustlChar(c) => 
        Some(*c as i32),
      _ => None
    }
  }

  pub fn to_float(&self) -> Option<f32> {
    match self {
      RustlV::RustlInt(i) => Some(*i as f32 ),
      RustlV::RustlFloat(f) => {
        Some(*f)
      },
      _ => None
    }
  }

  pub fn to_string(&self) -> String {
    match self {
      RustlV::RustlInt(i)  => 
        i.to_string(),
      RustlV::RustlFloat(f) => 
        f.to_string(),
      RustlV::RustlChar(c) => 
        c.to_string(),
      RustlV::RustlBool(b) => 
        b.to_string(),
      RustlV::RustlObject(obj) => 
        obj.borrow().to_string(),
      RustlV::RustlString(string) => 
        string.to_string(),
    }
  }

  pub fn to_bool(&self) -> Option<bool> {
    Some(match self {
      RustlV::RustlInt(i)  => *i != 0,
      RustlV::RustlFloat(f) => *f != 0.0,
      RustlV::RustlChar(c) => *c != 0 as char,
      RustlV::RustlBool(b) => *b,
      RustlV::RustlObject(_) => true,
      RustlV::RustlString(string) => 
        string.is_empty(),
    })
  }

  pub fn bool_not(&self) -> Option< Self > {
    Some(Self::new_bool(!self.to_bool()?))
  }

  bool_op_define! {||  bool_or} 
  bool_op_define! {&&  bool_and}

  numeric_op_define! { +  numeric_add }
  numeric_op_define! { -  numeric_sub }
  numeric_op_define! { *  numeric_multi }
  numeric_op_define! { /  numeric_div }
}

fn rustl_add(l:& RustlV, r:& RustlV) -> Option<RustlV>
{
  if !l.is_support_add_op() || !r.is_support_add_op(){
    return None;
  };

  if l.is_string() || r.is_string() {
    return Some(RustlV::new_string(
     &format!("{}{}", l.to_string(), r.to_string()) 
    ));
  };

  if l.is_char() || r.is_char() {
    return Some(RustlV::new_int(l.to_int_value()? + r.to_int_value()?));
  };


  l.numeric_add(r)
}

#[derive(Debug,Clone)]
struct FnDefinition {
  arg_list: Vec<String>,
  statements: Vec<Box<Expr>>,
}

trait Scope {

  // get local variable in current scope, if not find try parent
  fn get_local_variable(&self, name: &String) -> Option<&RustlV>;
  // set local variable in current scope
  fn set_local_variable(&mut self, name: &String, value:& RustlV);
  // always find definition in global scope
  fn get_fn_definition(&self, name:&String) -> Option<&FnDefinition>;
  // only support in global scope
  fn add_fn_definition(&mut self, _name:&String, _arg_list:& Vec<String>, _statements: &Vec<Box<Expr>>) {}
  fn do_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;

  // current scope is finished:
  // if an return-expresion is evaluated in function scope , this scope is finished
  fn finished(&self) -> bool;
  // set this scope to finish
  fn finish(&mut self) -> ();

  // set error in current scope , at same time shound finish scope
  fn set_error(&mut self, error:& RustlV);
  fn has_error(&self) -> bool {
    false
  }
}

impl Debug for dyn Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      self.do_fmt(f)
    }
}


struct FunctionScope<'a>
{
  stack_value: HashMap<String, RustlV>,
  parent_scope: &'a dyn Scope,
  return_value: RustlV,
  err: Option<RustlV>,
  finished_flag: bool,
}

impl<'a> Scope for FunctionScope<'a> {


  fn do_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.debug_struct("FunctionScope").field("stack_value", &self.stack_value).finish()
  }

  fn get_local_variable(&self, name: &String ) -> Option<&RustlV> {
    let res = self.stack_value.get(name);
    if res.is_some() {
      return res;
    }

    self.parent_scope.get_local_variable(name)
  }

  fn set_local_variable(&mut self, name: &String, value: &RustlV) {
    self.stack_value.insert(name.clone(), value.clone());
  }
  
  fn get_fn_definition(&self, _:&String) 
    -> Option<&FnDefinition> {
    None
  }
  
  fn set_error(&mut self, error:& RustlV) {
    self.err = Some(error.clone())
  }

  fn has_error(&self) -> bool { self.err.is_some() }
  fn finished(&self)  -> bool { self.finished_flag }
  fn finish(&mut self) -> () {
    self.finished_flag = true;
  }

}

#[derive(Debug)]
struct GlobalScope {
  global_variable: HashMap<String, RustlV>,
  user_defined_fn: HashMap<String, FnDefinition>,
  err: Option<RustlV>,
  finished_flag: bool,
}

impl Default for GlobalScope {
  fn default() -> Self {
    Self { global_variable: Default::default(), user_defined_fn: Default::default(), err: None, finished_flag: false }
  }
}

impl Scope for GlobalScope {
  fn get_local_variable(&self, name: &String ) -> Option<&RustlV> {
    self.global_variable.get(name)
  }

  fn set_local_variable(&mut self, name: &String, value:& RustlV) {
    self.global_variable.insert(name.clone(), value.clone());
  }
  
  fn get_fn_definition(&self, name:&String) -> Option<&FnDefinition> {
    self.user_defined_fn.get(name)
  }
  
  fn add_fn_definition(&mut self, name:&String, arg_list:& Vec<String>, statements: &Vec<Box<Expr>>) {
    self.user_defined_fn.insert(name.to_string(), FnDefinition { arg_list: arg_list.to_vec(), statements: statements.to_vec()});
  }
  
  fn do_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("GlobalScope").field("global_variable", &self.global_variable).field("user_defined_fn", &self.user_defined_fn).finish()
  }
 
  fn set_error(&mut self, error: & RustlV) {
    self.err = Some(error.clone())
  }

  
  fn has_error(&self) -> bool { self.err.is_some() }
  fn finished(&self)  -> bool { self.finished_flag }
  fn finish(&mut self) -> () {
    self.finished_flag = true;
  }
}

struct RustlCtx {
  rustl_nil: RustlV,
  builtin_func: RustlBuiltinFnRegister,
}

impl RustlCtx {
  fn get_nil(&self) -> RustlV {
    self.rustl_nil.clone()
  }
}

impl Default for RustlCtx {
  fn default() -> Self {
    Self { rustl_nil: RustlV::new_obj(), builtin_func: Default::default() }
  }
}

pub struct RustlArgList {
  pub args: Vec<Rc<RustlV>>,
}

trait RustlBuiltinFn {
  fn call(&mut self, args:& Vec<RustlV>) -> RustlV;
}

struct RustlBuiltinFnRegister {
  register_fn: HashMap<String,Box<dyn RustlBuiltinFn>>,
}

impl Default for RustlBuiltinFnRegister {
  fn default() -> Self {
      Self { register_fn: Default::default() }
  }
}

impl RustlBuiltinFnRegister {
  pub fn register(&mut self, name: &String ,f: Box<dyn RustlBuiltinFn>) {
    self.register_fn.insert(name.to_string(), f);
  }

  pub fn get_fn(&mut self, name: &String) -> Option<&mut Box<dyn RustlBuiltinFn>> {
    self.register_fn.get_mut(name)
  }
}

struct RustlFnPrint {
  rustl_nil: RustlV
}

impl RustlBuiltinFn for RustlFnPrint {
  fn call(&mut self, args:& Vec<RustlV>) -> RustlV {
    if args.len() == 0 {
      println!("");
    }
    println!("{}", args.get(0).unwrap().to_string());
    self.rustl_nil.clone()
  }
}

pub fn eval_ast(ast:&mut Expr) -> () {
  let mut root_scope = GlobalScope::default();

  let mut rustl_eval_ctx = RustlCtx::default();
  let AstKind::ProgramAst(ref statements) = ast.kind else {
    return ();
  };

  let rustl_print = RustlFnPrint{ rustl_nil: rustl_eval_ctx.get_nil().into() };
  rustl_eval_ctx.builtin_func.register(&"print".to_string(), Box::new(rustl_print));

  for mut ast_ in statements {
    eval_expression(&mut ast_, &mut root_scope, &mut rustl_eval_ctx);
    if root_scope.has_error() {
      println!("[rustl][error] {}",root_scope.err.clone().unwrap().to_string());
      break;
    }
  }

  // println!("scope:{root_scope:#?}")
}

fn create_numeric_value_from_ast(ast:&Box<Expr>) -> Option<RustlV> 
{
  let AstKind::NumericLiteral(ref numeric_string) = ast.kind else {
    return None;
  };

  match numeric_string.find('.') 
  {
    Some(_) => 
      match numeric_string.parse::<f32>() {
        Ok(float_value) =>  Some(RustlV::new_float(float_value)),
        Err(_) => None
      }
    None => 
      match numeric_string.parse::<i32>() {
        Ok(int_value) =>  Some(RustlV::new_int(int_value)),
        Err(_) => None
      }
  }
}

fn eval_expression<'a: 'b, 'b >(ast:& Box<Expr>, cur_scope: &'a mut dyn Scope, ctx: &mut RustlCtx) -> Option< RustlV >{
  match &ast.kind {
    AstKind::ProgramAst(_) => todo!(),
    AstKind::NumericLiteral(_) => {
      let value = create_numeric_value_from_ast(ast)?;
      Some(value)
    } ,
    AstKind::CharLiteral(char_string) => 
      Some(RustlV::new_char(*char_string.as_bytes().get(1).unwrap() as char)),
    AstKind::Let(name, value_ast) => {
      let value = eval_expression(&value_ast, cur_scope, ctx)?;
      cur_scope.set_local_variable(name, &value);
      Some(ctx.get_nil()) 
    },
    AstKind::AstName(name_string) => {
      let name_string_no_borrow = name_string.clone();
      let local_variable = cur_scope.get_local_variable(name_string).and_then(|v| Some(v.clone()));
      if local_variable.is_none() {
        cur_scope.finish();
        cur_scope.set_error(&RustlV::new_string(&format!("not found variable {}", name_string_no_borrow)));
      }
      local_variable
    },
    AstKind::BinaryOp(op_string, lhs, rhs) => {
      let lhs_value = eval_expression(lhs, cur_scope, ctx)?;
      let rhs_value = eval_expression(rhs, cur_scope, ctx)?;
      match op_string as &str {
        "+"   => rustl_add(&lhs_value, &rhs_value),
        "*"   => lhs_value.numeric_multi(&rhs_value),
        "/"   => lhs_value.numeric_div(&rhs_value),
        "-"   => lhs_value.numeric_sub(&rhs_value),
        "&&"  => lhs_value.bool_and(&rhs_value),
        "||"  => lhs_value.bool_or(&rhs_value),
        _     => {
          cur_scope.set_error(&RustlV::new_string(&format!("not support binary operator {}", op_string)));
          cur_scope.finish();
          None
        },
      }
    },
    AstKind::FnDeclAst(_, _, _) => None,
    AstKind::RustlAnnotation(_) => None,
    AstKind::AstNull => None,
    AstKind::FnDeclArgs(_) => None,
    AstKind::StringLiteral(literal) => 
      Some(RustlV::new_string(literal)),
    AstKind::FnCall(name ,args) => {
      let mut args_value: Vec<RustlV> = vec![];
      for arg in args {
        let r = eval_expression(arg, cur_scope, ctx);
        if cur_scope.finished() {
          return None;
        }

        // error !
        if r.is_none() && cur_scope.has_error() {
          cur_scope.finish();
          return None;
        }

        args_value.push(r.unwrap());
      }

      let user_defined_fn_opt = cur_scope.get_fn_definition(name);
      if user_defined_fn_opt.is_some() {
        let user_defined_fn: FnDefinition = user_defined_fn_opt.unwrap().clone();
        let mut new_scope = FunctionScope{stack_value:Default::default(), parent_scope: cur_scope, return_value: ctx.get_nil().clone(), err: None, finished_flag: false};
        for arg in 0..user_defined_fn.arg_list.len() {
          new_scope.set_local_variable(
            &user_defined_fn.arg_list[arg], 
            args_value
              .get(arg)
              .or(Some(&ctx.get_nil().clone()))
              .unwrap())
        }
        for ast in user_defined_fn.statements {
          // error accure!
          if eval_expression(&ast, &mut new_scope, ctx).is_none() && new_scope.has_error() {
            cur_scope.set_error(&new_scope.err.unwrap());
            cur_scope.finish();
            return None;
          }

          // return expression
          if new_scope.finished() {
            break;
          }
        }
        return Some(new_scope.return_value)
      }

      let builtin_fn = ctx.builtin_func.get_fn(name);

      if builtin_fn.is_none() {
        cur_scope.set_error(&RustlV::new_string(&format!("not find any function {}", name)));
        return None;
      }

      Some(builtin_fn.unwrap().call(&args_value))
    },
    AstKind::FnDefine(name, args, statements) => {
      let arg_list: Option<Vec<String>> = match &args.kind {
        AstKind::FnDeclArgs(arg_expr) => {
          Some(arg_expr.iter().map_while(|expr| { match &expr.kind {
            AstKind::AstName(name) => Some(name.clone()) ,
            _ => None
          } }).collect())
        },
        _ => None
      };
      cur_scope.add_fn_definition(name, &arg_list?, statements);
      None
    },
    AstKind::LiteralTrue => 
      Some(RustlV::new_bool(true)),
    AstKind::LiteralFalse => 
      Some(RustlV::new_bool(false)),
    AstKind::Unary(unary_op, expr) => {
      match unary_op as &str {
        "!" => eval_expression(expr, cur_scope, ctx)?.bool_not(),
        _ => {
          cur_scope.finish();
          cur_scope.set_error(&RustlV::new_string(&format!("not support unary operator{}", unary_op.clone())));
          None
        } 
      }
    },
    AstKind::ReturnExpr(expr) => {
      let eval_result = eval_expression(expr, cur_scope, ctx)?;
      cur_scope.finish();
      Some(eval_result)
    } ,
    AstKind::ObjectLiteral(_fields) => {
      let mut new_object = RustlV::new_obj();
      let RustlV::RustlObject(obj) = &mut new_object else {
        // should not reach here
        return None
      };
      for (field_name, value) in _fields {
        obj.borrow_mut().set_field(field_name, eval_expression(value, cur_scope, ctx)? )
      }
      Some(new_object)
    },
  }
}