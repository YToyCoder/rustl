use std::{collections::HashMap, fmt::Debug, rc::Rc};

use crate::sytax::{AstKind, Expr};

struct RustlObj 
{
  fields: HashMap<String, Rc<RustlValue>>
}

impl RustlObj {
  // pub fn get_field(& self,field_name: impl Into<String>) -> Option<&Rc<RustlValue>> {
  //   self.fields.get(&field_name.into())
  // }

  // pub fn set_field(&mut self, field_name: impl Into<String>, value: Rc<RustlValue>) -> () {
  //   self.fields.insert(field_name.into(), value);
  //   ()
  // }
}

impl RustlObj {
  pub fn empty() -> RustlObj {
    RustlObj{ fields: HashMap::new() }
  }
}

union RustlUValue {
  value_int: i32,
  value_float: f32,
  value_char: char,
  value_bool: bool,
  value_string: std::mem::ManuallyDrop<String>,
  obj : std::mem::ManuallyDrop<RustlObj>
}

impl RustlUValue {
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum RustlValueTyp {
  RustlInt,
  RustlFloat,
  RustlChar,
  RustlBool,
  RustlObj,
  RustlString
}

macro_rules! numeric_op_define {
  ($op:tt $name:tt) => {
    pub fn $name(&self, other:& Rc<RustlValue>) -> Option< Rc<RustlValue> >
    {
      if !self.is_numeric() || !other.is_numeric() {
        return None
      };
  
      let add_tag = 
        match (self.tag, other.tag) {
          (RustlValueTyp::RustlFloat, _) | 
          (_, RustlValueTyp::RustlFloat) 
            => RustlValueTyp::RustlFloat,
          _ => RustlValueTyp::RustlInt,
        };
  
      match add_tag {
        RustlValueTyp::RustlFloat => 
          Some(Rc::new(RustlValue::create_float(
            self.to_float()? $op other.to_float()?
          ))), 
        RustlValueTyp::RustlInt => 
          Some(Rc::new(RustlValue::create_int(
            self.to_int_value()? $op other.to_int_value()?
          ))), 
        _ => None
      }
    }
  };
}

macro_rules! bool_op_define {
  ($op:tt $name:tt) => {
    pub fn $name(&self, other:& Rc<RustlValue>) -> Option< Rc<RustlValue> > {
      Some(Rc::new(RustlValue::create_bool(self.to_bool()? $op other.to_bool()?)))
    }
  };
}

pub struct RustlValue 
{
  tag: RustlValueTyp,
  value: RustlUValue
}

impl RustlValue {
  pub fn is_support_add_op(&self) -> bool {
    match self.tag {
      RustlValueTyp::RustlInt | 
      RustlValueTyp::RustlFloat |
      RustlValueTyp::RustlChar |
      RustlValueTyp::RustlString => true,
      _ => false
    }
  }

  pub fn is_char(&self) -> bool { self.tag == RustlValueTyp::RustlChar }
  pub fn is_string(&self) -> bool { self.tag == RustlValueTyp::RustlString }
  pub fn is_bool(&self) -> bool { self.tag == RustlValueTyp::RustlBool }
  pub fn is_numeric(&self) -> bool {
    match self.tag {
      RustlValueTyp::RustlInt | 
      RustlValueTyp::RustlFloat => true,
      _ => false
    }
  }

  pub fn to_int_value(&self) -> Option<i32>  {
    unsafe {
      match self.tag {
        RustlValueTyp::RustlInt => Some(self.value.value_int),
        RustlValueTyp::RustlFloat => {
          Some(self.value.value_float as i32)
        }
        RustlValueTyp::RustlChar => {
          Some(self.value.value_char as i32)
        },
        _ => None
      }
    }
  }

  pub fn to_float(&self) -> Option<f32> {
    unsafe {
      match self.tag {
        RustlValueTyp::RustlInt => Some(self.value.value_int as f32 ),
        RustlValueTyp::RustlFloat => {
          Some(self.value.value_float)
        },
        _ => None
      }
    }
  }

  pub fn to_string(&self) -> String {
    unsafe {
      match self.tag {
        RustlValueTyp::RustlInt => 
          self.value.value_int.to_string(),
        RustlValueTyp::RustlFloat => 
          self.value.value_float.to_string(),
        RustlValueTyp::RustlChar => 
          self.value.value_char.to_string(),
        RustlValueTyp::RustlBool => 
          self.value.value_bool.to_string(),
        RustlValueTyp::RustlObj => 
          "{Obj}".to_string(),
        RustlValueTyp::RustlString => 
          self.value.value_string.to_string(),
      }
    }
  }

  pub fn to_bool(&self) -> Option<bool> {
    unsafe {
      match self.tag {
        RustlValueTyp::RustlBool 
          => Some(self.value.value_bool),
        RustlValueTyp::RustlInt
          => Some(self.value.value_int != 0),
        RustlValueTyp::RustlFloat
          => Some(self.value.value_float != 0.0),
        RustlValueTyp::RustlChar 
          => Some(self.value.value_char != 0 as char),
        RustlValueTyp::RustlString 
          => Some(!self.value.value_string.is_empty()),
        RustlValueTyp::RustlObj
          => Some(true),
      }
    }
  }

  pub fn bool_not(&self) -> Option< Rc<RustlValue> > {
    Some(Rc::new(RustlValue::create_bool(!self.to_bool()?)))
  }

  bool_op_define! {||  bool_or} 
  bool_op_define! {&&  bool_and}

  numeric_op_define! { +  numeric_add }
  numeric_op_define! { -  numeric_sub }
  numeric_op_define! { *  numeric_multi }
  numeric_op_define! { /  numeric_div }
}


impl Debug for RustlValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

    let mut binding = f.debug_struct("RustlValue");

    unsafe {
      match self.tag {
        RustlValueTyp::RustlInt => 
          binding.field("tag", &self.tag).field("value", &self.value.value_int).finish(),
        RustlValueTyp::RustlFloat => 
          binding.field("tag", &self.tag).field("value", &self.value.value_float).finish(),
        RustlValueTyp::RustlChar => 
          binding.field("tag", &self.tag).field("value", &self.value.value_char).finish(),
        RustlValueTyp::RustlBool => 
          binding.field("tag", &self.tag).field("value", &self.value.value_bool).finish(),
        RustlValueTyp::RustlObj =>  
          binding.field("tag", &self.tag).field("value", &self.value.obj.fields).finish(),
        RustlValueTyp::RustlString =>
          binding.field("tag", &self.tag).field("value", &self.value.value_string).finish(),
      }
    }
  }
}

fn rustl_add(l:& Rc<RustlValue>, r:& Rc<RustlValue>) -> Option<Rc<RustlValue>>
{
  if !l.is_support_add_op() || !r.is_support_add_op(){
    return None;
  };

  if l.is_char() || r.is_char() {
    return Some(Rc::new(RustlValue::create_int(l.to_int_value()? + r.to_int_value()?)));
  };

  if l.is_string() || r.is_string() {
    return Some(Rc::new(RustlValue::create_string(
     &format!("{}{}", l.to_string(), r.to_string()) 
    )));
  };

  l.numeric_add(r)
}

impl RustlValue 
{
  pub fn create_int(i: i32) -> RustlValue { 
    RustlValue{ tag: RustlValueTyp::RustlInt, value: RustlUValue{value_int: i} }
  }

  pub fn create_float(v: f32) -> RustlValue { 
    RustlValue{ tag: RustlValueTyp::RustlFloat, value: RustlUValue{value_float: v} }
  }

  pub fn create_char(v: char) -> RustlValue { 
    RustlValue{ tag: RustlValueTyp::RustlChar, value: RustlUValue{value_char: v} }
  }

  pub fn create_bool(v: bool) -> RustlValue { 
    RustlValue{ tag: RustlValueTyp::RustlBool, value: RustlUValue{value_bool: v} }
  }

  pub fn create_obj() -> RustlValue {
    RustlValue{ tag: RustlValueTyp::RustlObj, value: RustlUValue{obj: std::mem::ManuallyDrop::new( RustlObj::empty()) } }
  }

  pub fn create_string(string_literal: &String) -> RustlValue {
    RustlValue{ 
        tag: RustlValueTyp::RustlString, 
        value: RustlUValue{value_string: std::mem::ManuallyDrop::new( string_literal.clone()) } }
  }
  
  pub fn get_value_string(&self) -> Option<&String> 
  {
    if self.tag == RustlValueTyp::RustlString {
      unsafe {
        Some(&self.value.value_string)
      } 
    }
    else {
      None
    }
  }

}

#[derive(Debug,Clone)]
struct FnDefinition {
  arg_list: Vec<String>,
  statements: Vec<Box<Expr>>,
}

trait Scope {
  fn get_local_variable(&self, name: &String) -> Option<&Rc<RustlValue>>;
  fn set_local_variable(&mut self, name: &String, value: Rc<RustlValue>);
  fn get_fn_definition(&self, name:&String) -> Option<&FnDefinition>;
  fn add_fn_definition(&mut self, _name:&String, _arg_list:& Vec<String>, _statements: &Vec<Box<Expr>>) {}
  fn do_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;

  fn set_error(&mut self, error: Rc<RustlValue>);

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
  stack_value: HashMap<String, Rc<RustlValue>>,
  parent_scope: &'a dyn Scope,
  return_value: Rc<RustlValue>,
  err: Option<Rc<RustlValue>>
}

// impl<'a, 'b:'a> FunctionScope<'a, 'b> {
//   pub fn new(mut parent:&mut Box<dyn Scope + 'a>) -> Self {
//     FunctionScope{ stack_value: HashMap::default(), parent_scope: &mut parent}
//   }
// }

impl<'a> Scope for FunctionScope<'a> {

  fn do_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.debug_struct("FunctionScope").field("stack_value", &self.stack_value).finish()
  }

  fn get_local_variable(&self, name: &String ) -> Option<&Rc<RustlValue>> {
    let res = self.stack_value.get(name);
    if res.is_some() {
      return res;
    }

    self.parent_scope.get_local_variable(name)
  }

  fn set_local_variable(&mut self, name: &String, value: Rc<RustlValue>) {
    self.stack_value.insert(name.clone(), value);
  }
  
  fn get_fn_definition(&self, _:&String) -> Option<&FnDefinition> {
    None
  }
  
  fn set_error(&mut self, error: Rc<RustlValue>) {
    self.err = Some(error)
  }

  
  fn has_error(&self) -> bool {
    self.err.is_some()
  }
}

#[derive(Debug)]
struct GlobalScope {
  global_variable: HashMap<String, Rc<RustlValue>>,
  user_defined_fn: HashMap<String, FnDefinition>,
  err: Option<Rc<RustlValue>>
}

impl Default for GlobalScope {
  fn default() -> Self {
    Self { global_variable: Default::default(), user_defined_fn: Default::default(), err: None }
  }
}

impl Scope for GlobalScope {
  fn get_local_variable(&self, name: &String ) -> Option<&Rc<RustlValue>> {
    self.global_variable.get(name)
  }

  fn set_local_variable(&mut self, name: &String, value: Rc<RustlValue>) {
    self.global_variable.insert(name.clone(), value);
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
 
  fn set_error(&mut self, error: Rc<RustlValue>) {
    self.err = Some(error)
  }

  
  fn has_error(&self) -> bool {
    self.err.is_some()
  }
}

struct RustlCtx {
  rustl_nil: Rc<RustlValue>,
  builtin_func: RustlBuiltinFnRegister,
}

impl RustlCtx {
  fn get_nil(&self) -> Rc<RustlValue> {
    self.rustl_nil.clone()
  }
}

impl Default for RustlCtx {
  fn default() -> Self {
    Self { rustl_nil: Rc::new(RustlValue::create_obj()), builtin_func: Default::default() }
  }
}

pub struct RustlArgList {
  pub args: Vec<Rc<RustlValue>>,
}

trait RustlBuiltinFn {
  fn call(&mut self, args:& Vec<Rc<RustlValue>>) -> Rc<RustlValue>;

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
  rustl_nil: Rc<RustlValue>
}

impl RustlBuiltinFn for RustlFnPrint {
  fn call(&mut self, args:& Vec<Rc<RustlValue>>) -> Rc<RustlValue> {
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

  let rustl_print = RustlFnPrint{ rustl_nil: rustl_eval_ctx.get_nil() };
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

fn create_numeric_value_from_ast(ast:&Box<Expr>) -> Option<RustlValue> 
{
  let AstKind::NumericLiteral(ref numeric_string) = ast.kind else {
    return None;
  };

  match numeric_string.find('.') 
  {
    Some(_) => 
      match numeric_string.parse::<f32>() {
        Ok(float_value) =>  Some(RustlValue::create_float(float_value)),
        Err(_) => None
      }
    None => 
      match numeric_string.parse::<i32>() {
        Ok(int_value) =>  Some(RustlValue::create_int(int_value)),
        Err(_) => None
      }
  }
}

fn eval_expression<'a: 'b, 'b >(ast:& Box<Expr>, cur_scope: &'a mut dyn Scope, ctx: &mut RustlCtx) -> Option< Rc<RustlValue> >{
  match &ast.kind {
    AstKind::ProgramAst(_) => todo!(),
    AstKind::NumericLiteral(_) => {
      let value = create_numeric_value_from_ast(ast)?;
      Some(Rc::new(value))
    } ,
    AstKind::CharLiteral(char_string) => 
      Some(Rc::new(RustlValue::create_string(char_string))),
    AstKind::Let(name, value_ast) => {
      let value = eval_expression(&value_ast, cur_scope, ctx)?;
      cur_scope.set_local_variable(name, value);
      Some(ctx.get_nil()) 
    },
    AstKind::AstName(name_string) => {
      let name_string_no_borrow = name_string.clone();
      let local_variable = cur_scope.get_local_variable(name_string).and_then(|v| Some(v.clone()));
      if local_variable.is_none() {
        cur_scope.set_error(Rc::new(RustlValue::create_string(&format!("not found variable {}", name_string_no_borrow))));
      }
      local_variable
    },
      // Some(Rc::new(RustlValue::create_string(&name_string))) ,
      // cur_scope.get_local_variable(name_string).and_then(|rc| Some(rc.clone())),
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
        _     => None,
      }
    },
    AstKind::FnDeclAst(_, _, _) => None,
    AstKind::RustlAnnotation(_) => None,
    AstKind::AstNull => None,
    AstKind::FnDeclArgs(_) => None,
    AstKind::StringLiteral(literal) => 
      Some(Rc::new(RustlValue::create_string(literal))),
    AstKind::FnCall(name ,args) => {
      
      let args_value: Vec<Rc<RustlValue>> = 
        args
        .iter()
        .map(|expr| { 
            eval_expression(expr, cur_scope, ctx)
            .or(Some(ctx.get_nil().clone()))
            .unwrap() })
        .into_iter()
        .collect();
      
      let user_defined_fn_opt = cur_scope.get_fn_definition(name);
      if user_defined_fn_opt.is_some() {
        let user_defined_fn: FnDefinition = user_defined_fn_opt.unwrap().clone();
        let mut new_scope = FunctionScope{stack_value:Default::default(), parent_scope: cur_scope, return_value: ctx.get_nil().clone(), err: None};
        for arg in 0..user_defined_fn.arg_list.len() {
          new_scope.set_local_variable(
            &user_defined_fn.arg_list[arg], 
            args_value
              .get(arg)
              .or(Some(&ctx.get_nil().clone()))
              .unwrap()
              .clone())
        }
        for ast in user_defined_fn.statements {
          if eval_expression(&ast, &mut new_scope, ctx).is_none() && new_scope.has_error() {
            cur_scope.set_error(new_scope.err.unwrap());
            return None;
          }
        }
        return Some(new_scope.return_value)
      }

      let builtin_fn = ctx.builtin_func.get_fn(name)?;
      Some(builtin_fn.call(&args_value))
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
      Some(Rc::new(RustlValue::create_bool(true))),
    AstKind::LiteralFalse => 
      Some(Rc::new(RustlValue::create_bool(false))),
    AstKind::Unary(_, _) => todo!(),
  }
}