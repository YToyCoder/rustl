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

#[derive(Debug)]
struct EvalScope 
{
  stack_value: HashMap<String, Rc<RustlValue>>,
}

pub fn eval_ast(ast:&mut Expr) -> () {
  let mut root_scope = EvalScope{stack_value: HashMap::new()};

  let rustl_nil = Rc::new(RustlValue::create_obj());

  let AstKind::ProgramAst(ref statements) = ast.kind else {
    return ();
  };

  for mut ast_ in statements {
    eval_expression(&mut ast_, &mut root_scope, rustl_nil.clone());
  }

  println!("scope:{root_scope:#?}")
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

fn eval_expression(ast:& Box<Expr>, cur_scope: &mut EvalScope, nil: Rc<RustlValue>) -> Option< Rc<RustlValue> >{
  match &ast.kind {
    AstKind::ProgramAst(_) => todo!(),
    AstKind::NumericLiteral(_) => {
      let value = create_numeric_value_from_ast(ast)?;
      Some(Rc::new(value))
    } ,
    AstKind::CharLiteral(char_string) => 
      Some(Rc::new(RustlValue::create_string(char_string))),
    AstKind::Let(name, value_ast) => {
      let value = eval_expression(&value_ast, cur_scope, nil.clone())?;
      cur_scope.stack_value.insert(name.to_string(), value);
      Some(nil) 
    },
    AstKind::AstName(name_string) => 
      Some(Rc::new(RustlValue::create_string(&name_string))) ,
    AstKind::BinaryOp(op_string, lhs, rhs) => {
      let lhs_value = eval_expression(lhs, cur_scope, nil.clone())?;
      let rhs_value = eval_expression(rhs, cur_scope, nil.clone())?;
      if op_string.eq("+") {
        return rustl_add(&lhs_value, &rhs_value);
      } else if op_string.eq("*") {
        return lhs_value.numeric_multi(&rhs_value);
      } else if op_string.eq("/") {
        return lhs_value.numeric_div(&rhs_value);
      }else if op_string.eq("-") {
        return lhs_value.numeric_sub(&rhs_value);
      }
      None
    },
    AstKind::FnDeclAst(_, _, _) => None,
    AstKind::RustlAnnotation(_) =>  None,
    AstKind::AstNull => None,
    AstKind::FnDeclArgs(_) => None,
  }
}