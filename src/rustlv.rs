use std::{cell::RefCell, collections::HashMap, fmt::{Debug, Display}, rc::Rc};

use crate::sytax::{AstKind, Expr};

#[derive(Debug, Clone)]
pub struct RustlObj 
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
pub enum RustlV {
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

pub fn rustl_add(l:& RustlV, r:& RustlV) -> Option<RustlV> {
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

pub fn create_numeric_value_from_ast(ast:&Box<Expr>) -> Option<RustlV> {
  let AstKind::NumericLiteral(ref numeric_string) = ast.kind else {
    return None;
  };

  match numeric_string.find('.') {
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