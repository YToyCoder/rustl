use std::{collections::HashMap, fmt::Debug, rc::Rc};

use crate::sytax::{AstKind, Expr};

use crate::rustlv::{create_numeric_value_from_ast, rustl_add, RustlV};

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

pub fn eval_ast(ast:&mut Expr, code: &[u8]) -> () {
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
      let log = |scope: &GlobalScope| match &scope.err {
        Some(RustlV::RustlObject(obj)) => {
          let borrow = obj.borrow_mut();
          crate::sytax::log_err("[rustl][runtime]",
            code, 
            borrow.get_field("start_pos")?.to_int_value()? as usize, 
            borrow.get_field("end_pos")?.to_int_value()? as usize, 
            &borrow.get_field("msg")?.to_string() );
          Some(())
        },
        _ => None,
      };
      
      if log(&root_scope).is_none() {
        println!("[rustl][error] {}",root_scope.err.unwrap().clone().to_string());
      }
      break;
    }
  }

  // println!("scope:{root_scope:#?}")
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
        cur_scope.set_error(&RustlV::new_error_obj(&format!("not found variable {}", name_string_no_borrow), ast.loc.start , ast.loc.end));
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
        "=="  => Some( RustlV::new_bool(lhs_value == rhs_value)),
        ">"   => lhs_value.numeric_gt(&rhs_value),
        ">="  => lhs_value.numeric_ge(&rhs_value),
        "<"   => lhs_value.numeric_lt(&rhs_value),
        "<="  => lhs_value.numeric_le(&rhs_value),
        _     => {
          cur_scope.set_error(&RustlV::new_error_obj(&format!("not support binary operator {}", op_string), ast.loc.start , ast.loc.end));
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

        if r.is_none() {
          cur_scope.finish();
          cur_scope.set_error(&RustlV::new_error_obj(&format!("function call arg is None {}", name), ast.loc.start , ast.loc.end));
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
        cur_scope.set_error(&RustlV::new_error_obj(&format!("not find any function {}", name), ast.loc.start , ast.loc.end));
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
          cur_scope.set_error(&RustlV::new_error_obj(&format!("not support unary operator{}", unary_op.clone()), ast.loc.start , ast.loc.end));
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
    AstKind::FieldSet(prefix, field_name, value) => {
      let prefix_value = eval_expression(prefix, cur_scope, ctx)?;
      let value_result = eval_expression(value, cur_scope, ctx)?;
      match prefix_value {
        RustlV::RustlObject(object) 
          => {
            object.borrow_mut()
                .set_field(field_name, value_result);
            None
          }
        _ => {
          cur_scope.finish();
          cur_scope.set_error(&RustlV::new_error_obj(&format!("try to set field in which not object"), ast.loc.start , ast.loc.end));
          None
        },
      }
    },
    AstKind::FieldGet(expr, field_name) => {
      let Some(value) = eval_expression(expr, cur_scope, ctx) else {
        if !cur_scope.finished() {
          cur_scope.finish();
          cur_scope.set_error(&RustlV::new_error_obj(&format!("field get error, expression eval error"), ast.loc.start , ast.loc.end));
        }
        return None
      };

      match value {
        RustlV::RustlObject(object) 
          => object.borrow_mut()
                .get_field(field_name)
                .and_then(|v| Some(v.clone())),
        _ => {
          cur_scope.finish();
          cur_scope.set_error(&RustlV::new_error_obj(&format!("try to get field in which not object"), ast.loc.start , ast.loc.end));
          None
        },
      }
    },
  }
}