use std::usize;

use runtime::eval_ast;

pub mod lexer;
pub mod sytax;
pub mod runtime;
pub mod rustlv;

#[macro_use]
extern crate lazy_static;

fn main() {
  let mut read_token: Vec<lexer::Token> = vec![];

  let mut parse_code = 
  "
let string_variable = \"string_literal\";
let b = 'a';
print( \"b variable \" + b );
let c = 4.123 + 5 / 3 * 3 - 0.1;
@builtin
fn print (arg);
let d = string_variable + \"44\";
print(d);
print(c);
fn define_fn(not_used_arg) {
  let variable_in_fn = 3.1415926 * 1000;
  print(\"Hello, function definition!\");
  return variable_in_fn;
}
print( define_fn() );
print( !true || (false && false) );
let k = true && string_variable;
define_fn(123);
print(k);
let obj = {name: \"obj name\", loc_start: 1};
print(obj);
".to_string();
  let code_u8_arr =  unsafe { parse_code.as_bytes_mut() };
  parse_to_token(&mut read_token, code_u8_arr);

  let mut parser = sytax::Parser::new();
  let mut ctx = sytax::ParsingCtx::new(&read_token);
  // println!("{ctx:#?}");
  if parser.parse(&mut ctx, &code_u8_arr).is_err(){
    return ();
  }

  // println!("ast: \n{:#?}", parser.root);
  let Some(mut ast) = parser.root else {
    return ();
  };
  eval_ast(&mut ast);
  // println!("Hello, world!");
}

// fn read_and_parse(read_token: &mut Vec<lexer::Token>) -> Result<(), Box<dyn std::error::Error>>
// {
//   let file = File::open("rustl.rl")?;
//   let reader = BufReader::new(file);

//   for line in reader.lines() {
//     let mut read_line  = line?;
//     if read_line.len() == 0 {
//         continue;
//     }

//     let line_buffer: &mut [u8] = unsafe { read_line.as_bytes_mut() };
//     parse_to_token(read_token, line_buffer);
//   }

//   Ok(())

// }

fn parse_to_token(read_token: &mut Vec<lexer::Token>, mut buffer: &mut [u8]) -> () {
  let mut token_parsing_offset = 0;
  loop 
  {
    let Ok((mut token , size)) = lexer::Token::parse_from_string(&buffer) else  {
      if !buffer.is_empty() {
        // println!("not support is char parsing {}", buffer[0]);
        buffer = buffer.split_at_mut(1).1;
        token_parsing_offset += 1;
        // not support token yet!
        continue;
      }
      else {
        break;
      }
    };

    token.set_location(token_parsing_offset, token_parsing_offset + size);

    read_token.push(token);
    let ignore_space = || -> usize
    {
        let mut cursor = size ;
        loop {
            if cursor >= buffer.len() {
                break
            }

            if !buffer[cursor].is_ascii_whitespace()
            {
                break;
            }
            cursor += 1;
        }
        cursor
    };

    let next_position = ignore_space();
    token_parsing_offset += next_position;
    buffer = buffer.split_at_mut(next_position).1
  }
}