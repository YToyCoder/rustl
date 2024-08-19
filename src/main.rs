use std::usize;

use runtime::eval_ast;

pub mod lexer;
pub mod sytax;
pub mod runtime;
#[macro_use]
extern crate lazy_static;

fn main() {
  let mut read_token: Vec<lexer::Token> = vec![];

  let mut parse_code = "
    let string_variable = \"string_literal\";
    let b = 'a';
    let c = 4.123 + 5 / 3 * 3 - 0.1;
    @builtin
    fn print (arg);
    ".to_string();
  parse_to_token(&mut read_token, unsafe { parse_code.as_bytes_mut() });

  let mut parser = sytax::Parser::new();
  let mut ctx = sytax::ParsingCtx::new(&read_token);
  // println!("{ctx:#?}");
  parser.parse(&mut ctx);
  println!("ast: \n{:#?}", parser.root);
  let Some(mut ast) = parser.root else {
    return ();
  };
  eval_ast(&mut ast);
  println!("Hello, world!");
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
  loop 
  {
    let Ok((token , size)) = lexer::Token::parse_from_string(&buffer) else  {
      if !buffer.is_empty() {
        buffer = buffer.split_at_mut(1).1;
        // not support token yet!
        continue;
      }
      else {
        break;
      }
    };

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
    buffer = buffer.split_at_mut(next_position).1
  }
}