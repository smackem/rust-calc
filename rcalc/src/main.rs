extern crate rcalc;

use std::io;
use std::io::Write;
use rcalc::{ Calculator, RuntimeItem, Value };

fn main() {
    let mut calculator = Calculator::new();
    let mut precision = None;

    loop {
        print!("> ");
        io::stdout().flush().expect("stdout error");

        let line: String = {
            let mut buf = String::new();
            match io::stdin().read_line(&mut buf) {
                Ok(_) => buf,
                Err(e) => {
                    println!("{:?}", e);
                    "".to_string()
                },
            }
        };

        match line.trim() {
            "" => (),
            "#q" => break,
            line if line.starts_with("#precision ") => {
                if let Result::Ok(p) = line.split_whitespace().last().unwrap().parse::<usize>() {
                    precision = Some(p);
                } else {
                    precision = None;
                }
            },
            line => {
                match calculator.calc(line) {
                    Ok(item) => {
                        if let &RuntimeItem::Value(ref v) = item {
                            print_value(v, &precision);
                        } else {
                            println!("Function OK");
                        };
                    },
                    Err(msg) => println!("{}", msg),
                }
            }
        }
    }
}

fn print_value(v: &Value, precision: &Option<usize>) {
    match v {
        &Value::Integer(n) => {
            println!("= {}", n);
            println!("  {:#x}", n);
            println!("  {:#b}", n);
        },
        _ => {
            match *precision {
                Some(n) => println!("= {val:.prec$}", val = v, prec = n),
                None => println!("= {}", v),
            }
        },
    }
}
