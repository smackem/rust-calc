extern crate rcalc;

use std::io;
use std::io::{ BufWriter, Write };
use rcalc::{ Calculator, RuntimeItem, Value };

fn main() {
    let mut calculator = Calculator::new();
    let mut precision = None;
    let mut parallel_srcs = None;

    loop {
        print!("> ");
        io::stdout().flush().expect("stdout error");

        match read_line().trim() {
            "" => (),
            "#q" => break,
            "#:" => {
                parallel_srcs = match parallel_srcs {
                    Some(..) => None,
                    None => Some(vec![]),
                } 
            },
            "#!" => {
                if let Some(ref srcs) = parallel_srcs {
                    let item = calculator.calc_parallel(srcs.clone());
                    print_item(item, &precision);
                };
                parallel_srcs = None;
            },
            "#j" => {
                let mut buf = Vec::new();
                let _ = {
                    let mut writer = BufWriter::new(&mut buf);
                    calculator.write_json(&mut writer).expect("json output error");
                };
                println!("{}", String::from_utf8(buf).unwrap());
            },
            "?" => {
                println!("#q - quit rcalc");
                println!("#: - enter or leave parallel mode");
                println!("#! - leave parallel mode and evaluate all input concurrently");
                println!("#j - print a JSON representation of the current values");
            },
            line if line.starts_with("#precision ") => {
                if let Result::Ok(p) = line.split_whitespace().last().unwrap().parse::<usize>() {
                    precision = Some(p);
                } else {
                    precision = None;
                }
            },
            line => {
                match parallel_srcs {
                    Some(ref mut srcs) => srcs.push(line.to_string()),
                    None => {
                        match calculator.calc(line) {
                            Ok(item) => print_item(item, &precision),
                            Err(msg) => println!("{}", msg),
                        }
                    }
                }
            }
        }
    }
}

fn read_line() -> String {
    let mut buf = String::new();
    match io::stdin().read_line(&mut buf) {
        Ok(_) => buf,
        Err(e) => {
            println!("{:?}", e);
            "".to_string()
        },
    }
}

fn print_item(item: &RuntimeItem, precision: &Option<usize>) {
    if let &RuntimeItem::Value(ref v) = item {
        print_value(v, precision);
    } else {
        println!("Function OK");
    };
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
