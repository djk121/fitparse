extern crate fitparse;
extern crate clap;
extern crate failure;

use std::io::prelude::*;
use std::fs::File;
use clap::{App, Arg};

use fitparse::fitparsingstate::FitParsingState;
//use fitparse::FitMessage;
//use fitparse::fittypes::FitDataMessage;


fn main() {
    let matches = App::new("fitparse")
                    .version("0.1")
                    .about("maybe parses fit files")
                    .author("djk121@gmail.com")
                    .arg(Arg::with_name("index")
                        .help("index # of record to print")
                        .required(true)
                        .index(1))
                    .arg(Arg::with_name("input")
                        .help("fit file to parse")
                        .required(true)
                        .index(2))
                    .get_matches();

    let fname = matches.value_of("input").unwrap();
    let index = matches.value_of("index").unwrap().parse::<usize>().unwrap();

    let mut f = match File::open(fname) {
        Ok(fi) => fi,
        _ => panic!("boo")
    };
    let mut v = vec![];
    match f.read_to_end(&mut v) {
        Ok(_) => (),
        Err(e) => panic!("error reading file: {:?}", e)
    }

    let (file_header, o) = match fitparse::FitFileHeader::parse(&v) {
        Ok((ffh, o)) => (ffh, o),
        _ => panic!("unable to read header")
    };
    let mut parsing_state = FitParsingState::new();

    println!("WUT: {:?}", file_header);
    //let mut inp = &o[..(o.len()-2)];
    let mut inp = o;
    println!("len: {:?}", inp.len());
    let mut num = 0;
    let ps = &mut parsing_state;

    let mut messages = vec![];

    while inp.len() > 2 {
        println!("message #{}", num);
        match fitparse::parse_fit_message(inp, ps) {
            Ok((Some(_fm), out)) => {
                messages.push(_fm);
                inp = out;
            },
            Ok((None,out)) => {
                println!("index #{}: unknown message", num);
                inp = out;
            }
            Err(e) => { println!("{}", e); break; }
        }
        num = num + 1;
    }
    println!("Parsed num messages: {}", messages.len());
    println!("{:#?}", messages[index]);
}