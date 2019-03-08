extern crate clap;
extern crate failure;
extern crate fitparse;
extern crate itertools;

use clap::{App, Arg};
use std::fs::File;
use std::io::prelude::*;

use itertools::Itertools;

use fitparse::fitparsingstate::FitParsingState;
use fitparse::fittypes::FitDataMessage;
use fitparse::FitMessage;

fn main() {
    let matches = App::new("dfr")
        .version("0.1")
        .about("maybe parses fit files")
        .author("djk121@gmail.com")
        .arg(
            Arg::with_name("input")
                .help("fit file to parse")
                .required(true)
                .index(1),
        )
        .get_matches();

    let fname = matches.value_of("input").unwrap();

    let mut f = match File::open(fname) {
        Ok(fi) => fi,
        _ => panic!("boo"),
    };
    let mut v = vec![];
    match f.read_to_end(&mut v) {
        Ok(_) => (),
        Err(e) => panic!("error reading file: {:?}", e),
    }

    let (file_header, o) = match fitparse::FitFileHeader::parse(&v) {
        Ok((ffh, o)) => (ffh, o),
        _ => panic!("unable to read header"),
    };
    let mut parsing_state = FitParsingState::new();

    let mut inp = o;
    println!("len: {:?}", inp.len());
    let mut num = 0;
    let ps = &mut parsing_state;

    let mut messages = vec![];

    while inp.len() > 2 {
        println!("message #{}", num);
        num = num + 1;
        match fitparse::parse_fit_message(inp, ps) {
            Ok((Some(_fm), out)) => {
                messages.push(_fm);
                inp = out;
            }
            Ok((None, out)) => {
                println!("unknown message");
                inp = out;
            }
            Err(e) => {
                println!("{}", e);
                break;
            }
        }
    }
    println!("Messages: {}", messages.len());
    for rec in messages {
        match rec {
            FitMessage::Data(dm) => match dm {
                FitDataMessage::Record(r) => {
                    println!("{:#?}", r);
                    println!("{:#010b}", &r.raw_bytes.iter().format(", "));
                    println!();
                }
                _ => (),
            },
            _ => (),
        }
    }

    println!("inp.len() = {:?}", inp.len());
    println!("ffh: {:?}", file_header);
    println!("last 2 bytes: {:?}", &inp[inp.len() - 2..]);
}
