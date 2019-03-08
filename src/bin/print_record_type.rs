extern crate clap;
extern crate failure;
extern crate fitparse;

use clap::{App, Arg};
use std::fs::File;
use std::io::prelude::*;

use fitparse::fitparsingstate::FitParsingState;
use fitparse::FitMessage;

fn main() {
    let matches = App::new("fitparse")
        .version("0.1")
        .about("maybe parses fit files")
        .author("djk121@gmail.com")
        .arg(
            Arg::with_name("record_name")
                .help("name of record type to print")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("input")
                .help("fit file to parse")
                .required(true)
                .index(2),
        )
        .get_matches();

    let fname = matches.value_of("input").unwrap();
    let record_name = matches.value_of("record_name").unwrap();

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

    println!("WUT: {:?}", file_header);
    let mut inp = o;
    println!("len: {:?}", inp.len());
    let mut num = 0;
    let ps = &mut parsing_state;

    let mut messages = vec![];

    while inp.len() > 2 {
        //println!("message #{}", num);
        match fitparse::parse_fit_message(inp, ps) {
            Ok((Some(_fm), out)) => {
                messages.push(_fm);
                inp = out;
            }
            Ok((None, out)) => {
                println!("index #{}: unknown message", num);
                inp = out;
            }
            Err(e) => {
                println!("{}", e);
                break;
            }
        }
        num = num + 1;
    }
    println!("Parsed num messages: {}", messages.len());
    for message in messages {
        match message {
            FitMessage::Data(ref dm) => {
                if dm.message_name() == record_name {
                    println!("{:#?}", message);
                }
            }
            _ => (),
        }
    }
}
