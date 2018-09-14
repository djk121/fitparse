extern crate fitparse;
extern crate clap;
extern crate failure;

use std::io;
use std::io::prelude::*;
use std::fs::File;
use clap::{App, Arg};

use fitparse::fitparsingstate::FitParsingState;


fn main() {
    let matches = App::new("fitparse")
                    .version("0.1")
                    .about("maybe parses fit files")
                    .author("djk121@gmail.com")
                    .arg(Arg::with_name("input")
                        .help("fit file to parse")
                        .required(true)
                        .index(1))
                    .get_matches();

    let fname = matches.value_of("input").unwrap();

    let mut f = match File::open(fname) {
        Ok(fi) => fi,
        _ => panic!("boo")
    };
    let mut v = vec![];
    f.read_to_end(&mut v);

    let (file_header, o) = match fitparse::FitFileHeader::parse(&v) {
        Ok((ffh, o)) => (ffh, o),
        _ => panic!("unable to read header")
    };
    let mut parsing_state = FitParsingState::new();

    println!("WUT: {:?}", file_header);
    let mut inp = &o[..(o.len()-2)];
    println!("len: {:?}", inp.len());
    let mut num = 0;
    let ps = &mut parsing_state;

    loop {
        println!("message #{}", num);
        num = num + 1;

        match fitparse::parse_fit_message(inp, ps) {
            Ok((_fm, out)) => {
                println!("fm: {:#?}", _fm);
                inp = out;
                println!("len: {:?}", inp.len());
                //println!("after: inp[0..5]: {:?}, out[0..5]: {:?}", &inp[0..5], &out[0..5]);

            },
            Err(e) => { println!("{}", e); break; }
        }
    }
}
