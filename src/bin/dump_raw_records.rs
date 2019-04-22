extern crate clap;
extern crate failure;
extern crate fitparse;
extern crate itertools;

use clap::{App, Arg};
use std::fs::File;

use fitparse::fitfile::FitFile;

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
    let mut ff = FitFile::new(1024 * 1024 * 10, true);
    match ff.parse(&mut f) {
        Err(e) => panic!("failed to parse file: {:?}", e),
        _ => (),
    }

    println!("Parsed num messages: {}", ff.messages.len());
    for rec in ff.iter() {
        println!("{:?}", rec);
        println!();
    }
}
