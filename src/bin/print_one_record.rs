extern crate clap;
extern crate failure;
extern crate fitparse;

use clap::{App, Arg};
use std::fs::File;

use fitparse::fitfile::FitFile;

fn main() {
    let matches = App::new("fitparse")
        .version("0.1")
        .about("maybe parses fit files")
        .author("djk121@gmail.com")
        .arg(
            Arg::with_name("index")
                .help("index # of record to print")
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
    let index = matches.value_of("index").unwrap().parse::<usize>().unwrap();

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
    println!("{:#?}", ff.messages[index]);
}
