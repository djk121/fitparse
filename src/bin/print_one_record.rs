extern crate clap;
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
                .index(2),
        )
        .arg(
            Arg::with_name("input")
                .help("fit file to parse")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("expanded")
                .help("print expanded representation of each record")
                .short("e")
                .long("expanded")
                .takes_value(false),
        )
        .get_matches();

    let fname = matches.value_of("input").unwrap();
    let index = matches.value_of("index").unwrap().parse::<usize>().unwrap();
    let expanded = matches.is_present("expanded");

    let mut f = match File::open(fname) {
        Ok(fi) => fi,
        _ => panic!("boo"),
    };

    let mut ff = FitFile::new(1024 * 1024 * 10, true);
    if let Err(e) = ff.parse(&mut f) { panic!("failed to parse file: {:?}", e) }

    println!("Parsed num messages: {}", ff.messages.len());
    if expanded {
        println!("{:#?}", ff.messages[index]);
    } else {
        println!("{}", ff.messages[index]);
    }
}
