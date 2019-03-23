extern crate clap;
extern crate failure;
extern crate fitparse;

use clap::{App, Arg};
use std::fs::File;

use fitparse::fitfile::FitFile;
use fitparse::fittypes::FitDataMessage;
use fitparse::FitMessage;

fn main() {
    let matches = App::new("fitparse")
        .version("0.1")
        .about("maybe parses fit files")
        .author("djk121@gmail.com")
        .arg(
            Arg::with_name("input")
                .help("fit file to parse")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("silent")
                .help("don't print to stdout")
                .short("s")
                .long("silent")
                .takes_value(false),
        )
        .get_matches();

    let fname = matches.value_of("input").unwrap();
    let silent = matches.is_present("silent");

    let mut f = match File::open(fname) {
        Ok(fi) => fi,
        _ => panic!("boo"),
    };

    let mut ff = FitFile::new(1024 * 1024 * 10, true);
    match ff.parse(&mut f) {
        Err(e) => panic!("failed to parse file: {:?}", e),
        _ => (),
    }

    for rec in ff.iter_message_name("Record") {
        if let FitMessage::Data(FitDataMessage::Record(ref r)) = *rec {
            if silent == false {
                println!("distance: {:?}", r.distance);
            }
        }
    }
}
