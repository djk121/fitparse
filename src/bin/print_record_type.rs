extern crate clap;
extern crate fitparse;

use clap::{App, Arg};
use std::fs::File;

use fitparse::fitfile::FitFile;
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
        .arg(
            Arg::with_name("unknowns")
                .help("print messages not in the fit profile")
                .short("u")
                .long("unknowns")
                .takes_value(false),
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
    let record_name = matches.value_of("record_name").unwrap();
    let unknowns = matches.is_present("unknowns");
    let expanded = matches.is_present("expanded");

    let mut f = match File::open(fname) {
        Ok(fi) => fi,
        _ => panic!("boo"),
    };

    let mut ff = FitFile::new(1024 * 1024 * 10, true);
    if let Err(e) = ff.parse(&mut f) { panic!("failed to parse file: {:?}", e) }

    println!("Parsed num messages: {}", ff.messages.len());
    for message in ff.iter() {
        if unknowns {
            //if message.message_name() == "Unknown" {
            //    println!("{:#?}", message);
            //}
            continue;
        }

        match message {
            FitMessage::Data(ref dm) => {
                if dm.message_name() == record_name {
                    if expanded {
                        println!("{:#?}", message);
                    } else {
                        println!("{}", message);
                    }
                }
            }
            FitMessage::Definition(_d) => {
                if record_name == "Definition" {
                    if expanded {
                        println!("{:#?}", message);
                    } else {
                        println!("{}", message);
                    }
                }
            }
        }
    }
}
