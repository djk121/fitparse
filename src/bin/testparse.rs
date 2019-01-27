extern crate fitparse;
extern crate clap;
extern crate failure;

use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::rc::Rc;
use clap::{App, Arg};

use fitparse::fitparsingstate::FitParsingState;
use fitparse::FitMessage;
use fitparse::fittypes::{FitDataMessage, FitMessageRecord};
use fitparse::fittypes::FitDataMessage::Record;


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
    //let mut inp = &o[..(o.len()-2)];
    let mut inp = o;
    println!("len: {:?}", inp.len());
    let mut num = 0;
    let ps = &mut parsing_state;

    let mut messages = vec![];

    while inp.len() > 2 {
        println!("message #{}", num);
        num = num + 1;
        match fitparse::parse_fit_message(inp, ps) {
            Ok((_fm, out)) => {
                //println!("fm: {:#?}", _fm);
                messages.push(_fm);
                inp = out;
                //println!("len: {:?}", inp.len());
                //println!("after: inp[0..5]: {:?}, out[0..5]: {:?}", &inp[0..5], &out[0..5]);

            },
            Err(e) => { println!("{}", e); break; }
        }
    }
    println!("Messages: {}", messages.len());

    //let records = messages.into_iter()
                    //.filter(|ref fm| match fm { Data => true, Definition => false })
    //                .filter(|fm| match fm { FitMessage::Data(data) =>  true, _ => false })
                    //.map(|fdm| match fdm { FitMessage::Data(data) => data, _ => None })
                    //.filter(|fdm| match fdm { None => false, _ => true })
                    //.filter_map(|fm| match fm { FitMessage::Data(data) => data, _ => false })
    //                .collect::<Vec<FitMessage>>();

    //println!("Record Messages: {}", records.len());
    for rec in messages {
        match rec {
            FitMessage::Data(dm) => {
                match dm {
                    FitDataMessage::Record(r) => {
                        println!("record: {:?}", r.position_lat)
                    },
                    _ => ()
                }
            },
            _ => ()
        }
        //let FitMessage::Data{dm: FitDataMessage } = rec;
        //println!("Record: {:?}", &rec.header)
    }

    println!("inp.len() = {:?}", inp.len());
    println!("ffh: {:?}", file_header);
    println!("last 2 bytes: {:?}", &inp[inp.len()-2..]);
}
