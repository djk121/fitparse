use std::io;

//extern crate backtrace;
//use backtrace::Backtrace;

use errors;
use errors::Result;

use fitparsingstate::FitParsingState;
use {parse_fit_message, FitFileHeader, FitMessage, FitDataMessage};

pub struct FitFile {
    max_file_size: usize,
    retain_bytes: bool,
    pub messages: Vec<FitMessage>,
}

/*
enum FitMessageParseResult {
    Message(FitMessage),
    Error(Error)
}
*/

impl FitFile {
    pub fn new(max_file_size: usize, retain_bytes: bool) -> Self {
        FitFile {
            max_file_size: max_file_size,
            retain_bytes: retain_bytes,
            messages: vec![],
        }
    }

    pub fn parse(&mut self, byte_source: &mut dyn io::Read) -> Result<()> {
        //let mut header_bytes = Vec::with_capacity(14);
        let mut header_bytes = std::vec::from_elem(0, 14);

        // header is supposed to be 14 bytes. it contains the file length
        match byte_source.read(&mut header_bytes) {
            Err(e) => panic!("error reading file header: {:?}", e),
            _ => (),
        }

        //println!("header bytes: {:x?}", &header_bytes);
        //println!("{:?}", &header_bytes);

        let file_header = match FitFileHeader::parse(&header_bytes) {
            Ok((_, ffh)) => ffh,
            Err(e) => panic!("unable to parse header: {:?}", e),
        };

        if file_header.data_size as usize > self.max_file_size {
            return Err(errors::fit_file_too_large(
                self.max_file_size,
                file_header.data_size as usize,
            ));
        }

        let mut record_bytes = std::vec::from_elem(0, (file_header.data_size + 2) as usize);
        let mut records_starting_read_position = 0;
        // if the header was only 12 bytes, we need to put 2 back
        if file_header.header_size == 12 {
            record_bytes[0] = header_bytes[12];
            record_bytes[1] = header_bytes[13];
            records_starting_read_position = 2;
        }

        match byte_source.read(&mut record_bytes[records_starting_read_position..]) {
            Err(e) => panic!("error reading file records: {:?}", e),
            _ => (),
        }

        let mut parsing_state = FitParsingState::new();
        parsing_state.retain_bytes = self.retain_bytes;

        let mut inp = &record_bytes[0..];
        let mut num = 0;
        let ps = &mut parsing_state;

        while inp.len() > 2 {
            match parse_fit_message(inp, ps) {
                Ok((fm, out)) => {
                    //println!("{}", fm);
                    self.messages.push(fm);
                    inp = out;
                },
                Err(e) => {
                    panic!("error: {}", e);
                }
            }
            num = num + 1;
        }

        Ok(())
    }

    pub fn iter<'a>(&'a self) -> IterFitFile<'a> {
        IterFitFile {
            inner: self,
            pos: 0,
            message_names: vec![],
        }
    }

    pub fn iter_message_names<'a>(&'a self, message_names: Vec<&'static str>) -> IterFitFile<'a> {
        IterFitFile {
            inner: self,
            pos: 0,
            message_names: message_names,
        }
    }

    pub fn sport(&self) -> Result<String> {
        for sport_mesg in self.iter_message_names(vec!["Sport"]) {
            if let FitMessage::Data(FitDataMessage::Sport(ref m)) = sport_mesg {
                return Ok(m.sport.get_single()?.to_string());
            }
        }
        Err(errors::sport_message_not_present())
    }
}

pub struct IterFitFile<'a> {
    inner: &'a FitFile,
    message_names: Vec<&'static str>,
    pos: usize,
}

impl<'a> Iterator for IterFitFile<'a> {
    type Item = &'a FitMessage;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.inner.messages.len() {
            None
        } else {
            if self.message_names.is_empty() {
                self.pos += 1;
                self.inner.messages.get(self.pos - 1)
            } else {
                loop {
                    if self.pos >= self.inner.messages.len() {
                        return None;
                    }
                    self.pos += 1;
                    if let Some(m) = self.inner.messages.get(self.pos - 1) {
                        if self.message_names.iter().any(|&x| x == m.message_name()) {
                            return self.inner.messages.get(self.pos - 1);
                        }
                    }
                }
            }
        }
    }
}
