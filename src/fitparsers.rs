//extern crate chrono;

//use chrono::NaiveDateTime;

use std::str::FromStr;
use chrono::{DateTime, UTC, Duration, TimeZone};

use nom;
use nom::{Endianness, be_f32, le_f32, be_f64, le_f64};

use errors::{Error, ErrorKind, Result};

//trace_macros!(true);

named!(pub parse_bool_internal<&[u8], bool>,
    do_parse!(
        u: take!(1) >>
        (u[0] > 0)
    )
);

named!(parse_uint8_internal<&[u8], u8>,
    do_parse!(
        u: take!(1) >>
        (u[0])
    )
);

named!(parse_sint8_internal<&[u8], i8>,
    do_parse!(
        u: take!(1) >>
        (u[0] as i8)
    )
);

named_args!(parse_sint16_internal(endianness: Endianness)<i16>,
    i16!(endianness)
);

named_args!(parse_uint16_internal(endianness: Endianness)<u16>,
    u16!(endianness)
);

named_args!(parse_sint32_internal(endianness: Endianness)<i32>,
    i32!(endianness)
);

named_args!(parse_uint32_internal(endianness: Endianness)<u32>,
    u32!(endianness)
);

fn parse_float32_internal(input: &[u8], endianness: nom::Endianness) -> nom::IResult<&[u8], f32> {
    if endianness == Endianness::Big { be_f32(input) }
    else { le_f32(input) }
}

named_args!(parse_sint64_internal(endianness: Endianness)<i64>,
    i64!(endianness)
);

named_args!(parse_uint64_internal(endianness: Endianness)<u64>,
    u64!(endianness)
);

fn parse_float64_internal(input: &[u8], endianness: nom::Endianness) -> nom::IResult<&[u8], f64> {
    if endianness == Endianness::Big { be_f64(input) }
    else { le_f64(input) }
}

named_args!(parse_string_internal(num_bytes: usize)<String>,
    do_parse!(
        s: take_str!(num_bytes) >>
        (String::from_str(s).unwrap())
    )
);


named_args!(parse_byte_internal(num_bytes: usize)<&[u8]>,
    do_parse!(
        b: take!(num_bytes) >>
        (&b)
    )
);


//pub static GARMIN_EPOCH: DateTime<UTC> = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
fn parse_date_time_internal(input: &[u8], endianness: nom::Endianness) -> Result<(DateTime<UTC>, u32, &[u8])> {
    // if the value is < 0x10000000, it's relative to device power on, else
    // it's a normal unix timestamp, relative to the garmin epoch time
    match parse_uint32(input, endianness)? {
        (garmin_epoch_offset, o) => {
            let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
            match garmin_epoch_offset < 0x10000000 {
                true => Err(Error::unsupported_relative_timestamp()),
                false => {
                    let utc_dt = garmin_epoch + Duration::seconds(garmin_epoch_offset.into());
                    Ok((utc_dt, garmin_epoch_offset, o))
                }
            }
        }
    }
}

#[macro_export]
macro_rules! nom_internal_parser {
    ($func:ident, $input:expr, $endianness:expr) => (
        match $func($input, $endianness) {
            nom::IResult::Done(o, f) => Ok((f,o)),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => Err(Error::parse_incomplete(amount)),
            nom::IResult::Incomplete(nom::Needed::Unknown) => Err(Error::parse_incomplete_unknown()),
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    );
    ($func:ident, $input:expr) => (
        match $func($input) {
            nom::IResult::Done(o, f) => Ok((f,o)),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => Err(Error::parse_incomplete(amount)),
            nom::IResult::Incomplete(nom::Needed::Unknown) => Err(Error::parse_incomplete_unknown()),
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    );
}

macro_rules! nom_internal_nonzero_parser {
    ($func:ident, $input:expr) => (
        match nom_internal_parser!($func, $input)? {
            (num, o) => {
                match num {
                    //0 => Err(Error::parse_zero()),
                    0 => Ok((None, o)),
                    _ => Ok((Some(num), o))
                }
            }
        }
    );
    ($func:ident, $input:expr, $endianness:expr) => (
        match nom_internal_parser!($func, $input, $endianness)? {
            (num, o) => {
                match num {
                    0 => Ok((None, o)),
                    _ => Ok((Some(num), o))
                }
            }
        }
    );

}


macro_rules! nom_parser {
    ("bool") => (
        pub fn parse_bool(input: &[u8]) -> Result<(bool, &[u8])> {
            nom_internal_parser!(parse_bool_internal, input)
        }
    );
    ("enum") => (
        pub fn parse_enum(input: &[u8]) -> Result<(u8, &[u8])> {
            nom_internal_parser!(parse_uint8_internal, input)
        }
    );
    ("sint8") => (
        pub fn parse_sint8(input: &[u8]) -> Result<(i8, &[u8])> {
            nom_internal_parser!(parse_sint8_internal, input)
        }
    );
    ("uint8") => (
        pub fn parse_uint8(input: &[u8]) -> Result<(u8, &[u8])> {
            nom_internal_parser!(parse_uint8_internal, input)
        }
    );
    ("uint8z") => (
        pub fn parse_uint8z(input: &[u8]) -> Result<(Option<u8>, &[u8])> {
            nom_internal_nonzero_parser!(parse_uint8_internal, input)
        }
    );
    ("sint16") => (
        pub fn parse_sint16(input: &[u8], endianness: Endianness) -> Result<(i16, &[u8])> {
            nom_internal_parser!(parse_sint16_internal, input, endianness)
        }
    );
    ("uint16") => (
        pub fn parse_uint16(input: &[u8], endianness: Endianness) -> Result<(u16, &[u8])> {
            nom_internal_parser!(parse_uint16_internal, input, endianness)
        }
    );
    ("uint16z") => (
        pub fn parse_uint16z(input: &[u8], endianness: Endianness) -> Result<(Option<u16>, &[u8])> {
            nom_internal_nonzero_parser!(parse_uint16_internal, input, endianness)
        }
    );
    ("sint32") => (
        pub fn parse_sint32(input: &[u8], endianness: Endianness) -> Result<(i32, &[u8])> {
            nom_internal_parser!(parse_sint32_internal, input, endianness)
        }
    );
    ("uint32") => (
        pub fn parse_uint32(input: &[u8], endianness: Endianness) -> Result<(u32, &[u8])> {
            nom_internal_parser!(parse_uint32_internal, input, endianness)
        }
    );
    ("uint32z") => (
        pub fn parse_uint32z(input: &[u8], endianness: Endianness) -> Result<(Option<u32>, &[u8])> {
            nom_internal_nonzero_parser!(parse_uint32_internal, input, endianness)
        }
    );
    ("float32") => (
        pub fn parse_float32(input: &[u8], endianness: Endianness) -> Result<(f32, &[u8])> {
            nom_internal_parser!(parse_float32_internal, input, endianness)
        }
    );
    ("sint64") => (
        pub fn parse_sint64(input: &[u8], endianness: Endianness) -> Result<(i64, &[u8])> {
            nom_internal_parser!(parse_sint64_internal, input, endianness)
        }
    );
    ("uint64") => (
        pub fn parse_uint64(input: &[u8], endianness: Endianness) -> Result<(u64, &[u8])> {
            nom_internal_parser!(parse_uint64_internal, input, endianness)
        }
    );
    ("uint64z") => (
        pub fn parse_uint64z(input: &[u8], endianness: Endianness) -> Result<(Option<u64>, &[u8])> {
            nom_internal_nonzero_parser!(parse_uint64_internal, input, endianness)
        }
    );
    ("float64") => (
        pub fn parse_float64(input: &[u8], endianness: Endianness) -> Result<(f64, &[u8])> {
            nom_internal_parser!(parse_float64_internal, input, endianness)
        }
    );
    ("string") => (
        pub fn parse_string(input: &[u8], num_bytes: usize) -> Result<(String, &[u8])> {
            nom_internal_parser!(parse_string_internal, input, num_bytes)
        }
    );
    ("byte") => (
        pub fn parse_byte(input: &[u8], num_bytes: usize) -> Result<(&[u8], &[u8])> {
            nom_internal_parser!(parse_byte_internal, input, num_bytes)
        }
    );
    ("date_time") => (
        pub fn parse_date_time(input: &[u8], endianness: Endianness) -> Result<(DateTime<UTC>, u32, &[u8])> {
            parse_date_time_internal(input, endianness)
        }
    );
}

nom_parser!("bool");
nom_parser!("enum");
nom_parser!("sint8");
nom_parser!("uint8");
nom_parser!("uint8z");
nom_parser!("sint16");
nom_parser!("uint16");
nom_parser!("uint16z");
nom_parser!("sint32");
nom_parser!("uint32");
nom_parser!("uint32z");
nom_parser!("float32");
nom_parser!("sint64");
nom_parser!("uint64");
nom_parser!("uint64z");
nom_parser!("float64");
nom_parser!("string");
nom_parser!("byte");
nom_parser!("date_time");


//trace_macros!(false);
