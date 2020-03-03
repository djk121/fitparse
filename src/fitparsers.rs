use chrono::{DateTime, Duration, TimeZone, UTC};

use nom;
use nom::{be_f32, be_f64, le_f32, le_f64, Endianness};

use errors::{Error, Result};

use FitParseConfig;

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
    if endianness == Endianness::Big {
        be_f32(input)
    } else {
        le_f32(input)
    }
}

named_args!(parse_sint64_internal(endianness: Endianness)<i64>,
    i64!(endianness)
);

named_args!(parse_uint64_internal(endianness: Endianness)<u64>,
    u64!(endianness)
);

fn parse_float64_internal(input: &[u8], endianness: nom::Endianness) -> nom::IResult<&[u8], f64> {
    if endianness == Endianness::Big {
        be_f64(input)
    } else {
        le_f64(input)
    }
}

named_args!(parse_string_internal(num_bytes: usize)<String>,
    do_parse!(
        s: take!(num_bytes) >>
        (String::from_utf8_lossy(s).trim_matches(char::from(0)).to_string())
        //(CStr::from_bytes_with_nul(s).unwrap().to_str().unwrap().to_string())
    )
);

named_args!(parse_byte_internal(num_bytes: usize)<Vec<u8>>,
    do_parse!(
        b: take!(num_bytes) >>
        (b.to_vec())
    )
);

//pub static GARMIN_EPOCH: DateTime<UTC> = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
fn parse_date_time_internal(
    input: &[u8],
    parse_config: FitParseConfig,
) -> Result<(DateTime<UTC>, u32)> {
    // if the value is < 0x10000000, it's relative to device power on, else
    // it's a normal unix timestamp, relative to the garmin epoch time
    match parse_uint32(input, parse_config) {
        Ok(garmin_epoch_offset) => {
            let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
            match garmin_epoch_offset < 0x10000000 {
                true => Err(Error::unsupported_relative_timestamp()),
                false => {
                    let utc_dt = garmin_epoch + Duration::seconds(garmin_epoch_offset.into());
                    Ok((utc_dt, garmin_epoch_offset))
                }
            }
        }
        _ => Err(Error::parse_error("error parsing date time")),
    }
}

fn buffer(input: &[u8], desired_size: usize, endianness: Endianness) -> Vec<u8> {
    let mut res = std::vec::from_elem(0, desired_size);

    let mut len = input.len();
    if len == 0 {
        return res
    }

    len = len - 1;

    if endianness == Endianness::Big {
        let mut spot = desired_size;
        while len > 0 {
            res[spot] = input[len];
            len = len - 1;
            spot = spot - 1
        }
    } else {
        let mut idx = 0;
        while idx < len {
            res[idx] = input[idx];
            idx = idx + 1;
        }
    }

    res
}

#[macro_export]
macro_rules! nom_returning_internal_parser {
    ($func:ident, $input:expr, $endianness:expr) => {
        match $func($input, $endianness) {
            nom::IResult::Done(o, f) => Ok((f, o)),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(Error::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(Error::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    };
    ($func:ident, $input:expr) => {
        match $func($input) {
            nom::IResult::Done(o, f) => Ok((f, o)),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(Error::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(Error::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    };
}

#[macro_export]
macro_rules! nom_basic_internal_parser {
    ($func:ident, $input:expr, $endianness:expr) => {
        match $func($input, $endianness) {
            nom::IResult::Done(_, f) => Ok(f),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(Error::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(Error::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    };
    ($func:ident, $input:expr) => {
        match $func($input) {
            nom::IResult::Done(_, f) => Ok(f),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(Error::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(Error::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    };
}

#[macro_export]
macro_rules! nom_internal_parser {
    ($func:ident, $input:expr, $invalid_field_value:expr, $endianness:expr) => {
        match $func($input, $endianness) {
            nom::IResult::Done(_, f) => match f == $invalid_field_value {
                true => {
                    Err(Error::parse_invalid_field_value())
                },
                false => Ok(f),
            },
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(Error::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(Error::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    };
    ($func:ident, $input:expr, $invalid_field_value:expr) => {
        match $func($input) {
            nom::IResult::Done(_, f) => match f == $invalid_field_value {
                true => Err(Error::parse_invalid_field_value()),
                false => Ok(f),
            },
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(Error::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(Error::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(Error::parse_error(e.description())),
        }
    };
}

#[macro_export]
macro_rules! nom_internal_nonzero_parser {
    ($func:ident, $input:expr) => {
        match nom_internal_parser!($func, $input)? {
            (num, _) => {
                match num {
                    0 => Err(Error::parse_invalid_field_value()),
                    _ => Ok(num),
                }
            }
        }
    };
    ($func:ident, $input:expr, $endianness:expr) => {
        match nom_internal_parser!($func, $input, $endianness)? {
            (num, _) => match num {
                0 => Err(Error::parse_invalid_field_value()),
                _ => Ok(num),
            },
        }
    };
}

macro_rules! nom_parser {
    ("bool") => {
        pub fn parse_bool(input: &[u8], _parse_config: FitParseConfig) -> Result<bool> {
            nom_basic_internal_parser!(parse_bool_internal, input)
        }
    };
    ("enum") => {
        pub fn parse_enum(input: &[u8], _parse_config: FitParseConfig) -> Result<u8> {
            nom_internal_parser!(parse_uint8_internal, input, 0xFF)
        }
    };
    ("sint8") => {
        pub fn parse_sint8(input: &[u8], _parse_config: FitParseConfig) -> Result<i8> {
            nom_internal_parser!(parse_sint8_internal, input, 0x7F)
        }
    };
    ("uint8") => {
        pub fn parse_uint8(input: &[u8], _parse_config: FitParseConfig) -> Result<u8> {
            nom_internal_parser!(parse_uint8_internal, input, 0xFF)
        }
    };
    ("uint8z") => {
        pub fn parse_uint8z(input: &[u8], _parse_config: FitParseConfig) -> Result<u8> {
            nom_internal_parser!(parse_uint8_internal, input, 0x00)
        }
    };
    ("sint16") => {
        pub fn parse_sint16(input: &[u8], parse_config: FitParseConfig) -> Result<i16> {
            if input.len() < 2 {
                let inp = buffer(input, 2, parse_config.endianness());
                nom_internal_parser!(parse_sint16_internal, &inp, 0x7FFF, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_sint16_internal, input, 0x7FFF, parse_config.endianness())
            }
        }
    };
    ("uint16") => {
        pub fn parse_uint16(input: &[u8], parse_config: FitParseConfig) -> Result<u16> {
            if input.len() < 2 {
                let inp = buffer(input, 2, parse_config.endianness());
                nom_internal_parser!(parse_uint16_internal, &inp, 0xFFFF, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_uint16_internal, input, 0xFFFF, parse_config.endianness())
            }
        }
    };
    ("uint16z") => {
        pub fn parse_uint16z(input: &[u8], parse_config: FitParseConfig) -> Result<u16> {
            if input.len() < 2 {
                let inp = buffer(input, 2, parse_config.endianness());
                nom_internal_parser!(parse_uint16_internal, &inp, 0x0000, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_uint16_internal, input, 0x0000, parse_config.endianness())
            }
        }
    };
    ("sint32") => {
        pub fn parse_sint32(input: &[u8], parse_config: FitParseConfig) -> Result<i32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_internal_parser!(parse_sint32_internal, &inp, 0x7FFFFFFF, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_sint32_internal, input, 0x7FFFFFFF, parse_config.endianness())
            }
        }
    };
    ("uint32") => {
        pub fn parse_uint32(input: &[u8], parse_config: FitParseConfig) -> Result<u32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_internal_parser!(parse_uint32_internal, &inp, 0xFFFFFFFF, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_uint32_internal, input, 0xFFFFFFFF, parse_config.endianness())
            }
        }
    };
    ("uint32z") => {
        pub fn parse_uint32z(input: &[u8], parse_config: FitParseConfig) -> Result<u32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_internal_parser!(parse_uint32_internal, &inp, 0x00000000, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_uint32_internal, input, 0x00000000, parse_config.endianness())
            }
        }
    };
    ("float32") => {
        pub fn parse_float32(input: &[u8], parse_config: FitParseConfig) -> Result<f32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_basic_internal_parser!(parse_float32_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_float32_internal, input, parse_config.endianness())
            }
        }
    };
    ("sint64") => {
        pub fn parse_sint64(input: &[u8], parse_config: FitParseConfig) -> Result<i64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_basic_internal_parser!(parse_sint64_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_sint64_internal, input, parse_config.endianness())
            }
        }
    };
    ("uint64") => {
        pub fn parse_uint64(input: &[u8], parse_config: FitParseConfig) -> Result<u64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_basic_internal_parser!(parse_uint64_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_uint64_internal, input, parse_config.endianness())
            }
        }
    };
    ("uint64z") => {
        pub fn parse_uint64z(input: &[u8], parse_config: FitParseConfig) -> Result<u64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_internal_parser!(parse_uint64_internal, &inp, 0x0000000000000000, parse_config.endianness())
            } else {
                nom_internal_parser!(parse_uint64_internal, input, 0x0000000000000000, parse_config.endianness())
            }
        }
    };
    ("float64") => {
        pub fn parse_float64(input: &[u8], parse_config: FitParseConfig) -> Result<f64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_basic_internal_parser!(parse_float64_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_float64_internal, input, parse_config.endianness())
            }
        }
    };
    ("string") => {
        pub fn parse_string(input: &[u8], parse_config: FitParseConfig) -> Result<String> {
            nom_basic_internal_parser!(parse_string_internal, input, parse_config.field_size())
        }
    };
    ("byte") => {
        pub fn parse_byte(input: &[u8], parse_config: FitParseConfig) -> Result<Vec<u8>> {
            nom_basic_internal_parser!(parse_byte_internal, input, parse_config.field_size())
        }
    };
    ("date_time") => {
        pub fn parse_date_time(
            input: &[u8],
            parse_config: FitParseConfig,
        ) -> Result<(DateTime<UTC>, u32)> {
            parse_date_time_internal(input, parse_config)
        }
    };
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
nom_parser!("byte");
nom_parser!("string");
nom_parser!("date_time");
