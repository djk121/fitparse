use chrono::{DateTime, Duration, TimeZone, UTC};

use nom;
use nom::{be_f32, be_f64, le_f32, le_f64, Endianness};

use errors::{Error, Result};

use FitParseConfig;

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

fn parse_date_time_internal(
    input: &[u8],
    parse_config: &FitParseConfig,
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
    // peculiarly, shifting stuff right seems to be the right way to handle the input
    // being smaller than desired. 

    let mut res = std::vec::from_elem(0, desired_size);

    match endianness {
        Endianness::Big => {
            let mut write_idx = desired_size;
            let mut read_idx = input.len();

            while read_idx > 0 {
                read_idx = read_idx - 1;
                write_idx = write_idx - 1;
                res[write_idx] = input[read_idx];
            }
        },
        _ => {
            for i in 0..input.len() {
                res[i] = input[i];
            }
           
        }
    };
    res
}

/*
fn buffer(input: &[u8], desired_size: usize, endianness: Endianness) -> Vec<u8> {
    let mut res = std::vec::from_elem(0, desired_size);

    let mut len = input.len();
    if len == 0 {
        return res;
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
*/

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
            (num, _) => match num {
                0 => Err(Error::parse_invalid_field_value()),
                _ => Ok(num),
            },
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
        pub fn parse_bool(input: &[u8], _parse_config: &FitParseConfig) -> Result<bool> {
            nom_basic_internal_parser!(parse_bool_internal, input)
        }
    };
    ("enum") => {
        pub fn parse_enum(input: &[u8], _parse_config: &FitParseConfig) -> Result<u8> {
            nom_internal_parser!(parse_uint8_internal, input, 0xFF)
        }

        #[allow(dead_code)]
        pub fn parse_enum_as_bytes(input: &[u8], _parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_uint8_internal, input)?;
            Ok(vec![val])
        }
    };
    ("sint8") => {
        pub fn parse_sint8(input: &[u8], _parse_config: &FitParseConfig) -> Result<i8> {
            nom_internal_parser!(parse_sint8_internal, input, 0x7F)
        }

        #[allow(dead_code)]
        pub fn parse_sint8_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_sint8_internal, input)?;

            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint8") => {
        pub fn parse_uint8(input: &[u8], _parse_config: &FitParseConfig) -> Result<u8> {
            nom_internal_parser!(parse_uint8_internal, input, 0xFF)
        }

        pub fn parse_uint8_as_bytes(input: &[u8], _parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_uint8_internal, input)?;
            Ok(vec![val])
        }
    };
    ("uint8z") => {
        pub fn parse_uint8z(input: &[u8], _parse_config: &FitParseConfig) -> Result<u8> {
            nom_internal_parser!(parse_uint8_internal, input, 0x00)
        }

        #[allow(dead_code)]
        pub fn parse_uint8z_as_bytes(input: &[u8], _parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            Ok(vec![parse_uint8z(input, _parse_config)?])
        }

    };
    ("sint16") => {
        pub fn parse_sint16(input: &[u8], parse_config: &FitParseConfig) -> Result<i16> {
            if input.len() < 2 {
                let inp = buffer(input, 2, parse_config.endianness());
                nom_internal_parser!(
                    parse_sint16_internal,
                    &inp,
                    0x7FFF,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_sint16_internal,
                    input,
                    0x7FFF,
                    parse_config.endianness()
                )
            }
        }

        #[allow(dead_code)]
        pub fn parse_sint16_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_sint16_internal, input, parse_config.endianness())?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint16") => {
        pub fn parse_uint16(input: &[u8], parse_config: &FitParseConfig) -> Result<u16> {
            if input.len() < 2 {
                let inp = buffer(input, 2, parse_config.endianness());
                nom_internal_parser!(
                    parse_uint16_internal,
                    &inp,
                    0xFFFF,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_uint16_internal,
                    input,
                    0xFFFF,
                    parse_config.endianness()
                )
            }
        }

        pub fn parse_uint16_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_uint16_internal, input, parse_config.endianness())?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint16z") => {
        pub fn parse_uint16z(input: &[u8], parse_config: &FitParseConfig) -> Result<u16> {
            if input.len() < 2 {
                let inp = buffer(input, 2, parse_config.endianness());
                nom_internal_parser!(
                    parse_uint16_internal,
                    &inp,
                    0x0000,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_uint16_internal,
                    input,
                    0x0000,
                    parse_config.endianness()
                )
            }
        }

        #[allow(dead_code)]
        pub fn parse_uint16z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_uint16z(input, parse_config)?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("sint32") => {
        pub fn parse_sint32(input: &[u8], parse_config: &FitParseConfig) -> Result<i32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_internal_parser!(
                    parse_sint32_internal,
                    &inp,
                    0x7FFFFFFF,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_sint32_internal,
                    input,
                    0x7FFFFFFF,
                    parse_config.endianness()
                )
            }
        }

        #[allow(dead_code)]
        pub fn parse_sint32_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_sint32_internal, input, parse_config.endianness())?;
           
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint32") => {
        pub fn parse_uint32(input: &[u8], parse_config: &FitParseConfig) -> Result<u32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_internal_parser!(
                    parse_uint32_internal,
                    &inp,
                    0xFFFFFFFF,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_uint32_internal,
                    input,
                    0xFFFFFFFF,
                    parse_config.endianness()
                )
            }
        }

        pub fn parse_uint32_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = nom_basic_internal_parser!(parse_uint32_internal, input, parse_config.endianness())?;
            
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint32z") => {
        pub fn parse_uint32z(input: &[u8], parse_config: &FitParseConfig) -> Result<u32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_internal_parser!(
                    parse_uint32_internal,
                    &inp,
                    0x00000000,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_uint32_internal,
                    input,
                    0x00000000,
                    parse_config.endianness()
                )
            }
        }

        #[allow(dead_code)]
        pub fn parse_uint32z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_uint32z(input, parse_config)?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("float32") => {
        pub fn parse_float32(input: &[u8], parse_config: &FitParseConfig) -> Result<f32> {
            if input.len() < 4 {
                let inp = buffer(input, 4, parse_config.endianness());
                nom_basic_internal_parser!(parse_float32_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_float32_internal, input, parse_config.endianness())
            }
        }

        #[allow(dead_code)]
        pub fn parse_float32_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_float32(input, parse_config)?;
          
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("sint64") => {
        pub fn parse_sint64(input: &[u8], parse_config: &FitParseConfig) -> Result<i64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_basic_internal_parser!(parse_sint64_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_sint64_internal, input, parse_config.endianness())
            }
        }

        #[allow(dead_code)]
        pub fn parse_sint64_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_sint64(input, parse_config)?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint64") => {
        pub fn parse_uint64(input: &[u8], parse_config: &FitParseConfig) -> Result<u64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_basic_internal_parser!(parse_uint64_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_uint64_internal, input, parse_config.endianness())
            }
        }

        #[allow(dead_code)]
        pub fn parse_uint64_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_uint64(input, parse_config)?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("uint64z") => {
        pub fn parse_uint64z(input: &[u8], parse_config: &FitParseConfig) -> Result<u64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_internal_parser!(
                    parse_uint64_internal,
                    &inp,
                    0x0000000000000000,
                    parse_config.endianness()
                )
            } else {
                nom_internal_parser!(
                    parse_uint64_internal,
                    input,
                    0x0000000000000000,
                    parse_config.endianness()
                )
            }
        }

        #[allow(dead_code)]
        pub fn parse_uint64z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_uint64z(input, parse_config)?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("float64") => {
        pub fn parse_float64(input: &[u8], parse_config: &FitParseConfig) -> Result<f64> {
            if input.len() < 8 {
                let inp = buffer(input, 8, parse_config.endianness());
                nom_basic_internal_parser!(parse_float64_internal, &inp, parse_config.endianness())
            } else {
                nom_basic_internal_parser!(parse_float64_internal, input, parse_config.endianness())
            }
        }

        #[allow(dead_code)]
        pub fn parse_float64_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            let val = parse_float64(input, parse_config)?;
            match parse_config.endianness() {
                Endianness::Big => {
                    Ok(val.to_be_bytes().to_vec())
                },
                Endianness::Little => {
                    Ok(val.to_le_bytes().to_vec())
                }
            }
        }
    };
    ("string") => {
        pub fn parse_string(input: &[u8], parse_config: &FitParseConfig) -> Result<String> {
            nom_basic_internal_parser!(parse_string_internal, input, parse_config.field_size())
        }
    };
    ("byte") => {
        pub fn parse_byte(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            nom_basic_internal_parser!(parse_byte_internal, input, parse_config.field_size())
        }

        pub fn parse_byte_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
            parse_byte(input, parse_config)
        }
    };
    ("date_time") => {
        pub fn parse_date_time(
            input: &[u8],
            parse_config: &FitParseConfig,
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
