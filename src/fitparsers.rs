use chrono::{DateTime, Duration, TimeZone, UTC};

use nom;
use nom::number::Endianness;
use errors;
use errors::Result;

use {
    FitParseConfig, FitFieldDefinition, FitRecordHeader, FitCompressedTimestampHeader, 
    FitNormalRecordHeader, FitGlobalMesgNum, FitDefinitionMessage,
    FitDeveloperFieldDefinition, FitFileHeader
};

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
                true => Err(errors::unsupported_relative_timestamp()),
                false => {
                    let utc_dt = garmin_epoch + Duration::seconds(garmin_epoch_offset.into());
                    Ok((utc_dt, garmin_epoch_offset))
                }
            }
        }
        _ => Err(errors::parse_error("error parsing date time")),
    }
}

fn buffer(input: &[u8], desired_size: usize, endianness: nom::number::Endianness) -> Vec<u8> {
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

#[macro_export]
macro_rules! nom_returning_internal_parser {
    ($func:ident, $input:expr, $endianness:expr) => {
        match $func($input, $endianness) {
            nom::IResult::Done(o, f) => Ok((f, o)),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(errors::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(errors::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(errors::parse_error(e.description())),
        }
    };
    ($func:ident, $input:expr) => {
        match $func($input) {
            nom::IResult::Done(o, f) => Ok((f, o)),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(errors::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(errors::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(errors::parse_error(e.description())),
        }
    };
}

#[macro_export]
macro_rules! nom_basic_internal_parser {
    ($func:ident, $input:expr, $endianness:expr) => {
        match $func($input, $endianness) {
            nom::IResult::Done(_, f) => Ok(f),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(errors::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(errors::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(errors::parse_error(e.description())),
        }
    };
    ($func:ident, $input:expr) => {
        match $func($input) {
            nom::IResult::Done(_, f) => Ok(f),
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(errors::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(errors::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(errors::parse_error(e.description())),
        }
    };
}

#[macro_export]
macro_rules! nom_internal_parser {
    ($func:ident, $input:expr, $invalid_field_value:expr, $endianness:expr) => {
        match $func($input, $endianness) {
            nom::IResult::Done(_, f) => match f == $invalid_field_value {
                true => Err(errors::parse_invalid_field_value()),
                false => Ok(f),
            },
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(errors::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(errors::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(errors::parse_error(e.description())),
        }
    };
    ($func:ident, $input:expr, $invalid_field_value:expr) => {
        match $func($input) {
            nom::IResult::Done(_, f) => match f == $invalid_field_value {
                true => Err(errors::parse_invalid_field_value()),
                false => Ok(f),
            },
            nom::IResult::Incomplete(nom::Needed::Size(amount)) => {
                Err(errors::parse_incomplete(amount))
            }
            nom::IResult::Incomplete(nom::Needed::Unknown) => {
                Err(errors::parse_incomplete_unknown())
            }
            nom::IResult::Error(e) => Err(errors::parse_error(e.description())),
        }
    };
}

#[macro_export]
macro_rules! nom_internal_nonzero_parser {
    ($func:ident, $input:expr) => {
        match nom_internal_parser!($func, $input)? {
            (num, _) => match num {
                0 => Err(errors::parse_invalid_field_value()),
                _ => Ok(num),
            },
        }
    };
    ($func:ident, $input:expr, $endianness:expr) => {
        match nom_internal_parser!($func, $input, $endianness)? {
            (num, _) => match num {
                0 => Err(errors::parse_invalid_field_value()),
                _ => Ok(num),
            },
        }
    };
}


pub fn parse_bool(input: &[u8], parse_config: &FitParseConfig) -> Result<bool> {
    let (_, res) = parse_bool_inner(input, parse_config).map_err(|e| errors::nom_parsing_error(e.to_string()))?;
    Ok(res[0] > 0)
}

fn parse_bool_inner<'a>(input: &'a [u8], _parse_config: &FitParseConfig) -> nom::IResult<&'a [u8], &'a [u8]> {
    let (i, res) = nom::bytes::complete::take(1usize)(input)?;
    Ok((i, res))
}

#[allow(dead_code)]
pub fn parse_enum(input: &[u8], _parse_config: &FitParseConfig) -> Result<u8> {
    let parser = || -> nom::IResult<&[u8], &[u8]> {
        nom::bytes::complete::take(1usize)(input)
    };
    
    let (_, res) = parser()?;
    match res[0] {
        0xFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }        
}

#[allow(dead_code)]
pub fn parse_enum_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_enum(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
   
    Ok(vec![val])
}

#[allow(dead_code)]
pub fn parse_sint8<'a>(input: &'a [u8], _parse_config: &FitParseConfig) -> Result<i8> {
    let parser = || -> nom::IResult<&'a [u8], i8> {
        nom::number::complete::le_i8(input)
    };

    let (_, res) = parser()?;
    match res {
        0x7F => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}

#[allow(dead_code)]
pub fn parse_sint8_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_sint8(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Big => Ok(val.to_be_bytes().to_vec()),
        Endianness::Little => Ok(val.to_le_bytes().to_vec())
    }
}

#[allow(dead_code)]
pub fn parse_uint8<'a>(input: &'a [u8], _parse_config: &FitParseConfig) -> Result<u8> {
    let parser = || -> nom::IResult<&'a [u8], u8> {
        //nom::bytes::complete::take(1_usize)(input)
        nom::number::complete::le_u8(input)
    };
    
    let (_, val) = parser()?;
    match val {
        0xFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}

#[allow(dead_code)]
pub fn parse_uint8_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint8(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };

    match parse_config.endianness() {
        Endianness::Big => Ok(val.to_be_bytes().to_vec()),
        Endianness::Little => Ok(val.to_le_bytes().to_vec())
    }
}

#[allow(dead_code)]
pub fn parse_uint8z(input: &[u8], parse_config: &FitParseConfig) -> Result<u8> {
    let val = parse_uint8(input, parse_config)?;
    match val {
        0x00 => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}

#[allow(dead_code)]
pub fn parse_uint8z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint8z(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    Ok(vec![val])
}

/*
#[allow(dead_code)]
pub fn parse_sint16(input: &[u8], parse_config: &FitParseConfig) -> Result<i16> {
    let v = match input.len() < 2 {
        true => buffer(input, 2, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], i16> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_i16(&v),
            Endianness::Big => nom::number::complete::be_i16(&v)
        }
    };

    let (_, val) = parser()?;
    match val {
        0x7FFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}
*/

#[allow(dead_code)]
pub fn parse_sint16<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<i16> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 2 {
        true => {
            buf = buffer(input, 2, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], i16> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_i16(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_i16(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val) = parser(inp)?;
    match val {
        0x7FFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }  
}  

#[allow(dead_code)]
pub fn parse_sint16_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_sint16(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
} 

#[allow(dead_code)]
pub fn parse_uint16<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<u16> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 2 {
        true => {
            buf = buffer(input, 2, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], u16> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_u16(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_u16(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val) = parser(inp)?;
    match val {
        0xFFFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }  
}  

#[allow(dead_code)]
pub fn parse_uint16_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint16(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

#[allow(dead_code)]
pub fn parse_uint16z(input: &[u8], parse_config: &FitParseConfig) -> Result<u16> {
    let val = parse_uint16(input, parse_config)?;
    match val {
        0x0000 => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}

#[allow(dead_code)]
pub fn parse_uint16z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint16z(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Big => {
            Ok(val.to_be_bytes().to_vec())
        },
        Endianness::Little => {
            Ok(val.to_le_bytes().to_vec())
        }
    }
}

/*
#[allow(dead_code)]
pub fn parse_sint32(input: &[u8], parse_config: &FitParseConfig) -> Result<i32> {
    let v = match input.len() < 4 {
        true => buffer(input, 4, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], i32> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_i32(&v),
            Endianness::Big => nom::number::complete::be_i32(&v)
        }
    };

    let (_, val) = parser()?;
    match val {
        0x7FFFFFFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}
*/

#[allow(dead_code)]
pub fn parse_sint32<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<i32> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 4 {
        true => {
            buf = buffer(input, 4, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], i32> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_i32(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_i32(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val) = parser(inp)?;
    match val {
        0x7FFFFFFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }  
}  

#[allow(dead_code)]
pub fn parse_sint32_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_sint32(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

/*
#[allow(dead_code)]
pub fn parse_uint32(input: &[u8], parse_config: &FitParseConfig) -> Result<u32> {
    let v = match input.len() < 4 {
        true => buffer(input, 4, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], u32> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_u32(&v),
            Endianness::Big => nom::number::complete::be_u32(&v)
        }
    };

    let (_, val) = parser()?;
    match val {
        0xFFFFFFFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }  
} 
*/ 

#[allow(dead_code)]
pub fn parse_uint32<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<u32> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 4 {
        true => {
            buf = buffer(input, 4, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], u32> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_u32(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_u32(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val) = parser(inp)?;
    match val {
        0xFFFFFFFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }  
}  


#[allow(dead_code)]
pub fn parse_uint32_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint32(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

#[allow(dead_code)]
pub fn parse_uint32z(input: &[u8], parse_config: &FitParseConfig) -> Result<u32> {
    let val = parse_uint32(input, parse_config)?;
    match val {
        0x00000000 => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}

#[allow(dead_code)]
pub fn parse_uint32z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint32z(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Big => {
            Ok(val.to_be_bytes().to_vec())
        },
        Endianness::Little => {
            Ok(val.to_le_bytes().to_vec())
        }
    }
}

/*
#[allow(dead_code)]
pub fn parse_float32(input: &[u8], parse_config: &FitParseConfig) -> Result<f32> {
    let v = match input.len() < 4 {
        true => buffer(input, 4, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], f32> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_f32(&v),
            Endianness::Big => nom::number::complete::be_f32(&v)
        }
    };

    let (_, val) = parser()?;
    Ok(val)
}  
*/

#[allow(dead_code)]
pub fn parse_float32<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<f32> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 4 {
        true => {
            buf = buffer(input, 4, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], f32> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_f32(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_f32(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val)= parser(inp)?;
    Ok(val)
}  

#[allow(dead_code)]
pub fn parse_float32_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_float32(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0.0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

/*
#[allow(dead_code)]
pub fn parse_sint64(input: &[u8], parse_config: &FitParseConfig) -> Result<i64> {    
    let v = match input.len() < 8 {
        true => buffer(input, 8, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], i64> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_i64(&v),
            Endianness::Big => nom::number::complete::be_i64(&v)
        }
    };

    let (_, val) = parser()?;
    Ok(val)
}
*/

#[allow(dead_code)]
pub fn parse_sint64<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<i64> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 8 {
        true => {
            buf = buffer(input, 8, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], i64> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_i64(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_i64(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val)= parser(inp)?;
    Ok(val)
}  

#[allow(dead_code)]
pub fn parse_sint64_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_sint64(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

/*
#[allow(dead_code)]
pub fn parse_uint64(input: &[u8], parse_config: &FitParseConfig) -> Result<u64> {
    let v = match input.len() < 8 {
        true => buffer(input, 8, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], u64> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_u64(&v),
            Endianness::Big => nom::number::complete::be_u64(&v)
        }
    };

    let (_, val) = parser()?;
    Ok(val)
}  
*/

#[allow(dead_code)]
pub fn parse_uint64<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<u64> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 8 {
        true => {
            buf = buffer(input, 8, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], u64> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_u64(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_u64(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val)= parser(inp)?;
    Ok(val)
}  

#[allow(dead_code)]
pub fn parse_uint64_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint64(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

#[allow(dead_code)]
pub fn parse_uint64z(input: &[u8], parse_config: &FitParseConfig) -> Result<u64> {
    let val = parse_uint64(input, parse_config)?;
    match val {
        0x0000000000000000 => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val)
    }
}

#[allow(dead_code)]
pub fn parse_uint64z_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_uint64z(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Big => {
            Ok(val.to_be_bytes().to_vec())
        },
        Endianness::Little => {
            Ok(val.to_le_bytes().to_vec())
        }
    }
}

/*
#[allow(dead_code)]
pub fn parse_float64(input: &[u8], parse_config: &FitParseConfig) -> Result<f64> { 
    let v = match input.len() < 8 {
        true => buffer(input, 8, parse_config.endianness()),
        false => input.to_vec()
    };

    let parser = || -> nom::IResult<&[u8], f64> {
        match parse_config.endianness() {
            Endianness::Little => nom::number::complete::le_f64(&v),
            Endianness::Big => nom::number::complete::be_f64(&v)
        }
    };

    let (_, val) = parser()?;
    Ok(val)
}  
*/

#[allow(dead_code)]
pub fn parse_float64<'a: 'b, 'b>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<f64> {
    let buf: Vec<u8>;
    let inp: &[u8];
    match input.len() < 8 {
        true => {
            buf = buffer(input, 8, parse_config.endianness());
            inp = &buf;
        },
        false => inp = &input,
    }

    let parser = |i: &'b [u8]| -> nom::IResult<&'b [u8], f64> {

        match parse_config.endianness() {
            Endianness::Little => {
                let (_, o) = nom::number::complete::le_f64(i)?;
                nom::IResult::Ok((&[], o))
            },
            Endianness::Big => {
                let (_, o) = nom::number::complete::be_f64(i)?;
                nom::IResult::Ok((&[], o))
            },
        }
    };

    let (_, val)= parser(inp)?;
    Ok(val)
} 

#[allow(dead_code)]
pub fn parse_float64_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_float64(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0.0,
        Err(o_e) => return Err(o_e)
    };
    match parse_config.endianness() {
        Endianness::Little => Ok(val.to_le_bytes().to_vec()),
        Endianness::Big => Ok(val.to_be_bytes().to_vec())
    }
}

#[allow(dead_code)]
pub fn parse_string(input: &[u8], parse_config: &FitParseConfig) -> Result<String> {
    let parser = || -> nom::IResult<&[u8], &[u8]> {
        nom::bytes::complete::take(parse_config.field_size())(input)
    };
    
    let (_, bytes) = parser()?;
    Ok(
        String::from_utf8_lossy(bytes)
            .trim_matches(char::from(0))
            .to_string()
    )
}

#[allow(dead_code)]
pub fn parse_byte(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let parser = || -> nom::IResult<&[u8], &[u8]> {
        nom::bytes::complete::take(parse_config.field_size())(input)
    };
    let (_, bytes) = parser()?;
    Ok(bytes.to_vec())
}

#[allow(dead_code)]
pub fn parse_byte_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    parse_byte(input, parse_config)
}

#[allow(dead_code)]
pub fn parse_date_time(input: &[u8], parse_config: &FitParseConfig) -> Result<(DateTime<UTC>, u32)> {
    parse_date_time_internal(input, parse_config)
}


pub fn parse_record_header(input: &[u8]) -> Result<(&[u8], FitRecordHeader)> {
    let parser = || -> nom::IResult<&[u8], FitRecordHeader> {
        nom::branch::alt((parse_normal_record_header, parse_compressed_timestamp_record_header))(input)
    };
    let (o, frh) = parser()?;
    Ok((o, frh))
}

pub fn parse_normal_record_header(i: &[u8]) -> nom::IResult<&[u8], FitRecordHeader> {
    let (o, res): (&[u8], (u8, u8, u8, u8, u8)) = nom::bits::bits(nom::sequence::tuple::<_, _, (_, _), _>((
        nom::bits::complete::tag(0x0, 1_usize),
        nom::bits::complete::take(1_usize),
        nom::bits::complete::take(1_usize),
        nom::bits::complete::tag(0x0, 1_usize),
        nom::bits::complete::take(4_usize)
    )))(i)?;

    let data_or_definition = res.1 as u8;
    let developer_data_flag = res.2 != 0;
    let local_mesg_num = res.4 as u16;

    nom::IResult::Ok((
        o,
        FitRecordHeader::Normal(
            FitNormalRecordHeader::new(data_or_definition, developer_data_flag, local_mesg_num)
        )
    ))

}

pub fn parse_compressed_timestamp_record_header(i: &[u8]) -> nom::IResult<&[u8], FitRecordHeader> {
    let (o, res): (&[u8], (u8, u8, u8)) = nom::bits::bits(nom::sequence::tuple::<_, _, (_, _), _>((
        nom::bits::complete::tag(0x1, 1_usize),
        nom::bits::complete::take(2_usize),
        nom::bits::complete::take(5_usize),
    )))(i)?;

    let local_mesg_num = res.1 as u16;
    let offset_secs = res.2 as u8;

    nom::IResult::Ok((
        o,
        FitRecordHeader::CompressedTimestamp(
            FitCompressedTimestampHeader {
                local_mesg_num,
                offset_secs
            } 
        )
    ))
}

pub fn parse_field_definition<'a> (input: &'a [u8]) -> nom::IResult<&'a [u8], FitFieldDefinition> {
    
    let base_parser = |inp: &'a [u8]| -> nom::IResult<&'a [u8], (u8, u8, u8)> {
        let (i, definition_number) = nom::bytes::complete::take(1_usize)(inp)?;
        let (i, field_size) = nom::bytes::complete::take(1_usize)(i)?;
        let (i, base_type) = nom::bytes::complete::take(1_usize)(i)?;

        nom::IResult::Ok((i, (definition_number[0], field_size[0], base_type[0])))
    };

    let fd_parser = nom::combinator::map_res(base_parser, |field_components: (u8, u8, u8)| {
        FitFieldDefinition::new(field_components.0, field_components.1.into(), field_components.2)
    });

    let (o, fd) = fd_parser(input)?;
    nom::IResult::Ok((o, fd))
}

pub fn parse_definition_message(i: &[u8], header: FitNormalRecordHeader) -> Result<(&[u8], FitDefinitionMessage)> {
    let parser = || -> nom::IResult<&[u8], (Endianness, &[u8], u8, Vec<FitFieldDefinition>, u8, Vec<FitDeveloperFieldDefinition>)> {
        let (i, _) = nom::bytes::complete::tag([0u8])(i)?;
        let (i, endianness_raw) = nom::bytes::complete::take(1_usize)(i)?;
        let endianness = match endianness_raw[0] {
            0 => Endianness::Little,
            _ => Endianness::Big,
        };
        let (i, global_message_number) = nom::bytes::complete::take(2_usize)(i)?;
        //let (i, global_message_number) = match endianness {
        //    Endianness::Little => nom::number::complete::le_u16(i)?,
        //    Endianness::Big => nom::number::complete::be_u16(i)?
        //};

        //let (i, num_fields) = nom::bytes::complete::take(1)(i)?;
        let (i, num_fields) = match endianness {
            Endianness::Little => nom::number::complete::le_u8(i)?,
            Endianness::Big => nom::number::complete::be_u8(i)?
        };

        let (i, field_definitions) = nom::multi::count(parse_field_definition, num_fields.into())(i)?;
        let (i, num_developer_fields) = match header.developer_fields_present {           
            true => {
                match endianness {
                    Endianness::Little => nom::number::complete::le_u8(i)?,
                    Endianness::Big => nom::number::complete::be_u8(i)?
                }
            },
            false => (i, 0)
        };
        let (i, developer_field_definitions) = nom::multi::count(parse_developer_field_definition, num_developer_fields.into())(i)?;

        nom::IResult::Ok((i, (endianness, global_message_number, num_fields.into(), field_definitions, num_developer_fields.into(), developer_field_definitions)))
    };

    let (i, (endianness, global_message_number, num_fields, field_definitions, num_developer_fields, developer_field_definitions)) = parser()?;

    let message_size = 
        field_definitions.iter().fold(
            0, |sum, val| sum + (val.base_type_size() as usize)
        ) 
        +
        developer_field_definitions.iter().fold(
            0, |sum, val| sum + val.field_size);

    let (global_mesg_num, _) = FitGlobalMesgNum::parse(global_message_number, endianness)?;

    Ok((
        i,
        FitDefinitionMessage {
            header, 
            endianness,
            global_mesg_num,
            num_fields,
            message_size,
            field_definitions,
            num_developer_fields: num_developer_fields.into(),
            developer_field_definitions,
        }
    ))   
}

pub fn parse_developer_field_definition(i: &[u8]) -> nom::IResult<&[u8], FitDeveloperFieldDefinition> {
    //let (i, definition_number) = nom::bytes::complete::take(1_usize)(i)?;
    let (i, definition_number) = nom::number::complete::be_u8(i)?;
    //let (i, field_size) = nom::bytes::complete::take(1_usize)(i)?;
    let (i, field_size) = nom::number::complete::be_u8(i)?;
    //let (i, developer_data_index) = nom::bytes::complete::take(1_usize)(i)?;
    let (i, developer_data_index) = nom::number::complete::be_u8(i)?;
    
    Ok((
        i,
        FitDeveloperFieldDefinition {
            definition_number,
            field_size: field_size.into(),
            developer_data_index
        }
    ))
}

pub fn parse_fit_file_header(i: &[u8]) -> Result<(&[u8], FitFileHeader)> {
    let parser = || -> nom::IResult<&[u8], (u8, u8, u16, u32, Vec<u8>)> {
        let (i, header_size) = nom::number::complete::be_u8(i)?;
        let (i, protocol_version) = nom::number::complete::be_u8(i)?;
        //let (i, protocol_version) = nom::bytes::complete::take(1_usize)(i)?;
        let (i, profile_version) = nom::number::complete::le_u16(i)?;
        let (i, data_size) = nom::number::complete::le_u32(i)?;
        let (i, _) = nom::bytes::complete::tag(".FIT")(i)?;
        let (i, crc) = match header_size == 14 {
            true => {
                nom::combinator::map(
                    nom::bytes::complete::take(2_usize),
                    |o: &[u8]| o.to_vec()
                )(i)?
            },
            false => {
                (i, Vec::new())
            }
        };
        
        nom::IResult::Ok((i, (header_size, protocol_version, profile_version, data_size, crc)))
    };

    let (i, (header_size, protocol_version, profile_version, data_size, crc)) = parser()?;
        
    Ok((
        i,
        FitFileHeader {
            header_size,
            protocol_version,
            profile_version,
            data_size,
            crc: crc.into()
        }
    ))
}