use chrono::{DateTime, Duration, TimeZone, Utc};

use std::collections::HashMap;

use errors;
use errors::Result;
use nom::{self};
use nom::number::Endianness;
//use nom::error::VerboseError;

use paste::paste;

use {
    FitCompressedTimestampHeader, FitDefinitionMessage, FitDeveloperFieldDefinition,
    FitFieldDefinition, FitFileHeader, FitGlobalMesgNum, FitNormalRecordHeader, FitParseConfig,
    FitRecordHeader,
};

fn parse_date_time_internal(
    input: &[u8],
    parse_config: &FitParseConfig,
) -> Result<(DateTime<Utc>, u32)> {
    // if the value is < 0x10000000, it's relative to device power on, else
    // it's a normal unix timestamp, relative to the garmin epoch time
    match parse_uint32(input, parse_config) {
        Ok(garmin_epoch_offset) => {
            let garmin_epoch = Utc.ymd(1989, 12, 31).and_hms(0, 0, 0);
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
        }
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

/*
Necessary because otherwise the closure loses the type info.
*/
fn constrain<F, T>(f: F) -> F
where
    F: for<'a> Fn(&'a [u8]) -> nom::IResult<&'a [u8], T>,
{
    f
}

macro_rules! create_base_parser {
    ($input:expr, $parse_config:expr, $return_type:ty, $buffer_size:expr, $nom_le_parser:expr, $nom_be_parser:expr) => {
        {
            let buf: Vec<u8>;
            let inp: &[u8];
            match $input.len() < $buffer_size {
                true => {
                    buf = buffer($input, $buffer_size, $parse_config.endianness());
                    inp = &buf;
                }
                false => {
                    inp = &$input;
                }
            }

            let parser = constrain(|i: &[u8]| -> nom::IResult<&[u8], $return_type> {
                match $parse_config.endianness() {
                    Endianness::Little | Endianness::Native => {
                        let (_, o) = $nom_le_parser(i)?;
                        nom::IResult::Ok((&[], o.clone()))
                    }
                    Endianness::Big => {
                        let (_, o) = $nom_be_parser(i)?;
                        nom::IResult::Ok((&[], o.clone()))
                    }
                }
            });
            let (_, val) = parser(inp)?;
            //(parser, inp)
            val
        }
    }
}

macro_rules! create_base_parser_return {
    ($val:expr, $invalid_val:expr) => {
        match $val {
            $invalid_val => Err(errors::parse_invalid_field_value()),
            valid_val => Ok(valid_val),
        }
    };
    ($val:expr) => {
        Ok($val)
    }
}

macro_rules! create_as_bytes_variant {
    ($parser_fn_name:ident) => {
        paste! {

            #[allow(dead_code)]
            pub fn [<$parser_fn_name _as_bytes>](input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
                let val = match $parser_fn_name(input, parse_config) {
                    Ok(v) => v,
                    Err(o_e) => return Err(o_e),
                };
                match parse_config.endianness() {
                    Endianness::Little | Endianness::Native => Ok(val.to_le_bytes().to_vec()),
                    Endianness::Big => Ok(val.to_be_bytes().to_vec()),
                }
            }
        }
    }
}

macro_rules! create_z_variant {
    ($parser_fn_name:ident, $return_type:ty) => {
        paste! {
            #[allow(dead_code)]
            pub fn [<$parser_fn_name z>](input: &[u8], parse_config: &FitParseConfig) -> Result<$return_type> {
                let val = $parser_fn_name(input, parse_config)?;
                match val {
                    0x0000 => Err(errors::parse_invalid_field_value()),
                    valid_val => Ok(valid_val),
                }
            }
        }
    }
}

macro_rules! create_z_as_bytes_variant {
    ($parser_fn_name:ident) => {
        paste! {
            #[allow(dead_code)]
            pub fn [<$parser_fn_name z_as_bytes>](input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
                let val = match [<$parser_fn_name z>](input, parse_config) {
                    Ok(v) => v,
                    Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
                    Err(o_e) => return Err(o_e),
                };
                match parse_config.endianness() {
                    Endianness::Big => Ok(val.to_be_bytes().to_vec()),
                    Endianness::Little | Endianness::Native => Ok(val.to_le_bytes().to_vec()),
                }
            }
        }
    }
}



macro_rules! create_parsers {
    ($parser_fn_name:ident, $return_type:ty, $buffer_size:expr, $nom_le_parser:expr, $nom_be_parser:expr, $invalid_val:expr) => {
        pub fn $parser_fn_name<'a>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<$return_type> {
            let val = create_base_parser!(input, parse_config, $return_type, $buffer_size, $nom_le_parser, $nom_be_parser);
            create_base_parser_return!(val, $invalid_val)
            //create_invalid_val_section!(val, $invalid_val)
        }

        create_as_bytes_variant!($parser_fn_name);
        //create_z_variant!($parser_fn_name, $return_type);
        //create_z_as_bytes_variant!($parser_fn_name);
    };
    ($parser_fn_name:ident, $return_type:ty, $buffer_size:expr, $nom_le_parser:expr, $nom_be_parser:expr) => {
        pub fn $parser_fn_name<'a>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<$return_type> {
            let val = create_base_parser!(input, parse_config, $return_type, $buffer_size, $nom_le_parser, $nom_be_parser);
            create_base_parser_return!(val)
        }

        create_as_bytes_variant!($parser_fn_name);
        //create_z_variant!($parser_fn_name, $return_type);
        //create_z_as_bytes_variant!($parser_fn_name);
    }
}


pub fn parse_bool(input: &[u8], parse_config: &FitParseConfig) -> Result<bool> {
    let (_, res) = parse_bool_inner(input, parse_config)
        .map_err(|e| errors::nom_parsing_error(e.to_string()))?;
    Ok(res[0] > 0)
}

fn parse_bool_inner<'a>(
    input: &'a [u8],
    _parse_config: &FitParseConfig,
) -> nom::IResult<&'a [u8], &'a [u8]> {
    let (i, res) = nom::bytes::complete::take(1usize)(input)?;
    Ok((i, res))
}

#[allow(dead_code)]
pub fn parse_enum(input: &[u8], _parse_config: &FitParseConfig) -> Result<u8> {
    let parser = || -> nom::IResult<&[u8], &[u8]> { nom::bytes::complete::take(1usize)(input) };

    let (_, res) = parser()?;
    match res[0] {
        0xFF => Err(errors::parse_invalid_field_value()),
        valid_val => Ok(valid_val),
    }
}

#[allow(dead_code)]
pub fn parse_enum_as_bytes(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<u8>> {
    let val = match parse_enum(input, parse_config) {
        Ok(v) => v,
        Err(errors::FitParseError::ParseInvalidFieldValue { backtrace: _ }) => 0,
        Err(o_e) => return Err(o_e),
    };

    Ok(vec![val])
}

create_parsers!(parse_sint8, i8, 1, nom::number::complete::le_i8, nom::number::complete::le_i8, 0x7F);

create_parsers!(parse_uint8, u8, 1, nom::number::complete::le_u8, nom::number::complete::le_u8, 0xFF);
create_z_variant!(parse_uint8, u8);
create_z_as_bytes_variant!(parse_uint8);

create_parsers!(parse_sint16, i16, 2, nom::number::complete::le_i16, nom::number::complete::be_i16, 0x7FFF);

create_parsers!(parse_uint16, u16, 2, nom::number::complete::le_u16, nom::number::complete::be_u16, 0xFFFF);
create_z_variant!(parse_uint16, u16);
create_z_as_bytes_variant!(parse_uint16);

create_parsers!(parse_sint32, i32, 4, nom::number::complete::le_i32, nom::number::complete::be_i32, 0x7FFFFFFF);

create_parsers!(parse_uint32, u32, 4, nom::number::complete::le_u32, nom::number::complete::be_u32, 0xFFFFFFFF);
create_z_variant!(parse_uint32, u32);
create_z_as_bytes_variant!(parse_uint32);

create_parsers!(parse_float32, f32, 4, nom::number::complete::le_f32, nom::number::complete::be_f32);

create_parsers!(parse_sint64, i64, 8, nom::number::complete::le_i64, nom::number::complete::be_i64);

create_parsers!(parse_uint64, u64, 8, nom::number::complete::le_u64, nom::number::complete::be_u64);
create_z_variant!(parse_uint64, u64);
create_z_as_bytes_variant!(parse_uint64);

create_parsers!(parse_float64, f64, 8, nom::number::complete::le_f64, nom::number::complete::be_f64);

#[allow(dead_code)]
pub fn parse_string(input: &[u8], parse_config: &FitParseConfig) -> Result<String> {
    let parser = || -> nom::IResult<&[u8], &[u8]> {
        nom::bytes::complete::take(parse_config.field_size())(input)
    };

    let (_, bytes) = parser()?;
    Ok(String::from_utf8_lossy(bytes)
        .trim_matches(char::from(0))
        .to_string())
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
pub fn parse_date_time(
    input: &[u8],
    parse_config: &FitParseConfig,
) -> Result<(DateTime<Utc>, u32)> {
    parse_date_time_internal(input, parse_config)
}

#[allow(dead_code)]
pub fn parse_date_time_as_bytes(
    input: &[u8],
    parse_config: &FitParseConfig,
) -> Result<Vec<u8>> {
    parse_byte(input, parse_config)
}

pub fn parse_record_header(input: &[u8]) -> Result<(&[u8], FitRecordHeader)> {
    let parser = || -> nom::IResult<&[u8], FitRecordHeader> {
        nom::branch::alt((
            parse_normal_record_header,
            parse_compressed_timestamp_record_header,
        ))(input)
    };
    let (o, frh) = parser()?;
    Ok((o, frh))
}

pub fn parse_normal_record_header(i: &[u8]) -> nom::IResult<&[u8], FitRecordHeader> {
    let (o, res): (&[u8], (u8, u8, u8, u8, u8)) =
        //nom::bits::bits(nom::sequence::tuple::<_, _, (_, _), _>((
        nom::bits::bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(nom::sequence::tuple((
            nom::bits::complete::tag(0x0, 1_usize),
            nom::bits::complete::take(1_usize),
            nom::bits::complete::take(1_usize),
            nom::bits::complete::tag(0x0, 1_usize),
            nom::bits::complete::take(4_usize),
        )))(i)?;

    let data_or_definition = res.1 as u8;
    let developer_data_flag = res.2 != 0;
    let local_mesg_num = res.4 as u16;

    nom::IResult::Ok((
        o,
        FitRecordHeader::Normal(FitNormalRecordHeader::new(
            data_or_definition,
            developer_data_flag,
            local_mesg_num,
        )),
    ))
}

pub fn parse_compressed_timestamp_record_header(i: &[u8]) -> nom::IResult<&[u8], FitRecordHeader> {
    let (o, res): (&[u8], (u8, u8, u8)) =
        //nom::bits::bits(nom::sequence::tuple::<_, _, (_, _), _>((
        nom::bits::bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(nom::sequence::tuple((
            nom::bits::complete::tag(0x1, 1_usize),
            nom::bits::complete::take(2_usize),
            nom::bits::complete::take(5_usize),
        )))(i)?;

    let local_mesg_num = res.1 as u16;
    let offset_secs = res.2 as u8;

    nom::IResult::Ok((
        o,
        FitRecordHeader::CompressedTimestamp(FitCompressedTimestampHeader {
            local_mesg_num,
            offset_secs,
        }),
    ))
}

pub fn parse_field_definition<'a>(input: &'a [u8]) -> nom::IResult<&'a [u8], FitFieldDefinition> {
    let base_parser = |inp: &'a [u8]| -> nom::IResult<&'a [u8], (u8, u8, u8)> {
        let (i, definition_number) = nom::bytes::complete::take(1_usize)(inp)?;
        let (i, field_size) = nom::bytes::complete::take(1_usize)(i)?;
        let (i, base_type) = nom::bytes::complete::take(1_usize)(i)?;

        nom::IResult::Ok((i, (definition_number[0], field_size[0], base_type[0])))
    };

    let mut fd_parser = nom::combinator::map_res(base_parser, |field_components: (u8, u8, u8)| {
        FitFieldDefinition::new(
            field_components.0,
            field_components.1.into(),
            field_components.2,
        )
    });

    let (o, fd) = fd_parser(input)?;
    nom::IResult::Ok((o, fd))
}

pub fn parse_definition_message(
    i: &[u8],
    header: FitNormalRecordHeader,
) -> Result<(&[u8], FitDefinitionMessage)> {
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
            Endianness::Little | Endianness::Native => nom::number::complete::le_u8(i)?,
            Endianness::Big => nom::number::complete::be_u8(i)?
        };

        let (i, field_definitions) = nom::multi::count(parse_field_definition, num_fields.into())(i)?;
        let (i, num_developer_fields) = match header.developer_fields_present {
            true => {
                match endianness {
                    Endianness::Little | Endianness::Native => nom::number::complete::le_u8(i)?,
                    Endianness::Big => nom::number::complete::be_u8(i)?
                }
            },
            false => (i, 0)
        };
        let (i, developer_field_definitions) = nom::multi::count(parse_developer_field_definition, num_developer_fields.into())(i)?;

        nom::IResult::Ok((i, (endianness, global_message_number, num_fields.into(), field_definitions, num_developer_fields.into(), developer_field_definitions)))
    };

    let (
        i,
        (
            endianness,
            global_message_number,
            num_fields,
            field_definitions,
            num_developer_fields,
            developer_field_definitions,
        ),
    ) = parser()?;

    let message_size = field_definitions
        .iter()
        .fold(0, |sum, val| sum + (val.base_type_size() as usize))
        + developer_field_definitions
            .iter()
            .fold(0, |sum, val| sum + val.field_size);

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
            developer_field_descriptions: HashMap::new(),
            developer_data_ids: HashMap::new(),
        },
    ))
}

pub fn parse_developer_field_definition(
    i: &[u8],
) -> nom::IResult<&[u8], FitDeveloperFieldDefinition> {
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
            developer_data_index,
        },
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
                nom::combinator::map(nom::bytes::complete::take(2_usize), |o: &[u8]| o.to_vec())(i)?
            }
            false => (i, Vec::new()),
        };

        nom::IResult::Ok((
            i,
            (
                header_size,
                protocol_version,
                profile_version,
                data_size,
                crc,
            ),
        ))
    };

    let (i, (header_size, protocol_version, profile_version, data_size, crc)) = parser()?;

    Ok((
        i,
        FitFileHeader {
            header_size,
            protocol_version,
            profile_version,
            data_size,
            crc: crc.into(),
        },
    ))
}
