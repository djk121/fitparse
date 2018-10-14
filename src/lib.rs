
#![feature(trace_macros)]

use std::ops::Deref;
use std::rc::Rc;
use std::collections::HashMap;

extern crate bit_vec;
use bit_vec::BitVec;

extern crate byteorder;
use byteorder::{ByteOrder, BigEndian};

extern crate chrono;
use chrono::{DateTime, UTC, FixedOffset, TimeZone};

#[macro_use]
extern crate failure;

#[macro_use]
extern crate nom;
use nom::Endianness;

pub use errors::{Error, ErrorKind, Result};

use fittypes::{FitFieldFitBaseType, FitFieldMesgNum, FitDataMessage, FitMessageDeviceSettings, FitMessageDeveloperDataId, FitMessageFieldDescription};
use fitparsingstate::FitParsingState;
use fitparsers::{parse_uint8, parse_uint8z, parse_sint8, parse_sint16, parse_uint16, parse_uint16z, parse_uint32, parse_uint32z, parse_sint32, parse_byte, parse_string, parse_float32, parse_uint64, parse_sint64, parse_uint64z,
    parse_float64};

#[derive(Debug,PartialEq)]
enum FitNormalRecordHeaderMessageType {
    Data,
    Definition,
}

impl From<u8> for FitNormalRecordHeaderMessageType {
    fn from(mtype: u8) -> FitNormalRecordHeaderMessageType {
        match mtype {
            0 => FitNormalRecordHeaderMessageType::Data,
            1 => FitNormalRecordHeaderMessageType::Definition,
            _ => panic!("boo")
        }
    }
}

#[derive(Debug,PartialEq)]
pub struct FitNormalRecordHeader {
    message_type: FitNormalRecordHeaderMessageType,
    developer_fields_present: bool,
    local_mesg_num: u16,
}

named!(normal_record_header<&[u8], FitRecordHeader>,
    do_parse!(
        first_byte:
            bits!(
                tuple!(
                    tag_bits!(u8, 1, 0x0), // should be 0 for Normal
                    take_bits!(u8, 1), // data or definition flag
                    take_bits!(u8, 1), // developer data flag
                    tag_bits!(u8, 0, 0x0), // reserved
                    take_bits!(u8, 4) // local message type
            )) >>
            (FitRecordHeader::Normal(FitNormalRecordHeader {
                message_type: FitNormalRecordHeaderMessageType::from(first_byte.1),
                developer_fields_present: first_byte.2 == 1,
                local_mesg_num: first_byte.4 as u16,
            }))
    )
);

// these are supposed to come after some message indicating the base time
// to_which time_offset should be added, so we'll require extra context from
// the parser to figure out the absolute timestmap
#[derive(Debug)]
pub struct FitCompressedTimestampHeader {
    local_mesg_num: u16,
    offset_secs: u8,
}

named!(compressed_timestamp_record_header<&[u8], FitRecordHeader>,
    do_parse!(
        first_byte: bits!(
                        tuple!(
                            tag_bits!(u8, 1, 0x1), // should be 1 for Compressed
                            take_bits!(u8, 2), // local_mesg_num
                            take_bits!(u8, 4) // offset in seconds
                    )) >>
        (FitRecordHeader::CompressedTimestamp(FitCompressedTimestampHeader {
            local_mesg_num: first_byte.1.into(),
            offset_secs: first_byte.2,
        }))
    )
);

#[derive(Debug,PartialEq)]
pub struct FitFileHeader {
        header_size: u8,
        protocol_version: u8,
        profile_version: u16,
        data_size: u32,
        crc: Vec<u8>,
}

impl FitFileHeader {
    pub fn parse(input: &[u8]) -> Result<(FitFileHeader, &[u8])> {
        parse_fit_file_header(input)
    }
}

fn parse_fit_file_header(input: &[u8]) -> Result<(FitFileHeader, &[u8])> {
    nom_internal_parser!(parse_fit_file_header_internal, input)
}

named!(parse_fit_file_header_internal<&[u8], FitFileHeader>,
    do_parse!(
        header_size: take!(1) >>
        protocol_version: take!(1) >>
        profile_version: u16!(Endianness::Little) >>
        data_size: u32!(Endianness::Little) >>
        tag!(".FIT") >>
        crc: take!(2) >>
        (FitFileHeader {
            header_size: header_size[0],
            protocol_version: protocol_version[0],
            profile_version: profile_version,
            data_size: data_size,
            crc: crc.into(),
        })
    )
);

#[derive(Debug)]
pub enum FitMessage<'a> {
    Data(FitDataMessage<'a>),
    Definition(Rc<FitDefinitionMessage>),
}


pub fn parse_fit_message<'a>(input: &'a [u8], parsing_state: &mut FitParsingState<'a>) -> Result<(FitMessage<'a>, &'a [u8])> {
    // get the header first
    let (header, o) = parse_record_header(input)?;

    let (fit_message, out) = match header {
        FitRecordHeader::Normal(normal_header) => {
            match normal_header.message_type {
                FitNormalRecordHeaderMessageType::Data => {
                    let (data_message, o) = FitDataMessage::parse(o, FitRecordHeader::Normal(normal_header), parsing_state, None)?;
                    (FitMessage::Data(data_message), o)
                },
                FitNormalRecordHeaderMessageType::Definition => {
                    // let local_mesg_num = normal_header.local_mesg_num;
                    let (definition_message, o) = FitDefinitionMessage::parse(o, normal_header)?;
                    parsing_state.add(definition_message.header.local_mesg_num, definition_message.clone());
                    (FitMessage::Definition(definition_message.clone()), o)
                }
            }
        },
        _ => return Err(Error::unknown_error())
    };

    match &fit_message {
        FitMessage::Data(m) => {
            match m {
                FitDataMessage::DeviceSettings(ds) => {
                    match *ds.deref() {
                        FitMessageDeviceSettings{time_zone_offset, ..} => {
                            // device_settings.time_zone_offset is 'in quarter hour increments',
                            // so a value of +15 = (15 increments * 15 minutes * 60 seconds) =
                            // = +13500 seconds
                            if let Some(tzo) = time_zone_offset {
                                parsing_state.set_timezone_offset((tzo * 15.0 * 60.0).into());
                            }
                        }
                    }
                },
                FitDataMessage::FieldDescription(fd) => {
                    if let Some(ddi) = fd.developer_data_index {
                        parsing_state.set_developer_data_definition(ddi, FitDataMessage::FieldDescription(fd.clone()));
                    }
                }
                _ => {},
            }
        },
        _ => {},
    }

    Ok((fit_message, out))
}

#[derive(Debug,PartialEq)]
struct FitFieldDefinition {
    definition_number: u8,
    field_size: usize,
    base_type: u8,
}

impl FitFieldDefinition {
    pub fn field_size(&self) -> usize {
        match self.base_type {
            0 => 1, // enum
            1 => 1, // sint8
            2 => 1, // uint8
            3 => 2, // sint16
            4 => 2, // uint16
            5 => 4, // sint32
            6 => 4, // uint32
            7 => 1 * self.field_size, // String
            8 => 4, // float32
            9 => 8, // float64
            10 => 1, // uint8z
            11 => 2, // uint16z
            12 => 4, // uint32z
            13 => 1 * self.field_size, // byte
            14 => 8, // sint64
            15 => 8, // uint64
            16 => 8, // uint64z
            _ => panic!("unexpected FitField base_type")
        }
    }
}

#[derive(Debug,PartialEq)]
struct FitDeveloperFieldDefinition {
    definition_number: u8,
    field_size: usize,
    developer_data_index: u8,
}



// Definition message has two purposes:
//   1. list which fields from the global profile are present
//   2. list developer data fields which are present
//
// The definition message will be a companion to the parser for
// the actual message type.

#[derive(Debug)]
pub struct FitDefinitionMessage {
    header: FitNormalRecordHeader,
    endianness: nom::Endianness,
    global_mesg_num: FitFieldMesgNum,
    num_fields: u8,
    message_size: usize,
    field_definitions: Vec<FitFieldDefinition>,
    num_developer_fields: usize,
    developer_field_definitions: Vec<FitDeveloperFieldDefinition>
}

impl FitDefinitionMessage {
    fn parse(input: &[u8], header: FitNormalRecordHeader) -> Result<(Rc<FitDefinitionMessage>, &[u8])> {
        let (definition_message, o) = parse_definition_message(input, header)?;
        Ok((Rc::new(definition_message), o))
    }
}

#[derive(Debug)]
pub enum FitRecordHeader {
    Normal(FitNormalRecordHeader),
    CompressedTimestamp(FitCompressedTimestampHeader),
}

impl FitRecordHeader {
    fn parse(input: &[u8]) -> Result<(FitRecordHeader, &[u8])> {
        Ok(parse_record_header(input)?)
    }

    fn local_mesg_num(&self) -> u16 {
        match self {
            FitRecordHeader::Normal(nrh) => nrh.local_mesg_num,
            FitRecordHeader::CompressedTimestamp(ctrh) => ctrh.local_mesg_num
        }
    }
}

fn parse_record_header(input: &[u8]) -> Result<(FitRecordHeader, &[u8])> {
    nom_internal_parser!(parse_record_header_internal, input)
}

named!(parse_record_header_internal<&[u8], FitRecordHeader>,
    do_parse!(
        header: alt!(
                    compressed_timestamp_record_header |
                    normal_record_header) >>
        (header)
    )
);

named!(field_definition<&[u8], FitFieldDefinition>,
    do_parse!(
        definition_number: take!(1) >>
        field_size: take!(1) >>
        base_type_byte: bits!(
                            tuple!(
                                take_bits!(u8, 1), // endianess ability
                                tag_bits!(u8, 2, 0x0), // reserved
                                take_bits!(u8, 5) // base type number
                            )) >>
        (FitFieldDefinition{
            definition_number: definition_number[0],
            field_size: field_size[0] as usize,
            base_type: base_type_byte.2,
        })
    )
);

named!(developer_field_definition<&[u8], FitDeveloperFieldDefinition>,
    do_parse!(
        field_number: take!(1) >>
        field_size: take!(1) >>
        developer_data_index: take!(1) >>
        (FitDeveloperFieldDefinition{
            definition_number: field_number[0],
            field_size: field_size[0] as usize,
            developer_data_index: developer_data_index[0]
        })
    )
);

fn parse_definition_message(input: &[u8], header: FitNormalRecordHeader) -> Result<(FitDefinitionMessage, &[u8])> {
    nom_internal_parser!(parse_definition_message_internal, input, header)
}

//trace_macros!(true);

named_args!(parse_definition_message_internal(header: FitNormalRecordHeader)<FitDefinitionMessage>,
    do_parse!(
        tag!([0u8]) >>
        endianness_raw: take!(1) >>
        endianness: value!(match endianness_raw[0] {
            0 => nom::Endianness::Little,
            _ => nom::Endianness::Big,
        }) >>
        global_message_number: take!(2) >>
        num_fields: take!(1) >>
        field_definitions: many_m_n!(num_fields[0] as usize, num_fields[0] as usize, field_definition) >>
        num_developer_fields: switch!(call!(developer_fields_are_present, header.developer_fields_present),
            true => map!(take!(1), |x| x[0] as usize) |
            _ => value!(0)) >>
        developer_field_definitions: many_m_n!(num_developer_fields as usize, num_developer_fields as usize, developer_field_definition) >>


        (FitDefinitionMessage{
            header: header,
            endianness: endianness,
            global_mesg_num: FitFieldMesgNum::parse(global_message_number, endianness).unwrap().0,
            num_fields: num_fields[0],
            message_size: (field_definitions.iter().fold(
                0, |sum, val| sum + (val.field_size() as usize)) +
                developer_field_definitions.iter().fold(0, |sum, val| sum + val.field_size)),
            field_definitions: field_definitions,
            num_developer_fields: num_developer_fields,
            developer_field_definitions: developer_field_definitions,


        })
    )
);

named_args!(developer_fields_are_present(yesorno: bool)<bool>,
    do_parse!(
        (yesorno)
    )
);

#[derive(Debug)]
pub struct FitDeveloperDataDefinition<'a> {
    developer_data_id: Option<Rc<FitMessageDeveloperDataId<'a>>>,
    field_descriptions: HashMap<u8, Rc<FitMessageFieldDescription<'a>>>
}

impl<'a> FitDeveloperDataDefinition<'a> {
    fn new() -> FitDeveloperDataDefinition<'a> {
        FitDeveloperDataDefinition{
            developer_data_id: None,
            field_descriptions: HashMap::new()
        }
    }

    fn add(&mut self, message: FitDataMessage<'a>) -> &Self {
        //let m = &*message;

        match message {
            FitDataMessage::FieldDescription(fd) => {
                if let Some(fdn) = fd.field_definition_number {
                    self.field_descriptions.insert(fdn, fd.clone());
                }
                self
            },
            FitDataMessage::DeveloperDataId(ddi) => {
                self.developer_data_id = Some(ddi.clone());
                self
            },
            _ => self,
        }
    }

    fn get_field_description(&self, field_number: u8) -> Result<Rc<FitMessageFieldDescription<'a>>> {
        match self.field_descriptions.get(&field_number) {
            Some(fd) => Ok(fd.clone()),
            None => Err(Error::developer_field_description_not_found(field_number))
        }
    }
}

#[derive(Debug)]
enum FitBaseValue<'a> {
    Sint8(i8),
    Uint8(u8),
    Uint8z(Option<u8>),
    Sint16(i16),
    Uint16(u16),
    Uint16z(Option<u16>),
    Sint32(i32),
    Uint32(u32),
    Uint32z(Option<u32>),
    Float32(f32),
    Sint64(i64),
    Uint64(u64),
    Uint64z(Option<u64>),
    Float64(f64),
    String(String),
    Byte(&'a [u8])
}

impl<'a> FitBaseValue<'a> {
    fn parse(input: &'a [u8], variant: &FitFieldFitBaseType, endianness: Endianness, size: usize) -> Result<(FitBaseValue<'a>, &'a [u8])> {
        match variant {
            FitFieldFitBaseType::Sint8 => {
                let (val, o) = parse_sint8(input)?;
                Ok((FitBaseValue::Sint8(val), o))
            },
            FitFieldFitBaseType::Uint8 => {
                let (val, o) = parse_uint8(input)?;
                Ok((FitBaseValue::Uint8(val), o))
            },
            FitFieldFitBaseType::Sint16 => {
                let (val, o) = parse_sint16(input, endianness)?;
                Ok((FitBaseValue::Sint16(val), o))
            },
            FitFieldFitBaseType::Uint16 => {
                let (val, o) = parse_uint16(input, endianness)?;
                Ok((FitBaseValue::Uint16(val), o))
            },
            FitFieldFitBaseType::Sint32 => {
                let (val, o) = parse_sint32(input, endianness)?;
                Ok((FitBaseValue::Sint32(val), o))
            },
            FitFieldFitBaseType::Uint32 => {
                let (val, o) = parse_uint32(input, endianness)?;
                Ok((FitBaseValue::Uint32(val), o))
            },
            FitFieldFitBaseType::String => {
                let (val, o) = parse_string(input, size)?;
                Ok((FitBaseValue::String(val), o))
            },
            FitFieldFitBaseType::Float32 => {
                let (val, o) = parse_float32(input, endianness)?;
                Ok((FitBaseValue::Float32(val), o))
            },
            FitFieldFitBaseType::Float64 => {
                let (val, o) = parse_float64(input, endianness)?;
                Ok((FitBaseValue::Float64(val), o))
            },
            FitFieldFitBaseType::Uint8z => {
                let (val, o) = parse_uint8z(input)?;
                Ok((FitBaseValue::Uint8z(val), o))
            },
            FitFieldFitBaseType::Uint16z => {
                let (val, o) = parse_uint16z(input, endianness)?;
                Ok((FitBaseValue::Uint16z(val), o))
            },
            FitFieldFitBaseType::Uint32z => {
                let (val, o) = parse_uint32z(input, endianness)?;
                Ok((FitBaseValue::Uint32z(val), o))
            },
            FitFieldFitBaseType::Byte => {
                let (val, o) = parse_byte(input, size)?;
                Ok((FitBaseValue::Byte(val), o))
            },
            FitFieldFitBaseType::Sint64 => {
                let (val, o) = parse_sint64(input, endianness)?;
                Ok((FitBaseValue::Sint64(val), o))
            },
            FitFieldFitBaseType::Uint64 => {
                let (val, o) = parse_uint64(input, endianness)?;
                Ok((FitBaseValue::Uint64(val), o))
            },
            FitFieldFitBaseType::Uint64z => {
                let (val, o) = parse_uint64z(input, endianness)?;
                Ok((FitBaseValue::Uint64z(val), o))
            },
            _ => Err(Error::parse_unknown_base_value())
        }
    }
}

#[derive(Debug)]
struct FitFieldDeveloperData<'a> {
    field_description: Rc<FitMessageFieldDescription<'a>>,
    value: FitBaseValue<'a>,
}

impl<'a> FitFieldDeveloperData<'a> {
    fn parse(input: &'a [u8], field_description: Rc<FitMessageFieldDescription<'a>>, endianness: Endianness, field_size: usize) -> Result<(FitFieldDeveloperData<'a>, &'a [u8])> {
        let base_type_id = match &field_description.fit_base_type_id {
            Some(bti) => bti,
            None => return Err(Error::missing_fit_base_type())
        };

        let (val, o) = FitBaseValue::parse(input, base_type_id, endianness, field_size)?;
        Ok((FitFieldDeveloperData{
            field_description: field_description.clone(),
            value: val}, o))
    }
}

//trace_macros!(false);

fn shift_out_u8(inp: &[u8], num_bits: usize) -> Result<(Vec<u8>, u8)> {
    if (num_bits > 8) {
        return Err(Error::incorrect_shift_input());
    }

    let (left, right) = shift_right(inp, num_bits)?;
    Ok((left, right[3] as u8))
}

fn shift_out_u16(inp: &[u8], num_bits: usize) -> Result<(Vec<u8>, u16)> {
    if (num_bits > 16) {
        return Err(Error::incorrect_shift_input());
    }

    let (left, right) = shift_right(inp, num_bits)?;
    Ok((left, BigEndian::read_u16(&right[2..4])))
}

fn shift_right(inp: &[u8], num_bits: usize) -> Result<(Vec<u8>, Vec<u8>)> {
    if (inp.len() > 4 || num_bits > 32) {
        return Err(Error::incorrect_shift_input());
    }

    let input = BitVec::from_bytes(inp);

    let mut left = BitVec::from_elem(32, false);
    let mut right = BitVec::from_elem(32, false);

    let mut to_copy = num_bits;
    let mut input_ind = input.len() - 1;
    let mut right_ind = right.len() - 1;

    // copy num_bits to the result BV
    while to_copy >= 0 {
        right.set(right_ind, input[input_ind]);
        input_ind = input_ind - 1;
        right_ind = right_ind - 1;
        to_copy = to_copy - 1;
    }

    for i in 0..(input.len()-num_bits) {
        left.set(i+num_bits, input[i]);
    }

    return Ok((left.to_bytes(), right.to_bytes()))
}

#[cfg(test)]
mod tests {
    use *;

    #[test]
    fn normal_record_header_test() {
        let record_header_data = [0b01000000,];
        let expected = FitNormalRecordHeader{
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        };

        let res = normal_record_header(&record_header_data);
        println!("{:?}", res);

        assert_eq!(1,1);
    }

    #[test]
    fn definition_message_test() {
        let defintion_message_data = [0b00000000, // reserved, 1 byte
                                      0b00000000, // architecture, 1 byte
                                      0b00000000, 0b00000000, // global mseg num, 2 bytes
                                      0b00000100, // num_fields, 1 bytes, 4 here
                                      0b00000011, 0b00000100, 0b10001100, // 1st field def
                                      0b00000100, 0b00000100, 0b10000110, // 2nd field def
                                      0b00000001, 0b00000010, 0b10000100, // 3rd field def
                                      0b00000000, 0b00000001, 0b00000000, // 4th field def
                                      ];

        let res = definition_message(&defintion_message_data);
        println!("def: {:?}", res);
        assert_eq!(1,1);
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

mod fittypes;
#[macro_use] mod fitparsers;
pub mod fitparsingstate;
mod errors;
