use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use std::fmt;

extern crate bitvec;
use bitvec::prelude as bv;

extern crate chrono;
extern crate failure;

#[macro_use]
extern crate nom;
use nom::Endianness;

pub use errors::{Error, ErrorKind, Result};

use fitparsers::{
    parse_byte, parse_enum, parse_float32, parse_float64, parse_sint16, parse_sint32, parse_sint64,
    parse_sint8, parse_string, parse_uint16, parse_uint16z, parse_uint32, parse_uint32z,
    parse_uint64, parse_uint64z, parse_uint8, parse_uint8z,
};
use fitparsingstate::FitParsingState;
use fittypes::{
    FitDataMessage, FitFieldFitBaseType, FitFieldMesgNum, FitMessageDeveloperDataId,
    FitMessageDeviceSettings, FitMessageFieldDescription,
};

use fittypes_utils::FitFieldDateTime;

#[derive(Debug, PartialEq)]
enum FitNormalRecordHeaderMessageType {
    Data,
    Definition,
}

impl From<u8> for FitNormalRecordHeaderMessageType {
    fn from(mtype: u8) -> FitNormalRecordHeaderMessageType {
        match mtype {
            0 => FitNormalRecordHeaderMessageType::Data,
            1 => FitNormalRecordHeaderMessageType::Definition,
            _ => panic!("boo"),
        }
    }
}

#[derive(Debug, PartialEq)]
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
                    tag_bits!(u8, 1, 0x0), // reserved
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
#[derive(Debug, PartialEq)]
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
                            take_bits!(u8, 5) // offset in seconds
                    )) >>
        (FitRecordHeader::CompressedTimestamp(FitCompressedTimestampHeader {
            local_mesg_num: first_byte.1.into(),
            offset_secs: first_byte.2,
        }))
    )
);

#[derive(Debug, PartialEq)]
pub struct FitFileHeader {
    header_size: u8,
    protocol_version: u8,
    profile_version: u16,
    data_size: u32,
    crc: Vec<u8>,
}

impl FitFileHeader {
    pub fn parse(input: &[u8]) -> Result<(FitFileHeader, &[u8])> {
        match parse_fit_file_header(input)? {
            (Some(ffh), o) => Ok((ffh, o)),
            _ => Err(Error::parse_error("error parsing FitFileHeader")),
        }
    }
}

fn parse_fit_file_header(input: &[u8]) -> Result<(Option<FitFileHeader>, &[u8])> {
    nom_returning_internal_parser!(parse_fit_file_header_internal, input)
}

named!(parse_fit_file_header_internal<&[u8], FitFileHeader>,
    do_parse!(
        header_size: take!(1) >>
        protocol_version: take!(1) >>
        profile_version: u16!(Endianness::Little) >>
        data_size: u32!(Endianness::Little) >>
        tag!(".FIT") >>
        crc: switch!(value!(header_size[0] == 14),
            true => map!(take!(2), |x| x.to_vec()) |
            _ => value!(Vec::new())) >>
        //crc: take!(2) >>
        (FitFileHeader {
            header_size: header_size[0],
            protocol_version: protocol_version[0],
            profile_version: profile_version,
            data_size: data_size,
            crc: crc.into(),
        })
    )
);

trait FitRecord {
    fn message_name(&self) -> &'static str;
}

#[derive(Debug)]
pub enum FitMessage {
    Data(FitDataMessage),
    Definition(Rc<FitDefinitionMessage>),
}

impl fmt::Display for FitMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &FitMessage::Data(ref m) => writeln!(f, "{}", m),
            &FitMessage::Definition(ref m) => {
                writeln!(f, "Definition")?;
                writeln!(f, "{}", m)
            }
        }
    }
}

impl FitMessage {
    pub fn message_name(&self) -> &'static str {
        match self {
            &FitMessage::Data(ref m) => m.message_name(),
            &FitMessage::Definition(_) => "Definition",
        }
    }
}

pub fn parse_fit_message<'a>(
    input: &'a [u8],
    parsing_state: &mut FitParsingState,
) -> Result<(Option<FitMessage>, &'a [u8])> {
    let (header, o) = match parse_record_header(input) {
        Ok((Some(header), o)) => (header, o),
        Err(e) => {
            println!("ERROR");
            return Err(e);
        }
        _ => return Err(Error::parse_error("error parsing header")),
    };

    //println!("header: {:?}", header);

    let (fit_message, out) = match header {
        FitRecordHeader::Normal(normal_header) => match normal_header.message_type {
            FitNormalRecordHeaderMessageType::Data => {
                let (data_message, o) = FitDataMessage::parse(
                    o,
                    FitRecordHeader::Normal(normal_header),
                    parsing_state,
                    None,
                )?;
                match data_message {
                    None => return Ok((None, o)),
                    Some(dm) => (FitMessage::Data(dm), o),
                }
            }
            FitNormalRecordHeaderMessageType::Definition => {
                let (definition_message, o) = FitDefinitionMessage::parse(o, normal_header)?;
                parsing_state.add(
                    definition_message.header.local_mesg_num,
                    definition_message.clone(),
                );
                (FitMessage::Definition(definition_message.clone()), o)
            }
        },
        FitRecordHeader::CompressedTimestamp(compressed_timestamp_header) => {
            // must be a data message

            // 1. Get the last full timestamp from parsing_state
            let last_full_timestamp = parsing_state.get_last_timestamp()?;
            let new_from_offset = last_full_timestamp
                .new_from_compressed_timestamp(compressed_timestamp_header.offset_secs)?;
            let (data_message, o) = FitDataMessage::parse(
                o,
                FitRecordHeader::CompressedTimestamp(compressed_timestamp_header),
                parsing_state,
                Some(new_from_offset),
            )?;
            match data_message {
                None => return Ok((None, o)),
                Some(dm) => (FitMessage::Data(dm), o),
            }
        }
    };

    match &fit_message {
        FitMessage::Data(m) => {
            match m {
                FitDataMessage::DeviceSettings(ds) => {
                    match *ds.deref() {
                        FitMessageDeviceSettings {
                            ref time_zone_offset,
                            ..
                        } => {
                            // device_settings.time_zone_offset is 'in quarter hour increments',
                            // so a value of +15 = (15 increments * 15 minutes * 60 seconds) =
                            // = +13500 seconds
                            if let Some(tzo) = &time_zone_offset.value {
                                match tzo[0] {
                                    Some(first_offset) => parsing_state
                                        .set_timezone_offset((first_offset * 15.0 * 60.0).into()),
                                    _ => (),
                                }
                            }
                        }
                    }
                }
                FitDataMessage::FieldDescription(fd) => {
                    if let Some(ddi) = fd.developer_data_index.value {
                        parsing_state.set_developer_data_definition(
                            ddi,
                            FitDataMessage::FieldDescription(fd.clone()),
                        );
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
    //println!("fit_message: {}", fit_message);

    Ok((Some(fit_message), out))
}

#[derive(Debug, PartialEq)]
pub struct FitFieldValue<T> {
    pub value: Option<T>,
    pub units: String,
}

#[derive(Debug)]
pub struct FitMessageUnknownToSdk {
    number: u16,
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData>,
    unknown_fields: HashMap<u8, FitBaseValue>,
    pub raw_bytes: Vec<u8>,
    pub message_name: String,
}

impl fmt::Display for FitMessageUnknownToSdk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Unknown_{}", self.number)?;
        fmt_developer_fields!(self, f);
        fmt_unknown_fields!(self, f);
        Ok(())
    }
}

impl FitMessageUnknownToSdk {
    pub fn parse<'a>(
        number: u16,
        input: &'a [u8],
        header: FitRecordHeader,
        parsing_state: &mut FitParsingState,
        _timestamp: Option<FitFieldDateTime>,
    ) -> Result<(Rc<FitMessageUnknownToSdk>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageUnknownToSdk {
            number: number,
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            unknown_fields: HashMap::new(),
            raw_bytes: Vec::with_capacity(definition_message.message_size),
            message_name: "FitMessageUnknownToSdk".to_string(),
        };

        let inp = &input[..(message.definition_message.message_size)];
        message
            .raw_bytes
            .resize(message.definition_message.message_size, 0);
        message.raw_bytes.copy_from_slice(inp);

        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageUnknownToSdk::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageUnknownToSdk:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string));
            }
        };

        let mut inp2 = o;
        for dev_field in &message.definition_message.developer_field_definitions {
            let dev_data_definition =
                parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;
            let field_description =
                dev_data_definition.get_field_description(dev_field.definition_number)?;
            let (dd, outp) = FitFieldDeveloperData::parse(
                inp2,
                field_description.clone(),
                message.definition_message.endianness,
                dev_field.field_size,
            )?;
            message.developer_fields.push(dd);
            inp2 = outp;
        }

        Ok((Rc::new(message), inp2))
    }

    fn parse_internal<'a>(
        message: &mut FitMessageUnknownToSdk,
        input: &'a [u8],
        _tz_offset: f64,
    ) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let base_type = FitFieldFitBaseType::from(field.base_type);
            let val = FitBaseValue::parse(
                inp,
                &base_type,
                message.definition_message.endianness,
                field.field_size,
            )?;
            inp = &inp[field.field_size..];
            message.unknown_fields.insert(field.definition_number, val);
        }
        Ok(inp)
    }
}

#[derive(Debug, PartialEq)]
pub enum FitGlobalMesgNum {
    Known(FitFieldMesgNum),
    Unknown(u16),
}

impl fmt::Display for FitGlobalMesgNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &FitGlobalMesgNum::Known(ref mesg_num) => write!(f, "{:?}", mesg_num),
            &FitGlobalMesgNum::Unknown(num) => write!(f, "Unknown Mesg Num ({})", num),
        }
    }
}

impl FitGlobalMesgNum {
    fn parse(input: &[u8], endianness: Endianness) -> Result<(FitGlobalMesgNum, &[u8])> {
        let (raw_num, o) = match parse_uint16(input, endianness)? {
            Some(raw_num) => (raw_num, &input[2..]),
            _ => return Err(Error::parse_error("error parsing FitGlobalMesgNum")),
        };

        match FitFieldMesgNum::from(raw_num) {
            FitFieldMesgNum::UnknownToSdk => Ok((FitGlobalMesgNum::Unknown(raw_num), o)),
            known => Ok((FitGlobalMesgNum::Known(known), o)),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct FitFieldDefinition {
    definition_number: u8,
    field_size: usize,
    base_type: u8,
}

impl FitFieldDefinition {
    pub fn field_name(&self, mesg_num: &FitGlobalMesgNum) -> &'static str {
        FitDataMessage::field_name(mesg_num, self.definition_number)
    }

    pub fn base_type_name(&self) -> &'static str {
        match self.base_type {
            0 => "enum",
            1 => "sint8",
            2 => "uint8",
            131 => "sint16",
            132 => "uint16",
            133 => "sint32",
            134 => "uint32",
            7 => "string",
            136 => "float32",
            137 => "float64",
            10 => "uint8z",
            139 => "uint16z",
            140 => "uint32z",
            13 => "byte",
            142 => "sint64",
            143 => "uint64",
            144 => "uint64z",
            _ => "unknown",
        }
    }

    pub fn base_type_size(&self) -> usize {
        match self.base_type {
            0 => 1,                    // enum
            1 => 1,                    // sint8
            2 => 1,                    // uint8
            131 => 2,                  // sint16
            132 => 2,                  // uint16
            133 => 4,                  // sint32
            134 => 4,                  // uint32
            7 => 1 * self.field_size,  // String
            136 => 4,                  // float32
            137 => 8,                  // float64
            10 => 1,                   // uint8z
            139 => 2,                  // uint16z
            140 => 4,                  // uint32z
            13 => 1 * self.field_size, // byte
            142 => 8,                  // sint64
            143 => 8,                  // uint64
            144 => 8,                  // uint64z
            _ => panic!("unexpected FitField base_type"),
        }
    }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct FitDefinitionMessage {
    header: FitNormalRecordHeader,
    endianness: nom::Endianness,
    //global_mesg_num: FitFieldMesgNum,
    global_mesg_num: FitGlobalMesgNum,
    num_fields: u8,
    message_size: usize,
    field_definitions: Vec<FitFieldDefinition>,
    num_developer_fields: usize,
    developer_field_definitions: Vec<FitDeveloperFieldDefinition>,
}

impl fmt::Display for FitDefinitionMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "  {: >28}: {:?}",
            "global_mesg_num", self.global_mesg_num
        )?;
        writeln!(f, "  {: >28}: {:?}", "endianness", self.endianness)?;
        writeln!(f, "  {: >28}: {}", "num_fields", self.num_fields)?;
        writeln!(f, "  {: >28}: {}", "message_size", self.message_size)?;
        writeln!(f, "  {: >28}: ", "field_definitions")?;
        for field_definition in &self.field_definitions {
            writeln!(
                f,
                "  {: >30}name: {} ({}), field_size: {}, base_type: {}",
                " ",
                field_definition.field_name(&self.global_mesg_num),
                field_definition.definition_number,
                field_definition.field_size,
                field_definition.base_type_name()
            )?;
            //writeln!(f, "  {: >30}{:?}", " ", field_definition)?;
        }
        writeln!(
            f,
            "  {: >28}: {}",
            "num_developer_fields", self.num_developer_fields
        )?;
        writeln!(f, "  {: >28}:", "developer_field_definitions")?;
        for developer_field_definition in &self.developer_field_definitions {
            writeln!(f, "  {: >30}{:?}", " ", developer_field_definition)?;
        }
        Ok(())
    }
}

impl FitDefinitionMessage {
    fn parse(
        input: &[u8],
        header: FitNormalRecordHeader,
    ) -> Result<(Rc<FitDefinitionMessage>, &[u8])> {
        match parse_definition_message(input, header) {
            Ok((Some(fdm), o)) => Ok((Rc::new(fdm), o)),
            Err(e) => Err(e),
            _ => Err(Error::parse_error("error parsing definition message")),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FitRecordHeader {
    Normal(FitNormalRecordHeader),
    CompressedTimestamp(FitCompressedTimestampHeader),
}

impl FitRecordHeader {
    fn local_mesg_num(&self) -> u16 {
        match self {
            FitRecordHeader::Normal(nrh) => nrh.local_mesg_num,
            FitRecordHeader::CompressedTimestamp(ctrh) => ctrh.local_mesg_num,
        }
    }
}

fn parse_record_header(input: &[u8]) -> Result<(Option<FitRecordHeader>, &[u8])> {
    nom_returning_internal_parser!(parse_record_header_internal, input)
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
        base_type_byte: take!(1) >>
        (FitFieldDefinition{
            definition_number: definition_number[0],
            field_size: field_size[0] as usize,
            base_type: base_type_byte[0]
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

fn parse_definition_message(
    input: &[u8],
    header: FitNormalRecordHeader,
) -> Result<(Option<FitDefinitionMessage>, &[u8])> {
    nom_returning_internal_parser!(parse_definition_message_internal, input, header)
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
            global_mesg_num: FitGlobalMesgNum::parse(global_message_number, endianness).unwrap().0,
            num_fields: num_fields[0],
            message_size: (field_definitions.iter().fold(
                0, |sum, val| sum + (val.base_type_size() as usize)) +
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
pub struct FitDeveloperDataDefinition {
    developer_data_id: Option<Rc<FitMessageDeveloperDataId>>,
    field_descriptions: HashMap<u8, Rc<FitMessageFieldDescription>>,
}

impl FitDeveloperDataDefinition {
    fn new() -> FitDeveloperDataDefinition {
        FitDeveloperDataDefinition {
            developer_data_id: None,
            field_descriptions: HashMap::new(),
        }
    }

    fn add(&mut self, message: FitDataMessage) -> &Self {
        match message {
            FitDataMessage::FieldDescription(fd) => {
                if let Some(fdn) = fd.field_definition_number.value {
                    self.field_descriptions.insert(fdn, fd.clone());
                }
                self
            }
            FitDataMessage::DeveloperDataId(ddi) => {
                self.developer_data_id = Some(ddi.clone());
                self
            }
            _ => self,
        }
    }

    fn get_field_description(&self, field_number: u8) -> Result<Rc<FitMessageFieldDescription>> {
        match self.field_descriptions.get(&field_number) {
            Some(fd) => Ok(fd.clone()),
            None => Err(Error::developer_field_description_not_found(field_number)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum FitBaseValue {
    Enum(Option<u8>),
    EnumVec(Vec<Option<u8>>),
    Sint8(Option<i8>),
    Sint8Vec(Vec<Option<i8>>),
    Uint8(Option<u8>),
    Uint8Vec(Vec<Option<u8>>),
    Uint8z(Option<u8>),
    Uint8zVec(Vec<Option<u8>>),
    Sint16(Option<i16>),
    Sint16Vec(Vec<Option<i16>>),
    Uint16(Option<u16>),
    Uint16Vec(Vec<Option<u16>>),
    Uint16z(Option<u16>),
    Uint16zVec(Vec<Option<u16>>),
    Sint32(Option<i32>),
    Sint32Vec(Vec<Option<i32>>),
    Uint32(Option<u32>),
    Uint32Vec(Vec<Option<u32>>),
    Uint32z(Option<u32>),
    Uint32zVec(Vec<Option<u32>>),
    Float32(Option<f32>),
    Float32Vec(Vec<Option<f32>>),
    Sint64(Option<i64>),
    Sint64Vec(Vec<Option<i64>>),
    Uint64(Option<u64>),
    Uint64Vec(Vec<Option<u64>>),
    Uint64z(Option<u64>),
    Uint64zVec(Vec<Option<u64>>),
    Float64(Option<f64>),
    Float64Vec(Vec<Option<f64>>),
    String(Option<String>),
    Byte(Option<Vec<u8>>),
}

macro_rules! base_type_formatter {
    ($val:ident, $f:ident) => {
        match $val {
            None => write!($f, "None")?,
            Some(ref v) => write!($f, "{}", v)?,
        }
    };
}

macro_rules! base_type_vec_formatter {
    ($vals:ident, $f:ident) => {{
        write!($f, "[")?;
        for i in 0..$vals.len() - 1 {
            match $vals[i] {
                Some(v) => write!($f, "{}, ", v)?,
                None => write!($f, "None, ")?,
            }
        }
        match $vals[$vals.len() - 1] {
            Some(v) => write!($f, "{}] ", v)?,
            None => write!($f, "None], ")?,
        }
    }};
}

impl fmt::Display for FitBaseValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &FitBaseValue::Enum(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::EnumVec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Sint8(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Sint8Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint8(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint8Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint8z(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint8zVec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Sint16(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Sint16Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint16(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint16Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint16z(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint16zVec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Sint32(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Sint32Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint32(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint32Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint32z(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint32zVec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Float32(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Float32Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Sint64(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Sint64Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint64(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint64Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Uint64z(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Uint64zVec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::Float64(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Float64Vec(ref vals) => base_type_vec_formatter!(vals, f),
            &FitBaseValue::String(ref val) => base_type_formatter!(val, f),
            &FitBaseValue::Byte(ref val) => match val {
                Some(bytes) => write!(f, "{:?}", bytes)?,
                None => write!(f, "None")?,
            },
        }
        Ok(())
    }
}

impl FitBaseValue {
    fn parse<'a>(
        input: &'a [u8],
        variant: &FitFieldFitBaseType,
        endianness: Endianness,
        size: usize,
    ) -> Result<FitBaseValue> {
        match variant {
            FitFieldFitBaseType::Enum => {
                if size > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = size;

                    while i > 0 {
                        let val = parse_enum(outp)?;
                        outp = &outp[1..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::EnumVec(v))
                } else {
                    let val = parse_enum(input)?;
                    Ok(FitBaseValue::Enum(val))
                }
            }
            FitFieldFitBaseType::Sint8 => {
                if size > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = size;

                    while i > 0 {
                        let val = parse_sint8(outp)?;
                        outp = &outp[1..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Sint8Vec(v))
                } else {
                    let val = parse_sint8(input)?;
                    Ok(FitBaseValue::Sint8(val))
                }
            }
            FitFieldFitBaseType::Uint8 => {
                if size > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = size;

                    while i > 0 {
                        let val = parse_uint8(outp)?;
                        outp = &outp[1..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint8Vec(v))
                } else {
                    let val = parse_uint8(input)?;
                    Ok(FitBaseValue::Uint8(val))
                }
            }
            FitFieldFitBaseType::Sint16 => {
                let num_to_parse = size / 2;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_sint16(outp, endianness)?;
                        outp = &outp[2..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Sint16Vec(v))
                } else {
                    let val = parse_sint16(input, endianness)?;
                    Ok(FitBaseValue::Sint16(val))
                }
            }
            FitFieldFitBaseType::Uint16 => {
                let num_to_parse = size / 2;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_uint16(outp, endianness)?;
                        outp = &outp[2..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint16Vec(v))
                } else {
                    let val = parse_uint16(input, endianness)?;
                    Ok(FitBaseValue::Uint16(val))
                }
            }
            FitFieldFitBaseType::Sint32 => {
                let num_to_parse = size / 4;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_sint32(outp, endianness)?;
                        outp = &outp[4..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Sint32Vec(v))
                } else {
                    let val = parse_sint32(input, endianness)?;
                    Ok(FitBaseValue::Sint32(val))
                }
            }
            FitFieldFitBaseType::Uint32 => {
                let num_to_parse = size / 4;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_uint32(outp, endianness)?;
                        outp = &outp[4..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint32Vec(v))
                } else {
                    let val = parse_uint32(input, endianness)?;
                    Ok(FitBaseValue::Uint32(val))
                }
            }
            FitFieldFitBaseType::String => {
                let val = parse_string(input, size)?;
                Ok(FitBaseValue::String(val))
            }
            FitFieldFitBaseType::Float32 => {
                let num_to_parse = size / 4;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_float32(outp, endianness)?;
                        outp = &outp[4..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Float32Vec(v))
                } else {
                    let val = parse_float32(input, endianness)?;
                    Ok(FitBaseValue::Float32(val))
                }
            }
            FitFieldFitBaseType::Float64 => {
                let num_to_parse = size / 8;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_float64(outp, endianness)?;
                        outp = &outp[8..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Float64Vec(v))
                } else {
                    let val = parse_float64(input, endianness)?;
                    Ok(FitBaseValue::Float64(val))
                }
            }
            FitFieldFitBaseType::Uint8z => {
                if size > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = size;

                    while i > 0 {
                        let val = parse_uint8z(outp)?;
                        outp = &outp[1..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint8zVec(v))
                } else {
                    let val = parse_uint8z(input)?;
                    Ok(FitBaseValue::Uint8z(val))
                }
            }
            FitFieldFitBaseType::Uint16z => {
                let num_to_parse = size / 2;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_uint16z(outp, endianness)?;
                        outp = &outp[2..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint16zVec(v))
                } else {
                    let val = parse_uint16z(input, endianness)?;
                    Ok(FitBaseValue::Uint16z(val))
                }
            }
            FitFieldFitBaseType::Uint32z => {
                let num_to_parse = size / 4;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_uint32z(outp, endianness)?;
                        outp = &outp[4..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint32zVec(v))
                } else {
                    let val = parse_uint32z(input, endianness)?;
                    Ok(FitBaseValue::Uint32z(val))
                }
            }
            FitFieldFitBaseType::Byte => {
                let val = parse_byte(input, size)?;
                Ok(FitBaseValue::Byte(val))
            }
            FitFieldFitBaseType::Sint64 => {
                let num_to_parse = size / 8;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_sint64(outp, endianness)?;
                        outp = &outp[8..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Sint64Vec(v))
                } else {
                    let val = parse_sint64(input, endianness)?;
                    Ok(FitBaseValue::Sint64(val))
                }
            }
            FitFieldFitBaseType::Uint64 => {
                let num_to_parse = size / 8;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_uint64(outp, endianness)?;
                        outp = &outp[8..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint64Vec(v))
                } else {
                    let val = parse_uint64(input, endianness)?;
                    Ok(FitBaseValue::Uint64(val))
                }
            }
            FitFieldFitBaseType::Uint64z => {
                let num_to_parse = size / 8;

                if num_to_parse > 1 {
                    let mut outp = input;
                    let mut v = vec![];
                    let mut i = num_to_parse;

                    while i > 0 {
                        let val = parse_uint64z(outp, endianness)?;
                        outp = &outp[8..];
                        v.push(val);
                        i = i - 1;
                    }
                    Ok(FitBaseValue::Uint64zVec(v))
                } else {
                    let val = parse_uint64z(input, endianness)?;
                    Ok(FitBaseValue::Uint64z(val))
                }
            }
            _ => Err(Error::parse_unknown_base_value()),
        }
    }
}

#[derive(Debug)]
struct FitFieldDeveloperData {
    field_description: Rc<FitMessageFieldDescription>,
    value: FitBaseValue,
}

impl FitFieldDeveloperData {
    fn parse<'a>(
        input: &'a [u8],
        field_description: Rc<FitMessageFieldDescription>,
        endianness: Endianness,
        field_size: usize,
    ) -> Result<(FitFieldDeveloperData, &'a [u8])> {
        let base_type_id = match &field_description.fit_base_type_id.value {
            Some(bti) => bti,
            None => return Err(Error::missing_fit_base_type()),
        };

        let val = FitBaseValue::parse(input, base_type_id, endianness, field_size)?;
        Ok((
            FitFieldDeveloperData {
                field_description: field_description.clone(),
                value: val,
            },
            &input[field_size..],
        ))
    }
}

fn format_bits(input: &Vec<u8>) -> String {
    let mut s = String::new();
    for item in input {
        s.push_str(&format!("{:08b}, ", item));
    }

    s
}

pub fn bit_subset(inp: &[u8], start: usize, num_bits: usize, big_endian: bool) -> Result<Vec<u8>> {
    //println!("bit_subset");

    // 1. Figure out how many bytes we need from the input, get rid of excess
    let mut desired_byte_length = num_bits / 8;
    //println!("desired_byte_length: {}", desired_byte_length);
    let remainder = num_bits % 8;
    if remainder != 0 {
        desired_byte_length = desired_byte_length + 1;
    }

    let mut raw_input = inp.to_vec();
    //println!("raw_input: {}", format_bits(&raw_input));

    while raw_input.len() > desired_byte_length {
        //println!("raw_input (shrinking): {}", format_bits(&raw_input));
        raw_input.remove(raw_input.len() - 1);
    }

    // 2. flip endianness if need be
    //println!("raw_input: {}", format_bits(&raw_input));
    if big_endian == false {
        //println!("little_endian input, flipping raw_input");
        raw_input.reverse();
    }

    // 2. make the bit vector, shift as needed
    let mut bv = bv::BitVec::<bv::BigEndian, u8>::from_vec(raw_input);
    bv.rotate_left(start);
    //println!("post_rotation: {}", format_bits(&bv.as_slice().to_vec()));

    // 3. zero out the bits after the range we're interested in
    let bit_length = inp.len() * 8;
    let mut bit_zeroing_index = bit_length;
    while bit_zeroing_index < bit_length {
        bv.set(bit_zeroing_index, false);
        bit_zeroing_index = bit_zeroing_index + 1;
    }

    let mut ret = bv.as_slice().to_vec();

    // 5. if little-endian, reverse
    if big_endian == false {
        ret.reverse();
    }

    return Ok(ret);
}

pub fn subset_with_pad(
    inp: &[u8],
    start: usize,
    num_bits: usize,
    endianness: Endianness,
) -> Result<Vec<u8>> {
    // we're passed however many bytes the output should end up being
    let output_size = inp.len();
    // bit_subset should return us an even number of bytes
    let mut subset_bytes = num_bits / 8;

    let mut bytes: Vec<u8> = bit_subset(inp, start, num_bits, endianness == nom::Endianness::Big)?;
    //println!("subset_with_pad: len(input): {}, len(bytes): {}", inp.len(), bytes.len());
    while subset_bytes < output_size {
        subset_bytes = subset_bytes + 1;
        match endianness {
            nom::Endianness::Big => bytes.insert(0, 0),
            nom::Endianness::Little => bytes.push(0),
        }
    }

    //println!("subset_with_pad output: {}", format_bits(&bytes));
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use *;

    #[test]
    fn normal_record_header_test() {
        let record_header_data = [0b01000000];
        let expected = FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        };

        let (_, rh) = normal_record_header(&record_header_data).unwrap();
        //println!("{:?}", rh);

        assert_eq!(rh, FitRecordHeader::Normal(expected));
    }

    #[test]
    fn definition_message_test() {
        let defintion_message_data = [
            0b00000000, // reserved, 1 byte
            0b00000000, // architecture, 1 byte
            0b00000000, 0b00000000, // global mseg num, 2 bytes
            0b00000100, // num_fields, 1 bytes, 4 here
            0b00000011, 0b00000100, 0b10001100, // 1st field def
            0b00000100, 0b00000100, 0b10000110, // 2nd field def
            0b00000001, 0b00000010, 0b10000100, // 3rd field def
            0b00000000, 0b00000001, 0b00000000, // 4th field def
        ];

        let rh = FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        };

        let expected = FitDefinitionMessage {
            header: FitNormalRecordHeader {
                message_type: FitNormalRecordHeaderMessageType::Definition,
                developer_fields_present: false,
                local_mesg_num: 0,
            },
            endianness: Endianness::Little,
            global_mesg_num: FitGlobalMesgNum::Known(FitFieldMesgNum::FileId),
            num_fields: 4,
            field_definitions: vec![
                FitFieldDefinition {
                    definition_number: 3,
                    field_size: 4,
                    base_type: 140,
                },
                FitFieldDefinition {
                    definition_number: 4,
                    field_size: 4,
                    base_type: 134,
                },
                FitFieldDefinition {
                    definition_number: 1,
                    field_size: 2,
                    base_type: 132,
                },
                FitFieldDefinition {
                    definition_number: 0,
                    field_size: 1,
                    base_type: 0,
                },
            ],
            developer_field_definitions: vec![],
            num_developer_fields: 0,
            message_size: 11,
        };

        let (res, _) = parse_definition_message(&defintion_message_data, rh).unwrap();
        //let res = definition_message(&defintion_message_data);
        assert_eq!(res, Some(expected));
    }
}

pub mod fittypes;
#[macro_use]
mod fitparsers;
mod errors;
pub mod fitfile;
pub mod fitparsingstate;
#[macro_use]
pub mod fittypes_utils;
