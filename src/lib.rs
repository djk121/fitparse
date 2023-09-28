#![feature(error_generic_member_access)]

use std::collections::HashMap;
use std::convert::From;
use std::ops::Deref;
use std::sync::Arc;

use std::fmt;

extern crate bitvec;
use bitvec::order as bitvec_order;
use bitvec::prelude as bv;

extern crate chrono;

extern crate nom;
use nom::number::Endianness;

extern crate conv;
use conv::*;

extern crate anyhow;
extern crate thiserror;
extern crate paste;

use errors::Result;

use fitparsers::{
    parse_bool, parse_byte, parse_definition_message, parse_enum, parse_fit_file_header,
    parse_float32, parse_float64, parse_record_header, parse_sint16, parse_sint32, parse_sint64,
    parse_sint8, parse_string, parse_uint16, parse_uint16z, parse_uint32, parse_uint32z,
    parse_uint64, parse_uint64z, parse_uint8, parse_uint8z,
};
use fitparsingstate::FitParsingState;
use fittypes::{
    FitDataMessage, FitFieldFitBaseType, FitFieldMesgNum, FitMessageDeveloperDataId,
    FitMessageDeviceSettings, FitMessageFieldDescription,
};

use fittypes_utils::FitFieldDateTime;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct FitNormalRecordHeader {
    message_type: FitNormalRecordHeaderMessageType,
    developer_fields_present: bool,
    local_mesg_num: u16,
}

impl FitNormalRecordHeader {
    fn new(message_type: u8, developer_fields_present: bool, local_mesg_num: u16) -> Self {
        FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::from(message_type),
            developer_fields_present,
            local_mesg_num,
        }
    }
}

// these are supposed to come after some message indicating the base time
// to_which time_offset should be added, so we'll require extra context from
// the parser to figure out the absolute timestmap
#[derive(Debug, PartialEq, Clone)]
pub struct FitCompressedTimestampHeader {
    local_mesg_num: u16,
    offset_secs: u8,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FitFileHeader {
    header_size: u8,
    protocol_version: u8,
    profile_version: u16,
    data_size: u32,
    crc: Vec<u8>,
}

impl FitFileHeader {
    pub fn parse(input: &[u8]) -> Result<(&[u8], FitFileHeader)> {
        parse_fit_file_header(input)
    }
}

trait FitRecord {
    fn message_name(&self) -> &'static str;
}

#[derive(Debug)]
pub enum FitMessage {
    Data(FitDataMessage),
    Definition(Arc<FitDefinitionMessage>),
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
) -> Result<(FitMessage, &'a [u8])> {
    // failing to parse the header is unrecoverable
    let (o, header) = match parse_record_header(input) {
        Ok(res) => res,
        Err(e) => {
            //return Err(errors::parse_error(format!("error parsing header: {}", e)));
            return Err(errors::FitParseError::ParseError(e.to_string()));
        }
    };

    let (fit_message, out) = match header {
        FitRecordHeader::Normal(normal_header) => match normal_header.message_type {
            FitNormalRecordHeaderMessageType::Data => {
                let (data_message, o) = FitDataMessage::parse(
                    o,
                    FitRecordHeader::Normal(normal_header),
                    parsing_state,
                    None,
                )?;
                (FitMessage::Data(data_message), o)
            }
            FitNormalRecordHeaderMessageType::Definition => {
                let (definition_message, o) =
                    FitDefinitionMessage::parse(o, normal_header, parsing_state)?;
                parsing_state.add_definition(
                    definition_message.header.local_mesg_num,
                    Arc::clone(&definition_message),
                );
                (FitMessage::Definition(Arc::clone(&definition_message)), o)
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
            (FitMessage::Data(data_message), o)
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
                            if let AdjustedValue::Vec(v) = &time_zone_offset.value {
                                parsing_state
                                    .set_timezone_offset(<f64>::from(v[0].clone()) * 15.0 * 60.0);
                            }
                        }
                    }
                }
                FitDataMessage::FieldDescription(fd) => {
                    let developer_data_index =
                        u8::from(fd.developer_data_index.value.get_single()?);
                    let field_definition_number =
                        u8::from(fd.field_definition_number.value.get_single()?);
                    parsing_state.add_developer_field_description(
                        developer_data_index,
                        field_definition_number,
                        Arc::<FitMessageFieldDescription>::from(fd.clone()),
                    );
                }
                FitDataMessage::DeveloperDataId(ddi) => {
                    let developer_data_index =
                        u8::from(ddi.developer_data_index.value.get_single()?);
                    parsing_state.add_developer_data_id(
                        developer_data_index,
                        Arc::<FitMessageDeveloperDataId>::from(ddi.clone()),
                    );
                }
                _ => {}
            }
        }
        _ => {}
    }

    Ok((fit_message, out))
}

pub trait FitFieldParseable: Sized {
    fn parse(input: &[u8], parse_config: &FitParseConfig) -> errors::Result<Self>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum BasicValue<T: FitFieldParseable + Clone> {
    NotYetParsedSingle,
    NotYetParsedVec,
    Single(T),
    Vec(Vec<T>),
}

impl<T: FitFieldParseable + Clone> BasicValue<T> {
    #[allow(dead_code)]
    fn get_single(&self) -> Result<T> {
        match self {
            BasicValue::Single(x) => Ok(x.clone()),
            _ => Err(errors::bad_basic_value_call()),
        }
    }

    #[allow(dead_code)]
    fn get_vec(&self) -> Result<Vec<T>> {
        match self {
            BasicValue::Vec(x) => Ok(x.to_vec()),
            _ => Err(errors::bad_basic_value_call()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FitFieldBasicValue<T: FitFieldParseable + Clone> {
    pub value: BasicValue<T>,
    pub units: String,
    pub components: Vec<FitParseConfig>,
}

impl<T: FitFieldParseable + Clone> FitFieldBasicValue<T> {
    fn new_single(units: String) -> FitFieldBasicValue<T> {
        FitFieldBasicValue {
            value: BasicValue::<T>::NotYetParsedSingle,
            units: units,
            components: vec![],
        }
    }

    fn new_vec(units: String) -> FitFieldBasicValue<T> {
        FitFieldBasicValue {
            value: BasicValue::<T>::NotYetParsedVec,
            units: units,
            components: vec![],
        }
    }

    fn add_components<'a>(&self, components: Vec<FitParseConfig>) -> FitFieldBasicValue<T> {
        FitFieldBasicValue {
            value: self.value.clone(),
            units: self.units.clone(),
            components: components,
        }
    }

    fn is_parsed(&self) -> bool {
        match self.value {
            BasicValue::NotYetParsedSingle | BasicValue::NotYetParsedVec => false,
            _ => true,
        }
    }

    fn parse(
        &mut self,
        input: &[u8],
        parse_config: &FitParseConfig,
    ) -> Result<Vec<FitParseConfig>> {
        match self.value {
            BasicValue::NotYetParsedSingle | BasicValue::Single(_) => {
                self.value = BasicValue::Single(T::parse(input, parse_config)?);
            }
            BasicValue::NotYetParsedVec | BasicValue::Vec(_) => {
                let mut outp = input;
                let mut num_to_parse = parse_config.num_in_field();
                let mut v = vec![];
                while num_to_parse > 0 {
                    let val = T::parse(outp, parse_config)?;
                    v.push(val);
                    outp = &outp[parse_config.base_type_size()..];
                    num_to_parse = num_to_parse - 1;
                }
                self.value = BasicValue::Vec(v);
            }
        }

        let mut actions = vec![];
        for component in &self.components {
            actions.push(component.add_bytes_to_parse(&input[..parse_config.field_size()]));
        }
        Ok(actions)
    }

    pub fn get_single(&self) -> Result<T> {
        match self.value {
            BasicValue::Single(ref v) => return Ok(v.clone()),
            _ => return Err(errors::bad_basic_value_call()),
        }
    }

    pub fn get_vec(&self) -> Result<Vec<T>> {
        match self.value {
            BasicValue::Vec(ref v) => Ok(v.clone()),
            _ => Err(errors::bad_basic_value_call()),
        }
    }
}

impl<T: fmt::Display + FitFieldParseable + Clone> fmt::Display for FitFieldBasicValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            BasicValue::NotYetParsedSingle => return Ok(()),
            BasicValue::NotYetParsedVec => return Ok(()),
            BasicValue::Single(ref x) => {
                write!(f, "{}", x)?;
                if !self.units.is_empty() {
                    write!(f, " ({})", self.units)?;
                }
                return Ok(());
            }
            BasicValue::Vec(ref v) => {
                write!(f, "[")?;
                let as_strings: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                let joined = as_strings.join(", ");
                write!(f, "{}]", joined)?;
                if !self.units.is_empty() {
                    write!(f, " ({})", self.units)?;
                }
                return Ok(());
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AdjustedValue {
    NotYetParsedSingle,
    NotYetParsedVec,
    Single(FitFloat64),
    Vec(Vec<FitFloat64>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PreAdjustedValue<T: FitFieldParseable + FitF64Convertible + Clone> {
    NotYetParsedSingle,
    NotYetParsedVec,
    Single(T),
    Vec(Vec<T>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FitFieldAdjustedValue<T: FitFieldParseable + FitF64Convertible + Clone> {
    pub value: AdjustedValue,
    pub parsed_value: PreAdjustedValue<T>,
    pub units: String,
    pub scale: f64,
    pub offset: f64,
    pub components: Vec<FitParseConfig>,
}

impl<T: FitFieldParseable + FitF64Convertible + Clone> FitFieldAdjustedValue<T> {
    fn new_single(units: String, scale: f64, offset: f64) -> FitFieldAdjustedValue<T> {
        FitFieldAdjustedValue {
            value: AdjustedValue::NotYetParsedSingle,
            parsed_value: PreAdjustedValue::NotYetParsedSingle,
            units: units,
            scale: scale,
            offset: offset,
            components: vec![],
        }
    }

    fn new_vec(units: String, scale: f64, offset: f64) -> FitFieldAdjustedValue<T> {
        FitFieldAdjustedValue {
            value: AdjustedValue::NotYetParsedVec,
            parsed_value: PreAdjustedValue::NotYetParsedVec,
            units: units,
            scale: scale,
            offset: offset,
            components: vec![],
        }
    }

    fn add_components(&self, components: Vec<FitParseConfig>) -> FitFieldAdjustedValue<T> {
        FitFieldAdjustedValue {
            value: self.value.clone(),
            parsed_value: self.parsed_value.clone(),
            units: self.units.clone(),
            scale: self.scale.clone(),
            offset: self.offset.clone(),
            components: components,
        }
    }

    #[allow(dead_code)]
    fn is_parsed(&self) -> bool {
        match self.value {
            AdjustedValue::NotYetParsedSingle | AdjustedValue::NotYetParsedVec => false,
            _ => true,
        }
    }

    fn parse(
        &mut self,
        input: &[u8],
        parse_config: &FitParseConfig,
    ) -> Result<Vec<FitParseConfig>> {
        match self.value {
            AdjustedValue::NotYetParsedSingle | AdjustedValue::Single(_) => {
                let x = T::parse(input, parse_config)?;
                let val = AdjustedValue::Single(self.adjust(&x, parse_config));
                self.parsed_value = PreAdjustedValue::Single(x);
                self.value = val;
            }
            AdjustedValue::NotYetParsedVec | AdjustedValue::Vec(_) => {
                let mut outp = input;
                let mut num_to_parse = parse_config.num_in_field();
                let mut v = vec![];
                while num_to_parse > 0 {
                    let val = T::parse(outp, parse_config)?;
                    v.push(val);
                    outp = &outp[parse_config.base_type_size()..];
                    num_to_parse = num_to_parse - 1;
                }
                self.value =
                    AdjustedValue::Vec(v.iter().map(|x| self.adjust(x, parse_config)).collect());
            }
        }
        let mut actions = vec![];
        for component in &self.components {
            actions.push(component.add_bytes_to_parse(&input[..parse_config.field_size()]));
        }
        Ok(actions)
    }

    fn adjust(&self, val: &T, parse_config: &FitParseConfig) -> FitFloat64 {
        let mut adjusted = val.to_f64();

        // special handling for lat/long; convert from semicircles to degrees
        if self.units == "deg" {
            return FitFloat64::new(adjusted * (180.0_f64 / 2_f64.powf(31.0)));
        }

        // FIXME: there should be a cleaner way to configure this, but if
        // this is a component, and scale/offset are in the Fit Profile
        // for the target field, but not for the component definition,
        // we don't want to adjust the field at all.
        if parse_config.use_stored_input() && parse_config.scale_and_offset.is_none() {
            return FitFloat64::new(adjusted);
        }

        // if scale/offset are present in a component definition, they are
        // carried through the parse_config to here. If they are present on the
        // parse_config, use them instead of scale/offset as defined in
        // the field definition row.
        let (scale, offset) = match parse_config.scale_and_offset {
            Some(scale_and_offset) => scale_and_offset,
            None => (self.scale, self.offset),
        };

        if scale != 1.0 {
            adjusted = adjusted / scale;
        }

        if offset != 0.0 {
            adjusted = adjusted - offset;
        }
        FitFloat64::new(adjusted)
    }

    #[allow(dead_code)]
    pub fn get_single(&self) -> Result<f64> {
        match self.value {
            AdjustedValue::Single(ref v) => return Ok(v.to_f64()),
            _ => return Err(errors::bad_adjusted_value_call()),
        }
    }

    #[allow(dead_code)]
    pub fn get_vec(&self) -> Result<Vec<f64>> {
        match self.value {
            AdjustedValue::Vec(ref v) => Ok(v.iter().map(|x| x.to_f64()).collect()),
            _ => Err(errors::bad_adjusted_value_call()),
        }
    }
}

impl<T: fmt::Display + FitFieldParseable + FitF64Convertible + Clone> fmt::Display
    for FitFieldAdjustedValue<T>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            AdjustedValue::NotYetParsedSingle => return Ok(()), //write!(f, "<not yet parsed, single>"),
            AdjustedValue::NotYetParsedVec => return Ok(()), // write!(f, "<not yet parsed, vec>"),
            AdjustedValue::Single(ref x) => {
                write!(f, "{}", x)?;
                if !self.units.is_empty() {
                    write!(f, " ({})", self.units)?;
                }
                return Ok(());
            }
            AdjustedValue::Vec(ref v) => {
                write!(f, "[")?;
                let as_strings: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                let joined = as_strings.join(", ");
                write!(f, "{}]", joined)?;
                if !self.units.is_empty() {
                    write!(f, " ({})", self.units)?;
                }
                return Ok(());
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FitParseConfig {
    field_definition: Option<FitFieldDefinition>,
    endianness: Option<Endianness>,
    tz_offset_secs: Option<f64>,
    bytes_to_parse: Option<Vec<u8>>,
    bit_range: Option<(usize, usize)>,
    scale_and_offset: Option<(f64, f64)>,
    units: Option<String>,
}

macro_rules! fit_parse_config {
    ($endianness:expr) => {
        FitParseConfig {
            field_definition: None,
            endianness: Some($endianness),
            tz_offset_secs: None,
            bit_range: None,
            bytes_to_parse: None,
            scale_and_offset: None,
            units: None,
        }
    };
    ($endianness:expr, $size:expr) => {
        FitParseConfig {
            field_definition: None,
            endianness: Some($endianness),
            tz_offset_secs: None,
            bit_range: None,
            bytes_to_parse: None,
            scale_and_offset: None,
            units: None,
        }
    };
    ($endianness:expr, $size:expr, $tz_offset_secs:expr) => {
        FitParseConfig {
            field_definition: None,
            endianness: Some($endianness),
            tz_offset_secs: Some($tz_offset_secs),
            bit_range: None,
            bytes_to_parse: None,
            scale_and_offset: None,
            units: None,
        }
    };
}

impl FitParseConfig {
    pub fn new(
        field: FitFieldDefinition,
        endianness: Endianness,
        tz_offset: f64,
    ) -> FitParseConfig {
        FitParseConfig {
            field_definition: Some(field),
            endianness: Some(endianness),
            tz_offset_secs: Some(tz_offset),
            bit_range: None,
            bytes_to_parse: None,
            scale_and_offset: None,
            units: None,
        }
    }

    // useful for cases where a FPC is needed due to the API,
    // but not actually used
    pub fn new_empty() -> FitParseConfig {
        FitParseConfig {
            field_definition: None,
            endianness: None,
            tz_offset_secs: None,
            bit_range: None,
            bytes_to_parse: None,
            scale_and_offset: None,
            units: None,
        }
    }

    pub fn new_from_component(
        definition_number: u8,
        field_size: usize,
        base_type: u8,
        endianness: Endianness,
        bit_start: usize,
        num_bits: usize,
        scale_and_offset: Option<(f64, f64)>,
        units: Option<String>,
    ) -> Result<FitParseConfig> {
        Ok(FitParseConfig {
            field_definition: Some(FitFieldDefinition::new(
                definition_number,
                field_size,
                base_type,
            )?),
            endianness: Some(endianness),
            tz_offset_secs: None,
            bit_range: Some((bit_start, num_bits)),
            bytes_to_parse: None,
            scale_and_offset: scale_and_offset,
            units: units,
        })
    }

    pub fn add_bytes_to_parse(&self, bytes: &[u8]) -> FitParseConfig {
        FitParseConfig {
            field_definition: self.field_definition.clone(),
            endianness: self.endianness.clone(),
            tz_offset_secs: self.tz_offset_secs.clone(),
            bit_range: self.bit_range.clone(),
            bytes_to_parse: Some(bytes.to_vec()),
            scale_and_offset: self.scale_and_offset.clone(),
            units: self.units.clone(),
        }
    }

    pub fn use_stored_input(&self) -> bool {
        if self.bytes_to_parse.is_some() {
            true
        } else {
            false
        }
    }

    pub fn get_stored_input(&self) -> Result<Vec<u8>> {
        if self.bit_range.is_none() {
            panic!("FitParseConfig::get_stored_input without bit_range set")
        }

        if self.endianness.is_none() {
            panic!("FitParseConfig::get_stored_input without endianness set")
        }

        let (start, num_bits) = self.bit_range.unwrap();

        if let Some(ref bytes) = self.bytes_to_parse {
            let o = bit_subset(
                bytes,
                start,
                num_bits,
                self.endianness.unwrap(),
                self.field_size(),
            )?;
            Ok(o)
        } else {
            panic!("FitParseConfig::get_stored_input without bytes_to_parse set")
        }
    }

    pub fn field_definition_number(&self) -> u8 {
        match self.field_definition {
            None => panic!("called field_definition_number without field_definiton set"),
            Some(fd) => fd.definition_number,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.field_definition {
            Some(field) => field.is_array(),
            None => false,
        }
    }

    pub fn endianness(&self) -> Endianness {
        match self.endianness {
            None => panic!("used empty FitParseConfig.endianness"),
            Some(v) => v,
        }
    }

    pub fn field_size(&self) -> usize {
        if let Some(fd) = self.field_definition {
            fd.field_size
        } else {
            panic!("called size() without field_definition set")
        }
    }

    pub fn num_in_field(&self) -> usize {
        if let Some(fd) = self.field_definition {
            fd.num_in_field()
        } else {
            panic!("called num_in_field() without field_definition set")
        }
    }

    pub fn base_type_size(&self) -> usize {
        if let Some(fd) = self.field_definition {
            fd.base_type_size()
        } else {
            panic!("called base_type_size() without field_definition set")
        }
    }

    pub fn tz_offset_secs(&self) -> f64 {
        match self.tz_offset_secs {
            None => panic!("used empty FitParseConfig.tz_offset_secs"),
            Some(v) => v,
        }
    }

    pub fn base_type(&self) -> FitFieldFitBaseType {
        match self.field_definition {
            None => panic!("called FitParseConfig.base_type() without setting field_definition"),
            Some(v) => FitFieldFitBaseType::from(v.base_type),
        }
    }

    pub fn is_string(&self) -> bool {
        if let Some(fd) = self.field_definition {
            fd.is_string()
        } else {
            panic!("called is_string() without field_definition set")
        }
    }

    pub fn is_byte(&self) -> bool {
        if let Some(fd) = self.field_definition {
            fd.is_byte()
        } else {
            panic!("called is_byte() without field_definition set")
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct FitMessageUnknownToSdk {
    number: u16,
    header: FitRecordHeader,
    definition_message: Arc<FitDefinitionMessage>,
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
        parsing_state: &FitParsingState,
        _timestamp: Option<FitFieldDateTime>,
    ) -> Result<(FitMessageUnknownToSdk, &'a [u8])> {
        let definition_message = parsing_state.get_definition(header.local_mesg_num())?;
        let mut message = FitMessageUnknownToSdk {
            number: number,
            header: header,
            definition_message: Arc::clone(&definition_message),
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
                //let bt = Backtrace::force_capture();
                //let mut err_string = String::from("Error parsing FitMessageUnknownToSdk:");
                //err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                //err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(errors::message_parse_failed(
                    "FitMessageUnknownToSdk".to_string(),
                    definition_message,
                    input[..(message.definition_message.message_size)].to_vec(),
                    e,
                ));
            }
        };

        let mut inp2 = o;
        for dev_field in &message.definition_message.developer_field_definitions {
            let field_description = parsing_state.get_developer_field_description(
                dev_field.developer_data_index,
                dev_field.definition_number,
            )?;
            //let dev_data_definition =
            //    parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;
            //let field_description =
            //    dev_data_definition.get_field_description(dev_field.definition_number)?;

            let base_type_num = u8::from(field_description.fit_base_type_id.get_single()?);

            let field_def = FitFieldDefinition::new(
                dev_field.definition_number,
                dev_field.field_size,
                base_type_num,
            )?;
            let parse_config =
                FitParseConfig::new(field_def, message.definition_message.endianness, 0.0);
            let dd = FitFieldDeveloperData::parse(inp2, field_description.clone(), &parse_config)?;
            message.developer_fields.push(dd);
            // we can run out of input before all fields are consumed. according
            // to the spec, buffering with zero-padded fields is appropriate
            if inp2.len() < parse_config.field_size() {
                inp2 = &inp2[inp2.len()..];
            } else {
                inp2 = &inp2[parse_config.field_size()..];
            }
        }

        Ok((message, inp2))
    }

    fn parse_internal<'a>(
        message: &mut FitMessageUnknownToSdk,
        input: &'a [u8],
        tz_offset: f64,
    ) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_config =
                FitParseConfig::new(*field, message.definition_message.endianness, tz_offset);
            let val = FitBaseValue::parse(inp, &parse_config)?;
            inp = &inp[field.field_size..];
            message.unknown_fields.insert(field.definition_number, val);
        }
        Ok(inp)
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    // can I just parse FitFieldMesgNum directly?
    fn parse(input: &[u8], endianness: Endianness) -> Result<(FitGlobalMesgNum, &[u8])> {
        let parse_config = fit_parse_config!(endianness);
        let (raw_num, o) = match parse_uint16(input, &parse_config) {
            Err(_) => return Err(errors::parse_error("error parsing FitGlobalMesgNum")),
            Ok(raw_num) => (raw_num, &input[2..]),
        };

        match FitFieldMesgNum::from(raw_num) {
            FitFieldMesgNum::UnknownToSdk => Ok((FitGlobalMesgNum::Unknown(raw_num), o)),
            known => Ok((FitGlobalMesgNum::Known(known), o)),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct FitFieldDefinition {
    definition_number: u8,
    field_size: usize,
    base_type: FitFieldFitBaseType,
}

impl FitFieldDefinition {
    pub fn new(
        definition_number: u8,
        field_size: usize,
        base_type: u8,
    ) -> Result<FitFieldDefinition> {
        let bt = match FitFieldFitBaseType::from(base_type) {
            FitFieldFitBaseType::UnknownToSdk => {
                return Err(errors::unexpected_fit_field_base_type(
                    FitFieldFitBaseType::UnknownToSdk,
                ))
            }
            t => t,
        };

        Ok(FitFieldDefinition {
            definition_number: definition_number,
            field_size: field_size,
            base_type: bt,
        })
    }

    pub fn field_name(&self, mesg_num: &FitGlobalMesgNum) -> &'static str {
        FitDataMessage::field_name(mesg_num, self.definition_number)
    }

    pub fn is_array(&self) -> bool {
        match self.base_type {
            FitFieldFitBaseType::String | FitFieldFitBaseType::Byte => false,
            _ => self.array_size() > 1,
        }
    }

    pub fn array_size(&self) -> usize {
        self.field_size / self.base_type_size()
    }

    pub fn is_string(&self) -> bool {
        self.base_type == FitFieldFitBaseType::String
    }

    pub fn is_byte(&self) -> bool {
        self.base_type == FitFieldFitBaseType::Byte
    }

    pub fn num_in_field(&self) -> usize {
        self.field_size / self.base_type_size()
    }

    pub fn base_type_name(&self) -> String {
        self.base_type.to_string()
    }

    pub fn base_type_size(&self) -> usize {
        let sz = match self.base_type {
            FitFieldFitBaseType::String => 1 * self.field_size,
            FitFieldFitBaseType::Byte => 1 * self.field_size,
            FitFieldFitBaseType::Enum => 1,
            FitFieldFitBaseType::Sint8 => 1,
            FitFieldFitBaseType::Uint8 => 1,
            FitFieldFitBaseType::Uint8z => 1,
            FitFieldFitBaseType::Sint16 => 2,
            FitFieldFitBaseType::Uint16 => 2,
            FitFieldFitBaseType::Uint16z => 2,
            FitFieldFitBaseType::Sint32 => 4,
            FitFieldFitBaseType::Uint32 => 4,
            FitFieldFitBaseType::Uint32z => 4,
            FitFieldFitBaseType::Float32 => 4,
            FitFieldFitBaseType::Float64 => 8,
            FitFieldFitBaseType::Sint64 => 8,
            FitFieldFitBaseType::Uint64 => 8,
            FitFieldFitBaseType::Uint64z => 8,
            other => panic!(
                "should be impossible: bad base_type ({:?}) after construction",
                other
            ),
        };
        sz
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FitDeveloperFieldDefinition {
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

#[derive(Debug, PartialEq, Clone)]
pub struct FitDefinitionMessage {
    header: FitNormalRecordHeader,
    endianness: Endianness,
    global_mesg_num: FitGlobalMesgNum,
    num_fields: u8,
    message_size: usize,
    field_definitions: Vec<FitFieldDefinition>,
    num_developer_fields: usize,
    developer_field_definitions: Vec<FitDeveloperFieldDefinition>,
    developer_field_descriptions: HashMap<u8, HashMap<u8, Arc<FitMessageFieldDescription>>>,
    developer_data_ids: HashMap<u8, Arc<FitMessageDeveloperDataId>>,
}

/*
// because deriving doesn't work for the HashMap members
impl PartialEq for FitDefinitionMessage {
    fn eq(&self, other: &Self) -> bool {
        if self.header != other.header { return false; }
        if self.endianness != other.endianness { return false; }
        if self.global_mesg_num != other.global_mesg_num { return false; }
        if self.num_fields != other.num_fields { return false; }
        if self.message_size != other.message_size { return false; }
        if self.field_definitions != other.field_definitions { return false; }
        if self.num_developer_fields != other.num_developer_fields { return false; }
        if self.developer_field_definitions != other.developer_field_definitions { return false; }
        for (k, v) in self.developer_field_descriptions {
            if other.developer_field_descriptions[&k] != v { return false; }
        }

        return true;
    }
}
*/

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
        writeln!(f, "  {: >28}:", "developer_field_descriptions")?;
        for developer_field_description in &self.developer_field_descriptions {
            writeln!(f, "  {: >30}{:?}", " ", developer_field_description)?;
        }
        writeln!(
            f,
            "  {: >28}: {:?}",
            "developer_data_ids", self.developer_data_ids
        )?;
        Ok(())
    }
}

impl FitDefinitionMessage {
    fn parse<'a, 'b>(
        input: &'a [u8],
        header: FitNormalRecordHeader,
        parsing_state: &'b FitParsingState,
    ) -> Result<(Arc<FitDefinitionMessage>, &'a [u8])> {
        let (o, mut fdm) = parse_definition_message(input, header)?;

        // if there are developer fields, find their field descriptions and add them
        if fdm.num_developer_fields > 0 {
            let mut ddis: Vec<u8> = fdm
                .developer_field_definitions
                .iter()
                .map(|ffd| ffd.developer_data_index)
                .collect::<Vec<u8>>();
            ddis.sort();
            ddis.dedup();

            for dfd in &fdm.developer_field_definitions {
                let fd = parsing_state.get_developer_field_description(
                    dfd.developer_data_index,
                    dfd.definition_number,
                )?;
                let p = fdm
                    .developer_field_descriptions
                    .entry(dfd.developer_data_index)
                    .or_insert(HashMap::new());
                p.insert(dfd.definition_number, Arc::clone(&fd));
            }

            for developer_data_index in ddis {
                let developer_data_id =
                    parsing_state.get_developer_data_id(developer_data_index)?;
                fdm.developer_data_ids
                    .insert(developer_data_index, developer_data_id);
            }
        }

        Ok((Arc::new(fdm), o))
    }
}

#[derive(Debug, PartialEq, Clone)]
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

trait FitVecParser<T> {
    fn parse(input: &[u8], parse_config: &FitParseConfig) -> Result<Vec<T>>;
}

macro_rules! fit_base_type_base {
    ($name:ident, $name_enum:ident, $name_vec:ident, $type:ty, $parser:ident) => {
        #[derive(Debug, PartialEq, Clone)]
        pub enum $name_enum {
            InvalidValue,
            ValidValue($type),
        }

        #[derive(Debug, PartialEq, Clone)]
        pub struct $name(pub $name_enum);

        type $name_vec = Vec<$name>;

        impl $name {
            pub fn new(v: $type) -> $name {
                $name($name_enum::ValidValue(v))
            }
        }

        impl FitFieldParseable for $name {
            fn parse(input: &[u8], parse_config: &FitParseConfig) -> Result<$name> {
                match $parser(input, parse_config) {
                    Ok(val) => Ok($name($name_enum::ValidValue(val))),
                    Err(_e) => {
                        //println!("error parsing: {:?}", e);
                        //eprintln!("error parsing: {:?}", e);
                        Ok($name($name_enum::InvalidValue))
                    }
                }
            }
        }

        impl FitFieldParseable for $name_vec {
            fn parse(input: &[u8], parse_config: &FitParseConfig) -> Result<$name_vec> {
                let mut num_to_parse = parse_config.num_in_field();

                let mut outp = input;
                let mut v: Vec<$name> = vec![];

                while num_to_parse > 0 {
                    v.push($name::parse(outp, parse_config)?);
                    outp = &outp[parse_config.base_type_size()..];
                    num_to_parse = num_to_parse - 1;
                }
                Ok(v)
            }
        }
    };
}

pub trait FitF64Convertible {
    fn to_f64(&self) -> f64;
}

macro_rules! fit_base_type {
    ($name:ident, $name_enum:ident, $name_vec:ident, $type:ty, $parser:ident) => {
        fit_base_type_base!($name, $name_enum, $name_vec, $type, $parser);

        impl FitF64Convertible for $name {
            fn to_f64(&self) -> f64 {
                match self.0 {
                    $name_enum::InvalidValue => 0.0,
                    $name_enum::ValidValue(v) => match f64::value_from(v) {
                        Err(_) => 0.0,
                        Ok(x) => x,
                    },
                }
            }
        }

        impl From<$name> for $type {
            fn from(t: $name) -> $type {
                match t.0 {
                    $name_enum::InvalidValue => 0 as $type,
                    $name_enum::ValidValue(v) => v as $type,
                }
            }
        }

        impl From<&$name> for $type {
            fn from(t: &$name) -> $type {
                match t.0 {
                    $name_enum::InvalidValue => 0 as $type,
                    $name_enum::ValidValue(v) => v as $type,
                }
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    $name_enum::InvalidValue => write!(f, "InvalidValue"),
                    $name_enum::ValidValue(x) => write!(f, "{}", x),
                }
            }
        }
    };
}

fit_base_type_base!(FitBool, FitBoolOptions, FitBoolVec, bool, parse_bool);
fit_base_type_base!(FitEnum, FitEnumOptions, FitEnumVec, u8, parse_enum);
fit_base_type!(FitUint8, FitUint8Options, FitUint8Vec, u8, parse_uint8);
fit_base_type!(FitUint8z, FitUint8zOptions, FitUint8zVec, u8, parse_uint8z);
fit_base_type!(FitSint8, FitSint8Options, FitSint8Vec, i8, parse_sint8);
fit_base_type!(FitUint16, FitUint16Options, FitUint16Vec, u16, parse_uint16);
fit_base_type!(
    FitUint16z,
    FitUint16zOptions,
    FitUint16zVec,
    u16,
    parse_uint16z
);
fit_base_type!(FitSint16, FitSint16Options, FitSint16Vec, i16, parse_sint16);
fit_base_type!(FitUint32, FitUint32Options, FitUint32Vec, u32, parse_uint32);
fit_base_type!(
    FitUint32z,
    FitUint32zOptions,
    FitUint32zVec,
    u32,
    parse_uint32z
);
fit_base_type!(FitSint32, FitSint32Options, FitSint32Vec, i32, parse_sint32);
fit_base_type!(
    FitFloat32,
    FitFloat32Options,
    FitFloat32Vec,
    f32,
    parse_float32
);
fit_base_type!(FitUint64, FitUint64Options, FitUint64Vec, u64, parse_uint64);
fit_base_type!(
    FitUint64z,
    FitUint64zOptions,
    FitUint64zVec,
    u64,
    parse_uint64z
);
fit_base_type!(FitSint64, FitSint64Options, FitSint64Vec, i64, parse_sint64);
fit_base_type!(
    FitFloat64,
    FitFloat64Options,
    FitFloat64Vec,
    f64,
    parse_float64
);

impl From<f64> for FitFloat64 {
    fn from(x: f64) -> FitFloat64 {
        FitFloat64(FitFloat64Options::ValidValue(x))
    }
}

fit_base_type_base!(
    FitString,
    FitStringOptions,
    FitStringVec,
    String,
    parse_string
);
fit_base_type_base!(FitByte, FitByteOptions, FitByteVec, Vec<u8>, parse_byte);

// These need to be implemented manually, as FitByte is a Vec<u8>
// under the hood, and so cannot be formatted in the default way

impl fmt::Display for FitBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FitBoolOptions::InvalidValue => write!(f, "InvalidValue"),
            FitBoolOptions::ValidValue(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for FitEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FitEnumOptions::InvalidValue => write!(f, "InvalidValue"),
            FitEnumOptions::ValidValue(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for FitByte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FitByteOptions::InvalidValue => write!(f, "InvalidValue")?,
            FitByteOptions::ValidValue(ref v) => {
                write!(f, "[")?;
                let as_strings: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                let joined = as_strings.join(", ");
                write!(f, "{}]", joined)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for FitString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FitStringOptions::InvalidValue => write!(f, "InvalidValue"),
            FitStringOptions::ValidValue(ref x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum FitBaseValue {
    Enum(FitEnum),
    EnumVec(FitEnumVec),
    Sint8(FitSint8),
    Sint8Vec(FitSint8Vec),
    Uint8(FitUint8),
    Uint8Vec(FitUint8Vec),
    Uint8z(FitUint8z),
    Uint8zVec(FitUint8zVec),
    Sint16(FitSint16),
    Sint16Vec(FitSint16Vec),
    Uint16(FitUint16),
    Uint16Vec(FitUint16Vec),
    Uint16z(FitUint16z),
    Uint16zVec(FitUint16zVec),
    Sint32(FitSint32),
    Sint32Vec(FitSint32Vec),
    Uint32(FitUint32),
    Uint32Vec(FitUint32Vec),
    Uint32z(FitUint32z),
    Uint32zVec(FitUint32zVec),
    Float32(FitFloat32),
    Float32Vec(FitFloat32Vec),
    Sint64(FitSint64),
    Sint64Vec(FitSint64Vec),
    Uint64(FitUint64),
    Uint64Vec(FitUint64Vec),
    Uint64z(FitUint64z),
    Uint64zVec(FitUint64zVec),
    Float64(FitFloat64),
    Float64Vec(FitFloat64Vec),
    String(FitString),
    Byte(FitByte),
}

macro_rules! base_type_formatter {
    ($val:ident, $f:ident) => {
        write!($f, "{}", $val)
    };
}

macro_rules! base_type_vec_formatter {
    ($vals:ident, $f:ident) => {{
        write!($f, "[")?;
        for i in 0..$vals.len() - 1 {
            write!($f, "{}, ", $vals[i])?;
        }
        write!($f, "{}] ", $vals[$vals.len() - 1])
    }};
}

impl fmt::Display for FitBaseValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &FitBaseValue::Enum(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::EnumVec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Sint8(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Sint8Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint8(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint8Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint8z(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint8zVec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Sint16(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Sint16Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint16(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint16Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint16z(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint16zVec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Sint32(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Sint32Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint32(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint32Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint32z(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint32zVec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Float32(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Float32Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Sint64(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Sint64Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint64(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint64Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Uint64z(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Uint64zVec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::Float64(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Float64Vec(ref vals) => base_type_vec_formatter!(vals, f)?,
            &FitBaseValue::String(ref val) => base_type_formatter!(val, f)?,
            &FitBaseValue::Byte(ref val) => write!(f, "{:?}", val)?,
        }
        Ok(())
    }
}

macro_rules! fit_base_parse {
    ($ty_short:ident, $ty_long:ident, $ty_vec:ident, $input:expr, $parse_config:expr) => {{
        if $parse_config.is_array() {
            let v: Vec<$ty_long> = FitFieldParseable::parse($input, $parse_config)?;
            Ok(FitBaseValue::$ty_vec(v))
        } else {
            Ok(FitBaseValue::$ty_short($ty_long::parse(
                $input,
                $parse_config,
            )?))
        }
    }};
}

impl FitBaseValue {
    fn parse<'a>(input: &'a [u8], parse_config: &FitParseConfig) -> Result<FitBaseValue> {
        match parse_config.base_type() {
            FitFieldFitBaseType::Enum => {
                fit_base_parse!(Enum, FitEnum, EnumVec, input, parse_config)
            }
            FitFieldFitBaseType::Uint8 => {
                fit_base_parse!(Uint8, FitUint8, Uint8Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint8z => {
                fit_base_parse!(Uint8z, FitUint8z, Uint8zVec, input, parse_config)
            }
            FitFieldFitBaseType::Sint8 => {
                fit_base_parse!(Sint8, FitSint8, Sint8Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint16 => {
                fit_base_parse!(Uint16, FitUint16, Uint16Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint16z => {
                fit_base_parse!(Uint16z, FitUint16z, Uint16zVec, input, parse_config)
            }
            FitFieldFitBaseType::Sint16 => {
                fit_base_parse!(Sint16, FitSint16, Sint16Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint32 => {
                fit_base_parse!(Uint32, FitUint32, Uint32Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint32z => {
                fit_base_parse!(Uint32z, FitUint32z, Uint32zVec, input, parse_config)
            }
            FitFieldFitBaseType::Sint32 => {
                fit_base_parse!(Sint32, FitSint32, Sint32Vec, input, parse_config)
            }
            FitFieldFitBaseType::Float32 => {
                fit_base_parse!(Float32, FitFloat32, Float32Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint64 => {
                fit_base_parse!(Uint64, FitUint64, Uint64Vec, input, parse_config)
            }
            FitFieldFitBaseType::Uint64z => {
                fit_base_parse!(Uint64z, FitUint64z, Uint64zVec, input, parse_config)
            }
            FitFieldFitBaseType::Sint64 => {
                fit_base_parse!(Sint64, FitSint64, Sint64Vec, input, parse_config)
            }
            FitFieldFitBaseType::Float64 => {
                fit_base_parse!(Float64, FitFloat64, Float64Vec, input, parse_config)
            }
            FitFieldFitBaseType::Byte => {
                Ok(FitBaseValue::Byte(FitByte::parse(input, parse_config)?))
            }
            FitFieldFitBaseType::String => {
                Ok(FitBaseValue::String(FitString::parse(input, parse_config)?))
            }
            _ => Err(errors::parse_unknown_base_value()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FitFieldDeveloperData {
    field_description: Arc<FitMessageFieldDescription>,
    value: FitBaseValue,
}

impl FitFieldDeveloperData {
    fn parse<'a>(
        input: &'a [u8],
        field_description: Arc<FitMessageFieldDescription>,
        parse_config: &FitParseConfig,
    ) -> Result<FitFieldDeveloperData> {
        let val = FitBaseValue::parse(input, parse_config)?;
        Ok(FitFieldDeveloperData {
            field_description: field_description,
            value: val,
        })
    }
}

#[allow(dead_code)]
fn format_bits(input: &Vec<u8>) -> String {
    let mut s = String::new();
    for item in input {
        s.push_str(&format!("{:08b}, ", item));
    }

    s
}

pub fn bit_subset(
    inp: &[u8],
    start: usize,
    num_bits: usize,
    endianness: Endianness,
    output_size: usize,
) -> Result<Vec<u8>> {
    let mut raw_input = inp.to_vec();

    // flip endianness if need be
    if endianness == Endianness::Little {
        raw_input.reverse();
    }

    // make the bit vector, shift as needed
    let mut bv = bv::BitVec::<bitvec_order::Msb0, u8>::from_vec(raw_input);
    if start > 0 {
        bv.rotate_right(start);
    }

    // zero out any higher bits
    let mut clear_pos = 0;
    while clear_pos < (bv.len() - num_bits) {
        bv.set(clear_pos, false);
        clear_pos = clear_pos + 1;
    }

    let mut ret = bv.as_slice().to_vec();

    // shrink to fit, discarding high-order bytes
    while ret.len() > output_size {
        ret.remove(0);
    }

    // if little-endian, reverse
    if endianness == Endianness::Little {
        ret.reverse();
    }

    return Ok(ret);
}

#[cfg(test)]
mod tests {
    use *;

    #[test]
    fn bit_subset_test() {
        // input = 72
        // 0,5 and 5,3 should decode into 8 and 2

        let data = vec![0b01001000];

        let output1 = bit_subset(
            &data, // input
            0,     // start
            5,     // num_bits
            Endianness::Big,
            1, // output_size
        )
        .unwrap();

        assert_eq!(output1[0], 0b00001000);

        let output2 = bit_subset(
            &data, // input
            5,     // start
            3,     // num_bits
            Endianness::Big,
            1, // output_size
        )
        .unwrap();

        assert_eq!(output2[0], 0b00000010);
    }

    #[test]
    fn normal_record_header_test() {
        let record_header_data = [0b01000000];
        let expected = FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        };

        let (_, rh) = fitparsers::parse_normal_record_header(&record_header_data).unwrap();

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
                FitFieldDefinition::new(3, 4, 140).unwrap(),
                FitFieldDefinition::new(4, 4, 134).unwrap(),
                FitFieldDefinition::new(1, 2, 132).unwrap(),
                FitFieldDefinition::new(0, 1, 0).unwrap(),
            ],
            developer_field_definitions: vec![],
            num_developer_fields: 0,
            message_size: 11,
            developer_data_ids: HashMap::new(),
            developer_field_descriptions: HashMap::new(),
        };

        let (_, res) = parse_definition_message(&defintion_message_data, rh).unwrap();
        assert_eq!(res, expected);
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
