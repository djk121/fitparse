use std::backtrace::Backtrace;
use std::boxed::Box;
use std::sync::Arc;
use thiserror::Error;

use fmt::Debug;

use FitDefinitionMessage;
use FitFieldFitBaseType;

pub type Result<T> = anyhow::Result<T, FitParseError>;

#[derive(Error, Debug)]
pub enum FitParseError {
    #[error("insufficient data to finish parse, needed {0} bytes")]
    ParseIncomplete(usize),
    #[error("insufficient data to finish parse, unknown amount needed")]
    ParseIncompleteUnknown,
    #[error("invalid field value for field\n bt:\n{backtrace}")]
    ParseInvalidFieldValue { backtrace: Backtrace },
    #[error("error during parse: {0:?}")]
    ParseError(String),
    #[error("zero value found for type requiring non-zero")]
    ParseZero,
    #[error("timestamp relative to device power-on not supported")]
    UnsupportedRelativetimestamp,
    #[error("invalid value supplied for Field: {0:?}")]
    InvalidFieldValue(String),
    #[error("missing local -> global mesg_num mapping for local mesg num {0:?}")]
    InvalidLocalMesgNum(String),
    #[error("timestamp requested from ParsingState, but one hasn't been set")]
    TimestampBaseNotSet,
    #[error("timezone offset requested from ParsingState, but one hasn't been set")]
    TimezoneOffsetNotSet,
    #[error("incorrect header type supplied")]
    WrongHeaderType,
    #[error("unknown error")]
    UnknownError,
    #[error("get_timestamp called for message which doesn't have timestamp set")]
    TimestampNotSetOnMessage,
    #[error("message with timestamp field ended parsing without timestamp being set")]
    MissingTimestampField,
    #[error("attempt to parse field number not present in defintion message: {0}")]
    InvalidFieldNumber(u8),
    #[error("MessageParseFailed: {message_name}\n definition: {definition_message:?}\n bytes: {bytes:?}\n source: {source:#?}")]
    MessageParseFailed {
        message_name: String,
        definition_message: Arc<FitDefinitionMessage>,
        bytes: Vec<u8>,
        source: Box<FitParseError>,
    },
    #[error("bad call to as_64 or as_64_vec: not initialized or wrong variant")]
    BadAdjustedValueCall,
    #[error("bad call to as_single or as_vec: not initialized or wrong variant")]
    BadBasicValueCall,
    #[error("bad call to value(): not initialized")]
    BadFitBaseTypeValueCall,
    #[error("fit_base_type_id not supplied; can't parse developer data")]
    MissingFitBaseType,
    #[error("attempt to parse uncovered base value variant")]
    ParseUnknownBaseValue,
    #[error(
        "developer data definition number not found: {developer_data_id}, source: {backtrace}"
    )]
    DeveloperDataDefinitionNotFound {
        developer_data_id: u8,
        backtrace: Backtrace,
    },
    #[error("developer field number not found: {0}")]
    DeveloperFieldDescriptionNotFound(u8),
    #[error("insufficient data for shift")]
    InsufficientDataForShift,
    #[error("bad inputs for shift operation")]
    IncorrectShiftInput,
    #[error("field definition number not found: {0}")]
    FieldDefinitionNumberNotFound(u8),
    #[error("asked to parse bad subfield field number: {0}")]
    BadSubfieldFieldNumber(u8),
    #[error("invalid fit base type parse result")]
    InvalidFitBaseTypeParse,
    #[error("fit file too large, max_allowed: {0}, attempted: {1}")]
    FitFileTooLarge(usize, usize),
    #[error("Error parsing timestamps in Hr message")]
    HrMessageTimestamp,
    #[error("Field: Manufacture Range Min")]
    FieldMfgRangeMin,
    #[error("Field: Manufacture Range Max")]
    FieldMfgRangeMax,
    #[error("Field: Unknown to SDK")]
    FieldUnknownToSdk,
    #[error("Field: Invalid Value")]
    FieldInvalidValue,
    #[error("no FitMessageSport found in Fit File")]
    SportMessageNotPresent,
    #[error("unexpected FitField base_type {0}")]
    UnexpectedFitFieldBaseType(FitFieldFitBaseType),
    #[error("nom parsing error: {0}")]
    NomParsingError(String),
}

impl<T> From<nom::Err<T>> for FitParseError
where
    T: Debug,
{
    fn from(ne: nom::Err<T>) -> FitParseError {
        FitParseError::NomParsingError(format!("{:?}", ne))
    }
}

impl From<FitParseError> for std::fmt::Error {
    fn from(_t: FitParseError) -> Self {
        panic!("lol")
    }
}

#[allow(dead_code)]
pub(crate) fn parse_incomplete<E>(pi: usize) -> FitParseError {
    FitParseError::ParseIncomplete(pi)
}

#[allow(dead_code)]
pub(crate) fn parse_incomplete_unknown() -> FitParseError {
    FitParseError::ParseIncompleteUnknown
}

#[allow(dead_code)]
pub(crate) fn parse_invalid_field_value() -> FitParseError {
    let bt = Backtrace::force_capture();
    FitParseError::ParseInvalidFieldValue { backtrace: bt }
}

#[allow(dead_code)]
pub(crate) fn parse_error<T: AsRef<str>>(pe: T) -> FitParseError {
    FitParseError::ParseError(pe.as_ref().to_string())
}

#[allow(dead_code)]
pub(crate) fn parse_zero() -> FitParseError {
    FitParseError::ParseZero
}

#[allow(dead_code)]
pub(crate) fn unsupported_relative_timestamp() -> FitParseError {
    FitParseError::UnsupportedRelativetimestamp
}

#[allow(dead_code)]
pub(crate) fn invalid_field_value<T: AsRef<str>>(fv: T) -> FitParseError {
    FitParseError::InvalidFieldValue(fv.as_ref().to_string())
}

#[allow(dead_code)]
pub(crate) fn invalid_local_mesg_num<T: AsRef<str>>(ilmn: T) -> FitParseError {
    FitParseError::InvalidLocalMesgNum(ilmn.as_ref().to_string())
}

#[allow(dead_code)]
pub(crate) fn timestamp_base_not_set() -> FitParseError {
    FitParseError::TimestampBaseNotSet
}

#[allow(dead_code)]
pub(crate) fn timezone_offset_not_set() -> FitParseError {
    FitParseError::TimezoneOffsetNotSet
}

#[allow(dead_code)]
pub(crate) fn wrong_header_type() -> FitParseError {
    FitParseError::WrongHeaderType
}

#[allow(dead_code)]
pub(crate) fn unknown_error() -> FitParseError {
    FitParseError::UnknownError
}

#[allow(dead_code)]
pub(crate) fn timestamp_not_set_on_message() -> FitParseError {
    FitParseError::TimestampNotSetOnMessage
}

#[allow(dead_code)]
pub(crate) fn missing_timestamp_field() -> FitParseError {
    FitParseError::MissingTimestampField
}

#[allow(dead_code)]
pub(crate) fn invalid_field_number(ifn: u8) -> FitParseError {
    FitParseError::InvalidFieldNumber(ifn)
}

#[allow(dead_code)]
pub fn message_parse_failed(
    message_name: String,
    definition_message: Arc<FitDefinitionMessage>,
    bytes: Vec<u8>,
    source: FitParseError,
) -> FitParseError {
    FitParseError::MessageParseFailed {
        message_name,
        definition_message,
        bytes,
        source: Box::new(source),
    }
}

#[allow(dead_code)]
pub(crate) fn bad_adjusted_value_call() -> FitParseError {
    FitParseError::BadAdjustedValueCall
}

#[allow(dead_code)]
pub(crate) fn bad_basic_value_call() -> FitParseError {
    FitParseError::BadBasicValueCall
}

#[allow(dead_code)]
pub(crate) fn bad_fit_base_type_value_call() -> FitParseError {
    FitParseError::BadFitBaseTypeValueCall
}

#[allow(dead_code)]
pub(crate) fn missing_fit_base_type() -> FitParseError {
    FitParseError::MissingFitBaseType
}

#[allow(dead_code)]
pub(crate) fn parse_unknown_base_value() -> FitParseError {
    FitParseError::ParseUnknownBaseValue
}

#[allow(dead_code)]
pub(crate) fn developer_data_definition_not_found(dddn: u8) -> FitParseError {
    let bt = Backtrace::force_capture();
    FitParseError::DeveloperDataDefinitionNotFound {
        developer_data_id: dddn,
        backtrace: bt,
    }
}

#[allow(dead_code)]
pub(crate) fn developer_field_description_not_found(dfn: u8) -> FitParseError {
    FitParseError::DeveloperFieldDescriptionNotFound(dfn)
}

#[allow(dead_code)]
pub(crate) fn insufficient_data_for_shift() -> FitParseError {
    FitParseError::InsufficientDataForShift
}

#[allow(dead_code)]
pub(crate) fn incorrect_shift_input() -> FitParseError {
    FitParseError::IncorrectShiftInput
}

#[allow(dead_code)]
pub(crate) fn field_definition_number_not_found(fdn: u8) -> FitParseError {
    FitParseError::FieldDefinitionNumberNotFound(fdn)
}

#[allow(dead_code)]
pub(crate) fn bad_subfield_field_number(bn: u8) -> FitParseError {
    FitParseError::BadSubfieldFieldNumber(bn)
}

#[allow(dead_code)]
pub(crate) fn invalid_fit_base_type_parse() -> FitParseError {
    FitParseError::InvalidFitBaseTypeParse
}

#[allow(dead_code)]
pub(crate) fn fit_file_too_large(max_allowed: usize, attempted: usize) -> FitParseError {
    FitParseError::FitFileTooLarge(max_allowed, attempted)
}

#[allow(dead_code)]
pub(crate) fn hr_message_timestamp() -> FitParseError {
    FitParseError::HrMessageTimestamp
}

#[allow(dead_code)]
pub(crate) fn field_mfg_range_min() -> FitParseError {
    FitParseError::FieldMfgRangeMin
}

#[allow(dead_code)]
pub(crate) fn field_mfg_range_max() -> FitParseError {
    FitParseError::FieldMfgRangeMax
}

#[allow(dead_code)]
pub(crate) fn field_invalid_value() -> FitParseError {
    FitParseError::FieldInvalidValue
}

#[allow(dead_code)]
pub(crate) fn field_unknown_to_sdk() -> FitParseError {
    FitParseError::FieldUnknownToSdk
}

#[allow(dead_code)]
pub(crate) fn sport_message_not_present() -> FitParseError {
    FitParseError::SportMessageNotPresent
}

pub fn unexpected_fit_field_base_type(unexpected: FitFieldFitBaseType) -> FitParseError {
    FitParseError::UnexpectedFitFieldBaseType(unexpected)
}

pub fn nom_parsing_error(e: String) -> FitParseError {
    FitParseError::NomParsingError(e)
}
