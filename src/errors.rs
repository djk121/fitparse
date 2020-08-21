
use thiserror::Error;
//use std::error::Error;

pub type Result<T> = anyhow::Result<T, FitParseError>;

#[derive(Error, Debug)]
pub enum FitParseError {
    #[error("insufficient data to finish parse, needed {0} bytes")]
    ParseIncomplete(usize),
    #[error("insufficient data to finish parse, unknown amount needed")]
    ParseIncompleteUnknown,
    #[error("invalid field value for field")]
    ParseInvalidFieldValue,
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
    #[error("MessageParseFailed: {0}")]
    MessageParseFailed(String),
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
    #[error("developer data definition number not found: {0}")]
    DeveloperDataDefinitionNotFound(u8),
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
    UnexpectedFitFieldBaseType(usize)
}

impl From<FitParseError> for std::fmt::Error {
    fn from(_t: FitParseError) -> Self {
        panic!("lol")
    }
} 



#[allow(dead_code)]
pub(crate) fn parse_incomplete(pi: usize) -> FitParseError {
    FitParseError::ParseIncomplete(pi)
}

#[allow(dead_code)]
pub(crate) fn parse_incomplete_unknown() -> FitParseError {
    FitParseError::ParseIncompleteUnknown
}

#[allow(dead_code)]
pub(crate) fn parse_invalid_field_value() -> FitParseError {
    FitParseError::ParseInvalidFieldValue
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
pub(crate) fn message_parse_failed<T: AsRef<str>>(fm: T) -> FitParseError {
    FitParseError::MessageParseFailed(fm.as_ref().to_string())
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
    FitParseError::DeveloperDataDefinitionNotFound(dddn)
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

pub fn unexpected_fit_field_base_type(unexpected: usize) -> FitParseError {
    FitParseError::UnexpectedFitFieldBaseType(unexpected)
}

/*
#[derive(Debug)]
pub struct Error {
    ctx: Context<ErrorKind>,
}


impl Error {
    /// Return the kind of this error.
    pub fn kind(&self) -> &ErrorKind {
        self.ctx.get_context()
    }

    #[allow(dead_code)]
    pub(crate) fn parse_incomplete(pi: usize) -> Error {
        Error::from(ErrorKind::ParseIncomplete(pi))
    }

    #[allow(dead_code)]
    pub(crate) fn parse_incomplete_unknown() -> Error {
        Error::from(ErrorKind::ParseIncompleteUnknown)
    }

    #[allow(dead_code)]
    pub(crate) fn parse_invalid_field_value() -> Error {
        Error::from(ErrorKind::ParseInvalidFieldValue)
    }

    #[allow(dead_code)]
    pub(crate) fn parse_error<T: AsRef<str>>(pe: T) -> Error {
        Error::from(ErrorKind::ParseError(pe.as_ref().to_string()))
    }

    #[allow(dead_code)]
    pub(crate) fn parse_zero() -> Error {
        Error::from(ErrorKind::ParseZero)
    }

    #[allow(dead_code)]
    pub(crate) fn unsupported_relative_timestamp() -> Error {
        Error::from(ErrorKind::UnsupportedRelativetimestamp)
    }

    #[allow(dead_code)]
    pub(crate) fn invalid_field_value<T: AsRef<str>>(fv: T) -> Error {
        Error::from(ErrorKind::InvalidFieldValue(fv.as_ref().to_string()))
    }

    #[allow(dead_code)]
    pub(crate) fn invalid_local_mesg_num<T: AsRef<str>>(ilmn: T) -> Error {
        Error::from(ErrorKind::InvalidLocalMesgNum(ilmn.as_ref().to_string()))
    }

    #[allow(dead_code)]
    pub(crate) fn timestamp_base_not_set() -> Error {
        Error::from(ErrorKind::TimestampBaseNotSet)
    }

    #[allow(dead_code)]
    pub(crate) fn timezone_offset_not_set() -> Error {
        Error::from(ErrorKind::TimezoneOffsetNotSet)
    }

    #[allow(dead_code)]
    pub(crate) fn wrong_header_type() -> Error {
        Error::from(ErrorKind::WrongHeaderType)
    }

    #[allow(dead_code)]
    pub(crate) fn unknown_error() -> Error {
        Error::from(ErrorKind::UnknownError)
    }

    #[allow(dead_code)]
    pub(crate) fn timestamp_not_set_on_message() -> Error {
        Error::from(ErrorKind::TimestampNotSetOnMessage)
    }

    #[allow(dead_code)]
    pub(crate) fn missing_timestamp_field() -> Error {
        Error::from(ErrorKind::MissingTimestampField)
    }

    #[allow(dead_code)]
    pub(crate) fn invalid_field_number(ifn: u8) -> Error {
        Error::from(ErrorKind::InvalidFieldNumber(ifn))
    }

    #[allow(dead_code)]
    pub(crate) fn message_parse_failed<T: AsRef<str>>(fm: T) -> Error {
        Error::from(ErrorKind::MessageParseFailed(fm.as_ref().to_string()))
    }

    #[allow(dead_code)]
    pub(crate) fn bad_adjusted_value_call() -> Error {
        Error::from(ErrorKind::BadAdjustedValueCall)
    }

    #[allow(dead_code)]
    pub(crate) fn bad_basic_value_call() -> Error {
        Error::from(ErrorKind::BadBasicValueCall)
    }

    #[allow(dead_code)]
    pub(crate) fn bad_fit_base_type_value_call() -> Error {
        Error::from(ErrorKind::BadFitBaseTypeValueCall)
    }

    #[allow(dead_code)]
    pub(crate) fn missing_fit_base_type() -> Error {
        Error::from(ErrorKind::MissingFitBaseType)
    }

    #[allow(dead_code)]
    pub(crate) fn parse_unknown_base_value() -> Error {
        Error::from(ErrorKind::ParseUnknownBaseValue)
    }

    #[allow(dead_code)]
    pub(crate) fn developer_data_definition_not_found(dddn: u8) -> Error {
        Error::from(ErrorKind::DeveloperDataDefinitionNotFound(dddn))
    }

    #[allow(dead_code)]
    pub(crate) fn developer_field_description_not_found(dfn: u8) -> Error {
        Error::from(ErrorKind::DeveloperFieldDescriptionNotFound(dfn))
    }

    #[allow(dead_code)]
    pub(crate) fn insufficient_data_for_shift() -> Error {
        Error::from(ErrorKind::InsufficientDataForShift)
    }

    #[allow(dead_code)]
    pub(crate) fn incorrect_shift_input() -> Error {
        Error::from(ErrorKind::IncorrectShiftInput)
    }

    #[allow(dead_code)]
    pub(crate) fn field_definition_number_not_found(fdn: u8) -> Error {
        Error::from(ErrorKind::FieldDefinitionNumberNotFound(fdn))
    }

    #[allow(dead_code)]
    pub(crate) fn bad_subfield_field_number(bn: u8) -> Error {
        Error::from(ErrorKind::BadSubfieldFieldNumber(bn))
    }

    #[allow(dead_code)]
    pub(crate) fn invalid_fit_base_type_parse() -> Error {
        Error::from(ErrorKind::InvalidFitBaseTypeParse)
    }

    #[allow(dead_code)]
    pub(crate) fn fit_file_too_large(max_allowed: usize, attempted: usize) -> Error {
        Error::from(ErrorKind::FitFileTooLarge((max_allowed, attempted)))
    }

    #[allow(dead_code)]
    pub(crate) fn hr_message_timestamp() -> Error {
        Error::from(ErrorKind::HrMessageTimestamp)
    }

    #[allow(dead_code)]
    pub(crate) fn field_mfg_range_min() -> Error {
        Error::from(ErrorKind::FieldMfgRangeMin)
    }

    #[allow(dead_code)]
    pub(crate) fn field_mfg_range_max() -> Error {
        Error::from(ErrorKind::FieldMfgRangeMax)
    }

    #[allow(dead_code)]
    pub(crate) fn field_invalid_value() -> Error {
        Error::from(ErrorKind::FieldInvalidValue)
    }

    #[allow(dead_code)]
    pub(crate) fn field_unknown_to_sdk() -> Error {
        Error::from(ErrorKind::FieldUnknownToSdk)
    }

    #[allow(dead_code)]
    pub(crate) fn sport_message_not_present() -> Error {
        Error::from(ErrorKind::SportMessageNotPresent)
    }
}

impl Fail for Error {
    fn cause(&self) -> Option<&dyn Fail> {
        self.ctx.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.ctx.backtrace()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ctx.fmt(f)
    }
}

/// The specific kind of error that can occur.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    ParseIncomplete(usize),
    ParseIncompleteUnknown,
    ParseInvalidFieldValue,
    ParseError(String),
    ParseZero,
    UnsupportedRelativetimestamp,
    InvalidFieldValue(String),
    InvalidLocalMesgNum(String),
    TimestampBaseNotSet,
    TimezoneOffsetNotSet,
    WrongHeaderType,
    UnknownError,
    TimestampNotSetOnMessage,
    MissingTimestampField,
    InvalidFieldNumber(u8),
    MessageParseFailed(String),
    BadAdjustedValueCall,
    BadBasicValueCall,
    BadFitBaseTypeValueCall,
    MissingFitBaseType,
    ParseUnknownBaseValue,
    DeveloperDataDefinitionNotFound(u8),
    DeveloperFieldDescriptionNotFound(u8),
    InsufficientDataForShift,
    IncorrectShiftInput,
    FieldDefinitionNumberNotFound(u8),
    BadSubfieldFieldNumber(u8),
    InvalidFitBaseTypeParse,
    FitFileTooLarge((usize, usize)),
    HrMessageTimestamp,
    FieldMfgRangeMin,
    FieldMfgRangeMax,
    FieldUnknownToSdk,
    FieldInvalidValue,
    SportMessageNotPresent
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::ParseIncomplete(ref pi) => write!(
                f,
                "insufficient data to finish parse, needed {:?} bytes",
                pi,
            ),
            ErrorKind::ParseIncompleteUnknown => write!(
                f,
                "insufficient data to finish parse, unknown amount needed",
            ),
            ErrorKind::ParseInvalidFieldValue => write!(f, "invalid field value for field",),
            ErrorKind::ParseError(ref pe) => write!(f, "error during parse: {:?}", pe),
            ErrorKind::ParseZero => write!(f, "zero value found for type requiring non-zero",),
            ErrorKind::UnsupportedRelativetimestamp => {
                write!(f, "timestamp relative to device power-on not supported",)
            }
            ErrorKind::InvalidFieldValue(ref fv) => {
                write!(f, "invalid value supplied for Field: {:?}", fv,)
            }
            ErrorKind::InvalidLocalMesgNum(ref ilmn) => write!(
                f,
                "missing local -> global mesg_num mapping for local mesg num {:?}",
                ilmn,
            ),
            ErrorKind::TimestampBaseNotSet => write!(
                f,
                "timestamp requested from ParsingState, but one hasn't been set",
            ),
            ErrorKind::TimezoneOffsetNotSet => write!(
                f,
                "timezone offset requested from ParsingState, but one hasn't been set",
            ),
            ErrorKind::WrongHeaderType => write!(f, "incorrect header type supplied"),
            ErrorKind::UnknownError => write!(f, "unknown error"),
            ErrorKind::TimestampNotSetOnMessage => write!(
                f,
                "get_timestamp called for message which doesn't have timestamp set",
            ),
            ErrorKind::MissingTimestampField => write!(
                f,
                "message with timestamp field ended parsing without timestamp being set"
            ),
            ErrorKind::InvalidFieldNumber(ref ifn) => write!(
                f,
                "attempt to parse field number not present in defintion message: {:?}",
                ifn,
            ),
            ErrorKind::MessageParseFailed(ref fm) => write!(f, "MessageParseFailed: {:?}", fm),
            ErrorKind::BadAdjustedValueCall => write!(
                f,
                "bad call to as_64 or as_64_vec: not initialized or wrong variant"
            ),
            ErrorKind::BadBasicValueCall => write!(
                f,
                "bad call to as_single or as_vec: not initialized or wrong variant"
            ),
            ErrorKind::BadFitBaseTypeValueCall => write!(f, "bad call to value(): not initialized"),
            ErrorKind::MissingFitBaseType => write!(
                f,
                "fit_base_type_id not supplied; can't parse developer data"
            ),
            ErrorKind::ParseUnknownBaseValue => {
                write!(f, "attempt to parse uncovered base value variant")
            }
            ErrorKind::DeveloperDataDefinitionNotFound(ref dddn) => {
                write!(f, "developer data definition number not found: {:?}", dddn)
            }
            ErrorKind::DeveloperFieldDescriptionNotFound(ref dfn) => {
                write!(f, "developer field number not found: {:?}", dfn)
            }
            ErrorKind::InsufficientDataForShift => write!(f, "insufficient data for shift"),
            ErrorKind::IncorrectShiftInput => write!(f, "bad inputs for shift operation"),
            ErrorKind::FieldDefinitionNumberNotFound(ref fdn) => {
                write!(f, "field definition number not found: {:?}", fdn)
            }
            ErrorKind::BadSubfieldFieldNumber(ref bn) => {
                write!(f, "asked to parse bad subfield field number: {:?}", bn)
            }
            ErrorKind::InvalidFitBaseTypeParse => write!(f, "invalid fit base type parse result"),
            ErrorKind::FitFileTooLarge(ref fftl) => write!(
                f,
                "fit file too large, max_allowed: {:?}, attempted: {:?}",
                fftl.0, fftl.1
            ),
            ErrorKind::HrMessageTimestamp => write!(f, "Error parsing timestamps in Hr message"),
            ErrorKind::FieldMfgRangeMin => write!(f, "Field: Manufacture Range Min"),
            ErrorKind::FieldMfgRangeMax => write!(f, "Field: Manufacture Range Max"),
            ErrorKind::FieldInvalidValue => write!(f, "Field: Invalid Value"),
            ErrorKind::FieldUnknownToSdk => write!(f, "Field: Unknown to SDK"),
            ErrorKind::SportMessageNotPresent => write!(f, "no FitMessageSport found in Fit File"),
        }
    }
}
*/

/*
impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error::from(Context::new(kind))
    }
}

impl From<Context<ErrorKind>> for Error {
    fn from(ctx: Context<ErrorKind>) -> Error {
        Error { ctx }
    }
}

impl From<Error> for fmt::Error {
    fn from(_e: Error) -> fmt::Error {
        return fmt::Error;
    }
}
*/
