use failure::{Backtrace, Context, Fail};
use std::fmt;
use std::result;

pub type Result<T> = result::Result<T, Error>;

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

        }
    }
}

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
