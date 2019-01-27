
use std::fmt;
use std::result;
use failure::{Backtrace, Context, Fail};

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
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
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
    MissingFitBaseType,
    ParseUnknownBaseValue,
    DeveloperDataDefinitionNotFound(u8),
    DeveloperFieldDescriptionNotFound(u8),
    InsufficientDataForShift,
    IncorrectShiftInput,
    FieldDefinitionNumberNotFound(u8),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::ParseIncomplete(ref pi) => {
                write!(f, "insufficient data to finish parse, needed {:?} bytes", pi)
            },
            ErrorKind::ParseIncompleteUnknown => {
                write!(f, "insufficient data to finish parse, unknown amount needed")
            },
            ErrorKind::ParseError(ref pe) => {
                write!(f, "error during parse: {:?}", pe)
            },
            ErrorKind::ParseZero => {
                write!(f, "zero value found for type requiring non-zero")
            },
            ErrorKind::UnsupportedRelativetimestamp => {
                write!(f, "timestamp relative to device power-on not supported")
            },
            ErrorKind::InvalidFieldValue(ref fv) => {
                write!(f, "invalid value supplied for Field: {:?}", fv)
            },
            ErrorKind::InvalidLocalMesgNum(ref ilmn) => {
                write!(f, "missing local -> global mesg_num mapping for local mesg num {:?}", ilmn)
            },
            ErrorKind::TimestampBaseNotSet => {
                write!(f, "timestamp requested from ParsingState, but one hasn't been set")
            },
            ErrorKind::TimezoneOffsetNotSet => {
                write!(f, "timezone offset requested from ParsingState, but one hasn't been set")
            },
            ErrorKind::WrongHeaderType => {
                write!(f, "incorrect header type supplied")
            },
            ErrorKind::UnknownError => {
                write!(f, "unknown error")
            },
            ErrorKind::TimestampNotSetOnMessage => {
                write!(f, "get_timestamp called for message which doesn't have timestamp set")
            },
            ErrorKind::MissingTimestampField => {
                write!(f, "message with timestamp field ended parsing without timestamp being set")
            },
            ErrorKind::InvalidFieldNumber(ref ifn) => {
                write!(f, "attempt to parse field number not present in defintion message: {:?}", ifn)
            },
            ErrorKind::MessageParseFailed(ref fm) => {
                write!(f, "MessageParseFailed: {:?}", fm)
            },
            ErrorKind::MissingFitBaseType => {
                write!(f, "fit_base_type_id not supplied; can't parse developer data")
            },
            ErrorKind::ParseUnknownBaseValue => {
                write!(f, "attempt to parse uncovered base value variant")
            },
            ErrorKind::DeveloperDataDefinitionNotFound(ref dddn) => {
                write!(f, "developer data definition number not found: {:?}", dddn)
            },
            ErrorKind::DeveloperFieldDescriptionNotFound(ref dfn) => {
                write!(f, "developer field number not found: {:?}", dfn)
            },
            ErrorKind::InsufficientDataForShift => {
                write!(f, "insufficient data for shift")
            },
            ErrorKind::IncorrectShiftInput => {
                write!(f, "bad inputs for shift operation")
            },
            ErrorKind::FieldDefinitionNumberNotFound(ref fdn) => {
                write!(f, "field definition number not found: {:?}", fdn)
            },

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
