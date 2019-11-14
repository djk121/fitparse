use chrono::{DateTime, Duration, FixedOffset, TimeZone, UTC};
use errors::{Error, Result};
use fitparsers::{parse_date_time, parse_uint32};
use nom::Endianness;
use std::cmp::Ordering;
use std::mem::transmute;

#[macro_export]
macro_rules! fmt_unknown_fields {
    ($s:ident, $f:ident) => {
        if $s.unknown_fields.len() > 0 {
            for (field_number, field_value) in $s.unknown_fields.iter() {
                writeln!(
                    $f,
                    "  {: >28}: {}",
                    format!("unknown_{}", field_number),
                    field_value
                )?;
            }
        }
    };
}

#[macro_export]
macro_rules! fmt_developer_fields {
    ($s:ident, $f:ident) => {
        if $s.developer_fields.len() > 0 {
            for developer_field in &$s.developer_fields {
                if let Some(field_names) = &developer_field.field_description.field_name.value {
                    if let Some(name) = &field_names[0] {
                        write!($f, "  {: >28}: ", name)?;
                    }
                }
                write!($f, "{}", developer_field.value)?;
                if let Some(field_units) = &developer_field.field_description.units.value {
                    if let Some(units) = &field_units[0] {
                        write!($f, " [{}]", units)?;
                    }
                }
                writeln!($f)?;
            }
        }
    };
}

#[macro_export]
macro_rules! fmt_raw_bytes {
    ($s:ident, $f:ident) => {{
        write!($f, "  {: >28}: [", "raw_bytes")?;
        for i in 0..$s.raw_bytes.len() - 1 {
            write!($f, "{:08b}", $s.raw_bytes[i])?;
            if i < $s.raw_bytes.len() - 1 {
                write!($f, ",")?;
            }
        }
        writeln!($f, "]")?;
    }};
}

#[macro_export]
macro_rules! fmt_message_field {
    ($thing:expr, $thingname:expr, $f:ident) => {
        if let Some(v) = &$thing.value {
            write!($f, "  {: >28}: {:?}", $thingname, v)?;
            if $thing.units.len() > 0 {
                write!($f, " [{}]", &$thing.units)?;
            }
            writeln!($f)?;
        }
    };
}

#[derive(Copy, Clone, Debug)]
pub struct FitFieldDateTime {
    seconds_since_garmin_epoch: u32,
    rust_time: DateTime<UTC>,
}

impl FitFieldDateTime {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<FitFieldDateTime> {
        let (utc_dt, garmin_epoch_offset) = parse_date_time(input, endianness)?;
        Ok(FitFieldDateTime {
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: utc_dt,
        })
    }

    #[allow(dead_code)]
    pub fn new_from_compressed_timestamp(&self, offset_secs: u8) -> Result<FitFieldDateTime> {
        let last_5_existing = {
            let bytes: [u8; 4] = unsafe { transmute(self.seconds_since_garmin_epoch.to_be()) };
            bytes[3] & 0x0000001F
        };
        let last_5_offset = offset_secs & 0x0000001F;

        let new_epoch_offset = match last_5_existing.cmp(&last_5_offset) {
            Ordering::Equal => self.seconds_since_garmin_epoch,
            Ordering::Greater => {
                (self.seconds_since_garmin_epoch & 0b11111111_11111111_11111111_11100000)
                    + last_5_offset as u32
                    + 0x20
            }
            Ordering::Less => {
                (self.seconds_since_garmin_epoch & 0b11111111_11111111_11111111_11100000)
                    + last_5_offset as u32
            }
        };

        let bytes: [u8; 4] = unsafe { transmute(new_epoch_offset.to_be()) };

        let result = FitFieldDateTime::parse(&bytes, Endianness::Big)?;
        Ok(result)
    }
}

#[derive(Debug)]
pub struct FitFieldLocalDateTime {
    seconds_since_garmin_epoch: u32,
    rust_time: DateTime<FixedOffset>,
}

impl FitFieldLocalDateTime {
    pub fn parse(
        input: &[u8],
        endianness: Endianness,
        _offset_secs: f64,
    ) -> Result<FitFieldLocalDateTime> {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let result = parse_uint32(input, endianness)?;
        let garmin_epoch_offset = match result {
            Some(geo) => geo,
            None => return Err(Error::invalid_fit_base_type_parse()),
        };
        let local_dt = FixedOffset::east(_offset_secs as i32).timestamp(
            (garmin_epoch + Duration::seconds(garmin_epoch_offset.into())).timestamp(),
            0, // nanosecs
        );

        Ok(FitFieldLocalDateTime {
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: local_dt,
        })
    }
}
