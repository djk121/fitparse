use chrono::{DateTime, Duration, FixedOffset, TimeZone, UTC};
use errors::{Error, Result};
use fitparsers::{parse_date_time, parse_uint32};
use nom::Endianness;
use std::cmp::Ordering;
use std::mem::transmute;

#[macro_export]
macro_rules! field_parser_base_type {
    ("string", $bytes:expr, $field:expr) => {
        parse_string(&$bytes[0..$field.field_size], $field.field_size);
    };
    ("byte", $bytes:expr, $field:expr) => {
        parse_byte(&$bytes[0..$field.field_size], $field.field_size);
    };
    ("bool", $bytes:expr, $field:expr) => {
        parse_bool(&$bytes[0..$field.field_size]);
    };
    ("enum", $bytes:expr, $field:expr) => {
        parse_enum(&$bytes[0..$field.field_size]);
    };
    ("uint8", $bytes:expr, $field:expr) => {
        parse_uint8(&$bytes[0..$field.field_size]);
    };
    ("uint8z", $bytes:expr, $field:expr) => {
        parse_uint8z(&$bytes[0..$field.field_size]);
    };
    ("sint8", $bytes:expr, $field:expr) => {
        parse_sint8(&$bytes[0..$field.field_size]);
    };
    ("uint16", $bytes:expr, $field:expr, $message:expr) => {
        parse_uint16(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("uint16z", $bytes:expr, $field:expr, $message:expr) => {
        parse_uint16z(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("sint16", $bytes:expr, $field:expr, $message:expr) => {
        parse_sint16(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("uint32", $bytes:expr, $field:expr, $message:expr) => {
        parse_uint32(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("uint32z", $bytes:expr, $field:expr, $message:expr) => {
        parse_uint32z(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("sint32", $bytes:expr, $field:expr, $message:expr) => {
        parse_sint32(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("float32", $bytes:expr, $field:expr, $message:expr) => {
        parse_float32(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("uint64", $bytes:expr, $field:expr, $message:expr) => {
        parse_uint64(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("uint64z", $bytes:expr, $field:expr, $message:expr) => {
        parse_uint64z(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("sint64", $bytes:expr, $field:expr, $message:expr) => {
        parse_sint64(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
    ("float64", $bytes:expr, $field:expr, $message:expr) => {
        parse_float32(
            &$bytes[0..$field.field_size],
            $message.definition_message.endianness,
        );
    };
}

#[macro_export]
macro_rules! field_parser_fit_type {
    ($field:ty, $bytes:expr, $f:expr, $message:expr, $tz_offset:expr) => {
        <$field>::parse(
            &$bytes[0..$f.field_size],
            $message.definition_message.endianness,
            $tz_offset,
        );
    };
    ($field:ty, $bytes:expr, $f:expr, $message:expr) => {
        <$field>::parse(
            &$bytes[0..$f.field_size],
            $message.definition_message.endianness,
        );
    };

    ($field:ty, $bytes:expr, $f:expr) => {
        <$field>::parse(&$bytes[0..$f.field_size]);
    };
}

#[macro_export]
macro_rules! scale_and_offset_parse_assignment {
    ("vec", $val:expr, $message_field:expr, $scale:expr, 0) => {
        $message_field.value = Some(
            $val.into_iter()
                .filter_map(|x| x)
                .map(|i| Some(i as f64 / $scale as f64))
                .collect(),
        );
    };
    ("vec", $val:expr, $message_field:expr, $scale:expr, $offset:expr) => {
        $message_field.value = Some(
            $val.into_iter()
                .filter_map(|x| x)
                .map(|i| Some(i as f64 / $scale as f64 - ($offset as f64)))
                .collect(),
        );
    };
    ($val:expr, $message_field:expr, $scale:expr, 0) => {
        match $val {
            Some(result) => $message_field.value = Some(result as f64 / $scale as f64),
            None => $message_field.value = None,
        }
    };
    ($val:expr, $message_field:expr, $scale:expr, $offset:expr) => {
        match $val {
            Some(result) => {
                $message_field.value = Some(result as f64 / $scale as f64 - ($offset as f64))
            }
            None => $message_field.value = None,
        }
    };
}

#[macro_export]
macro_rules! scale_and_offset_vec_parse_assignment {
    ($val:expr, $message_field:expr, $scale:expr, $offset:expr) => {
        $message_field.value = Some(
            val.into_iter()
                .filter_map(|x| x)
                .map(|i| Some(i as f64 / $scale as f64))
                .collect(),
        );
    };
    ($val:expr, $message_field:expr, $scale:expr) => {
        $message_field.value = Some(
            val.into_iter()
                .filter_map(|x| x)
                .map(|i| Some(i as f64 / $scale as f64 - ($offset as f64)))
                .collect(),
        );
    };
}

#[macro_export]
macro_rules! deg_parse_assignment {
    ($val:expr, $message_field:expr) => {
        match $val {
            Some(result) => {
                $message_field.value = Some((result as f64) * (180.0_f64 / 2_f64.powf(31.0)))
            }
            None => $message_field.value = None,
        }
    };
}

#[macro_export]
macro_rules! main_parse_message {
    ($input:expr, $message:expr, $parsing_state:expr, $output_type:ty) => {{
        let inp = &$input[..($message.definition_message.message_size)];
        if $parsing_state.retain_bytes == true {
            $message
                .raw_bytes
                .resize($message.definition_message.message_size, 0);
            $message.raw_bytes.copy_from_slice(inp);
        }
        let tz_offset = $parsing_state.get_timezone_offset();
        let o = match <$output_type>::parse_internal(&mut $message, $input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string =
                    String::from(concat!("Error parsing ", stringify!($output_type), ":"));
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string));
            }
        };

        o
    }};
}

#[macro_export]
macro_rules! parse_subfields {
    ($message:expr, $parsing_state:expr, $output_type:ty) => {
        match <$output_type>::parse_subfields(&mut $message, $parsing_state.get_timezone_offset()) {
            Err(e) => {
                let mut err_string = String::from(concat!(
                    "Error parsing subfields for ",
                    stringify!($output_type),
                    ":"
                ));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string));
            }
            Ok(_) => (),
        }
    };
}

#[macro_export]
macro_rules! parse_developer_fields {
    ($inp2:expr, $message:expr, $parsing_state:expr) => {
        for dev_field in &$message.definition_message.developer_field_definitions {
            let dev_data_definition =
                $parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;
            let field_description =
                dev_data_definition.get_field_description(dev_field.definition_number)?;
            let (dd, outp) = FitFieldDeveloperData::parse(
                $inp2,
                field_description.clone(),
                $message.definition_message.endianness,
                dev_field.field_size,
            )?;
            $message.developer_fields.push(dd);
            $inp2 = outp;
        }
    };
}

#[macro_export]
macro_rules! parsing_state_set_timestamp {
    ($message:expr, $timestamp:expr, $parsing_state:expr) => {
        match $timestamp {
            Some(ts) => {
                $message.timestamp.value = Some(ts);
            }
            None => match $message.timestamp.value {
                Some(ts) => {
                    $parsing_state.set_last_timestamp(ts);
                }
                None => return Err(Error::missing_timestamp_field()),
            },
        }
    };
}

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
        offset_secs: f64,
    ) -> Result<FitFieldLocalDateTime> {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let result = parse_uint32(input, endianness)?;
        let garmin_epoch_offset = match result {
            Some(geo) => geo,
            None => return Err(Error::invalid_fit_base_type_parse()),
        };
        let local_dt = FixedOffset::east(offset_secs as i32).timestamp(
            (garmin_epoch + Duration::seconds(garmin_epoch_offset.into())).timestamp(),
            0, // nanosecs
        );

        Ok(FitFieldLocalDateTime {
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: local_dt,
        })
    }
}
