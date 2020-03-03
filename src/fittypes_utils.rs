use chrono::{DateTime, Duration, FixedOffset, TimeZone, UTC};
use errors::{Error, Result};
use fitparsers::{parse_date_time, parse_uint32};
use fitparsingstate::FitParsingState;
use nom::Endianness;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::mem::transmute;
use std::rc::Rc;
use subset_with_pad;
use FitBaseValue;
use FitDefinitionMessage;
use FitFieldDefinition;
use FitFieldDeveloperData;
use FitFieldFitBaseType;
use FitFieldParseable;
use {FitFieldBasicValue, FitFieldAdjustedValue};
use FitParseConfig;
use FitRecord;
use FitRecordHeader;
use {BasicValue, AdjustedValue};
use {FitUint8, FitUint16, FitUint32, FitFloat64};

#[macro_export]
macro_rules! vec_fit_field_parseable {
    ($name:ident) => {
        impl FitFieldParseable for Vec<$name> {
            fn parse(input: &[u8], parse_config: FitParseConfig) -> Result<Vec<$name>> {
                let mut num_to_parse = parse_config.num_in_field();
           
                let mut outp = input;
                let mut v = vec![];
        
                while num_to_parse > 0 {
                    let val = $name::parse(outp, parse_config)?;
                    outp = &outp[parse_config.base_type_size()..];
                    v.push(val);
                    num_to_parse = num_to_parse - 1;
                }
                Ok(v)
            }
        }
    }
}

#[macro_export]
macro_rules! fit_field_parse_instruction {
    ($fit_field_definition:expr) => {
        FitFieldParseInstruction {
            field_definition: $fit_field_definition,
            bit_range: None,
            scale: None,
            offset: None,
        }
    };
}

/*
#[macro_export]
macro_rules! fit_field_basic_value {
    ($units:expr) => {
        FitFieldValue {
            value: None,
            units: $units.to_string(),
            scale: None,
            offset: None,
            is_array: false,
            components: vec![],
        }
    };
    ($units:expr, $scale:expr) => {
        FitFieldValue {
            value: None,
            units: $units.to_string(),
            scale: Some($scale),
            offset: None,
            is_array: false,
            components: vec![],
        }
    };
    ($units:expr, $scale:expr, $offset:expr) => {
        FitFieldValue {
            value: None,
            units: $units.to_string(),
            scale: Some($scale),
            offset: Some($offset),
            is_array: false,
            components: vec![],
        }
    };
}
*/

#[macro_export]
macro_rules! parse_internal_field {
    ($components_bit_range:expr, $inp:expr, $field:expr, $message:expr, $message_field:expr, "base_type", $is_degrees:expr, $scale_and_offset:expr) => {
        match $components_bit_range {
            Some((bit_range_start, num_bits)) => {
                let bytes = subset_with_pad(
                    &$inp[0..$field.field_size],
                    bit_range_start,
                    num_bits,
                    $message.definition_message.endianness,
                )?;

                match field.is_array() {
                    true => {
                        let mut array_size = field.array_size();
                        let mut val = Vec::with_capacity(array_size);
                        let mut tempp = &bytes[..];
                        while array_size > 0 {
                            let v = field_parser_base_type!($field.base_type_name(), tempp, $field)?;
                            tempp = &tempp[field.base_type_size()..];
                            val.push(v);
                            array_size = array_size - 1
                        }
                        match $scale_and_offset {
                            true =>
                        }
                        $message_field.value = val;
                    },
                    false => {
                        saved_outp = &$inp[$field.field_size..];

                        let val = field_parser_base_type!($field.base_type_name(), $inp, $field, $message)?;

                        match $is_degrees {
                            true => deg_parse_assignment!(val, $message_field),
                            false => $message_field.value = val;
                        }
                    }
                }
            }
            None => {
                saved_outp = &$inp[$field.field_size..];
                match field.is_array() {
                    true => {
                        let mut array_size = field.array_size();
                        let mut val = Vec::with_capacity(array_size);
                        let mut tempp = inp;
                        while array_size > 0 {
                            let v = field_parser_base_type!($field.base_type_name(), tempp, $field)?;
                            tempp = &tempp[field.base_type_size()..];
                            val.push(v);
                            array_size = array_size - 1
                        }
                        $message_field.value = val;
                    },
                    false => {
                        saved_outp = &$inp[$field.field_size..];

                        let val = field_parser_base_type!($field.base_type_name(), $inp, $field, $message)?;

                        match $is_degrees {
                            true => deg_parse_assignment!(val, $message_field),
                            false => $message_field.value = val;
                        }
                    }
                }
            }
        }

        Ok(())
    };
}

#[macro_export]
macro_rules! field_parser_base_type {
    ("string", $bytes:expr, $parse_config:expr) => {
        parse_string(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("byte", $bytes:expr, $parse_config:expr) => {
        parse_byte(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("bool", $bytes:expr, $parse_config:expr) => {
        parse_bool(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("enum", $bytes:expr, $parse_config:expr) => {
        parse_enum(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint8", $bytes:expr, $parse_config:expr) => {
        parse_uint8(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint8z", $bytes:expr, $parse_config:expr) => {
        parse_uint8z(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("sint8", $bytes:expr, $parse_config:expr) => {
        parse_sint8(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint16", $bytes:expr, $parse_config:expr) => {
        parse_uint16(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint16z", $bytes:expr, $parse_config:expr) => {
        parse_uint16z(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("sint16", $bytes:expr, $parse_config:expr) => {
        parse_sint16(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint32", $bytes:expr, $parse_config:expr) => {
        parse_uint32(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint32z", $bytes:expr, $parse_config:expr) => {
        parse_uint32z(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("sint32", $bytes:expr, $parse_config:expr) => {
        parse_sint32(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("float32", $bytes:expr, $parse_config:expr) => {
        parse_float32(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint64", $bytes:expr, $parse_config:expr) => {
        parse_uint64(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("uint64z", $bytes:expr, $parse_config:expr) => {
        parse_uint64z(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("sint64", $bytes:expr, $parse_config:expr) => {
        parse_sint64(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
    ("float64", $bytes:expr, $parse_config:expr) => {
        parse_float32(&$bytes[0..$parse_config.field_size()], $parse_config);
    };
}

/*
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
*/

/*
#[macro_export]
macro_rules! field_parser_fit_type_fn {
    ($field:ty, $bytes:expr, $f:expr, $message:expr, $tz_offset:expr, $fn_name:expr) => {
        fn field_parser_$fn_name() -> Result<$fn_name> {
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
*/

#[macro_export]
macro_rules! field_parser_fit_type {
    ($field:ty, $bytes:expr, $parse_config:expr) => {
        <$field>::parse(&$bytes[0..$parse_config.size()], $parse_config);
    };
}

/*
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
*/

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
        //println!("parse_developer_fields, inp2: {:?}", $inp2);
        //println!("dev_fields: {:?}", &$message.definition_message.developer_field_definitions);
        for dev_field in &$message.definition_message.developer_field_definitions {
            let dev_data_definition =
                $parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;
            let field_description =
                dev_data_definition.get_field_description(dev_field.definition_number)?;
            //let parse_config = fit_parse_config!($message.definition_message.endianness, dev_field.field_size);
            //let parse_config = FitParseConfig{ endianness: Some($message.definition_message.endianness), size: Some(dev_field.field_size) };

            let base_type_num: u8 = match field_description.fit_base_type_id.get_single()? {
                FitFieldFitBaseType::Enum => 0,
                FitFieldFitBaseType::Sint8 => 1,
                FitFieldFitBaseType::Uint8 => 2,
                FitFieldFitBaseType::Sint16 => 131,
                FitFieldFitBaseType::Uint16 => 132,
                FitFieldFitBaseType::Sint32 => 133, 
                FitFieldFitBaseType::Uint32 => 134,
                FitFieldFitBaseType::String => 7, 
                FitFieldFitBaseType::Float32 => 136, 
                FitFieldFitBaseType::Float64 => 137,
                FitFieldFitBaseType::Uint8z => 10,
                FitFieldFitBaseType::Uint16z => 139,
                FitFieldFitBaseType::Uint32z => 140,
                FitFieldFitBaseType::Byte => 13,
                FitFieldFitBaseType::Sint64 => 142,
                FitFieldFitBaseType::Uint64 => 143,
                FitFieldFitBaseType::Uint64z => 144,
                _ => return Err(Error::unknown_error())
            };

            let def_num = <u8>::from(field_description.field_definition_number.get_single()?);

            let parse_config = FitParseConfig {
                field_definition:
                    Some(FitFieldDefinition {
                            definition_number: def_num,
                            field_size: dev_field.field_size,
                            base_type: base_type_num
                    }),
                endianness: Some($message.definition_message.endianness),
                tz_offset_secs: None,
                bit_range: None,
            };

            let dd = FitFieldDeveloperData::parse(
                $inp2,
                field_description.clone(),
                parse_config
            )?;
            $message.developer_fields.push(dd);
            // we can run out of input before all fields are consumed. according
            // to the spec, buffering with zero-padded fields is appropriate
            //println!("dev_field post parse, inp2.len(): {}", $inp2.len());
            if $inp2.len() < parse_config.field_size() {
                $inp2 = &$inp2[$inp2.len()..];
            } else {
                $inp2 = &$inp2[parse_config.field_size()..]; 
            }
                //$inp2 = outp;
        }
    };
}

#[macro_export]
macro_rules! parsing_state_set_timestamp {
    ($message:expr, $timestamp:expr, $parsing_state:expr) => {
        match $timestamp {
            Some(ts) => {
                $message.timestamp.value = BasicValue::Single(ts);
            }
            None => {
                let ts = $message.timestamp.get_single()?;
                $parsing_state.set_last_timestamp(ts);
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
                let field_names = &developer_field.field_description.field_name;
                let name = &field_names.get_vec()?[0];
                write!($f, "  {: >28}: ", name)?;
                write!($f, "{}", developer_field.value)?;
                let field_units = &developer_field.field_description.units;
                let units = &field_units.get_vec()?[0];
                write!($f, " [{}]", units)?;
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
macro_rules! fmt_message_field_basic {
    ($thing:expr, $thingname:expr, $f:ident) => {
        let val = match &$thing.value {
            BasicValue::NotYetParsedSingle | BasicValue::NotYetParsedVec => "not yet parsed".to_string(),
            BasicValue::Single(v) => format!("{:?}", v),
            BasicValue::Vec(v) => {
                let s = "[".to_string();
                for i in 0..v.len() - 1 {
                    s.push(format!("{}, ", v[i]));
                }
                s.push(format!("{}]", v[v.len()-1]));
                s
            }
        };
        write!($f, "  {: >28}: {:?}", $thingname, val)?;
    };
}

#[macro_export]
macro_rules! fmt_message_field_adjusted {
    ($thing:expr, $thingname:expr, $f:ident) => {
        let val = match &$thing.value {
            AdjustedValue::NotYetParsedSingle | AdjustedValue::NotYetParsedVec => "not yet parsed".to_string(),
            AdjustedValue::Single(v) => format!("{:?}", v),
            AdjustedValue::Vec(v) => {
                let s = "[".to_string();
                for i in 0..v.len() - 1 {
                    s.push(format!("{}, ", v[i]));
                }
                s.push(format!("{}]", v[v.len()-1]));
                s
            }
        };
        write!($f, "  {: >28}: {:?}", $thingname, val)?;
    };
}

#[macro_export]
macro_rules! fmt_message_field {
    ($thing:expr, $thingname:expr, false, $f:ident) => {
        writeln!($f, "  {: >28}: {}", $thingname, $thing)
    };
    ($thing:expr, $thingname:expr, true, $f:ident) => {
        writeln!($f, "  {: >28}: {}", $thingname, $thing)
    };
}
/*
#[macro_export]
macro_rules! fmt_message_field {
    ($thing:expr, $thingname:expr, false, $f:ident) => {
        let val = match &$thing.value {
            BasicValue::NotYetParsedSingle | BasicValue::NotYetParsedVec  => "not yet parsed".to_string(),
            BasicValue::Single(a) => format!("{}", a),
            BasicValue::Vec(v) => {
                let mut s = "[".to_string();
                for i in 0..v.len() - 1 {
                    let x = v[i];
                    let y = x.to_string();
                    s.push_str(&y);
                }
                let x = (&v[v.len()-1]).to_string();
                s.push_str(&format!("{}]", &(x.to_string())));
                s
            }
        };
        writeln!($f, "  {: >28}: {}", $thingname, &val)?
    };
    ($thing:expr, $thingname:expr, true, $f:ident) => {
        let val = match &$thing.value {
            AdjustedValue::NotYetParsedSingle | AdjustedValue::NotYetParsedVec => "not yet parsed".to_string(),
            AdjustedValue::Single(v) => format!("{}", v),
            AdjustedValue::Vec(v) => {
                let mut s = "[".to_string();
                for i in 0..v.len() - 1 {
                    s.push_str(&(&v[i]).to_string())
                    //s.push_str(&format!("{}, ", v[i]));
                }
                s.push_str(&format!("{}]", &(&v[v.len()-1]).to_string()));
                s
            }
        };
        writeln!($f, "  {: >28}: {}", $thingname, &val)?;        
    };
}
*/
/*
#[macro_export]
macro_rules! fmt_message_field {
    ($thing:expr, $thingname:expr, $f:ident) => {
        if let Some(v) = &$thing.get_option()? {
            write!($f, "  {: >28}: {:?}", $thingname, v)?;
            if $thing.units.len() > 0 {
                write!($f, " [{}]", &$thing.units)?;
            }
            writeln!($f)?;
        }
    };
}
*/

#[macro_export]
macro_rules! fmt_message_subfield {
    ($thing:expr, $thingname:expr, $f:ident) => {
        writeln!($f, "  {: >28}: {:?}", $thingname, $thing)?;
    };
}


#[derive(Copy, Clone, Debug)]
pub struct FitFieldDateTime {
    seconds_since_garmin_epoch: u32,
    rust_time: DateTime<UTC>,
}

impl fmt::Display for FitFieldDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "seconds_since_garmin_epoch: {}, rust_time: {}", self.seconds_since_garmin_epoch, self.rust_time)
    }
}

impl FitFieldParseable for FitFieldDateTime {
    fn parse(input: &[u8], parse_config: FitParseConfig) -> Result<FitFieldDateTime> {
        let (utc_dt, garmin_epoch_offset) = parse_date_time(input, parse_config)?;
        Ok(FitFieldDateTime {
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: utc_dt,
        })
    }
}

impl FitFieldDateTime {
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
        //let parse_config = FitParseConfig{ endianness: Some(Endianness::Big), size: None };
        let parse_config = fit_parse_config!(Endianness::Big);
        let result = FitFieldDateTime::parse(&bytes, parse_config)?;
        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub struct FitFieldLocalDateTime {
    seconds_since_garmin_epoch: u32,
    rust_time: DateTime<FixedOffset>,
}

impl fmt::Display for FitFieldLocalDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "seconds_since_garmin_epoch: {}, rust_time: {}", self.seconds_since_garmin_epoch, self.rust_time)
    }
}

impl FitFieldParseable for FitFieldLocalDateTime {
    fn parse(input: &[u8], parse_config: FitParseConfig) -> Result<FitFieldLocalDateTime> {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let garmin_epoch_offset = parse_uint32(input, parse_config)?;
        let local_dt = FixedOffset::east(parse_config.tz_offset_secs() as i32).timestamp(
            (garmin_epoch + Duration::seconds(garmin_epoch_offset.into())).timestamp(),
            0, // nanosecs
        );

        Ok(FitFieldLocalDateTime {
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: local_dt,
        })
    }
}

// Record type HR has crazy processing for the field event_timestamp_12,
// where the 10 components in that field are to be added to a base
// timestamp that is stored in the event_timestamp field. I'm not sure
// how to automate the creation of that logic, so for now, FitMessageHr is
// manually defined here instead of generated.

#[derive(Debug)]
pub struct FitMessageHr {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData>,
    unknown_fields: HashMap<u8, FitBaseValue>,
    pub raw_bytes: Vec<u8>,
    pub message_name: &'static str,
    pub timestamp: FitFieldBasicValue<FitFieldDateTime>,
    pub fractional_timestamp: FitFieldAdjustedValue<FitUint16>,
    pub time256: FitFieldAdjustedValue<FitUint8>,
    pub filtered_bpm: FitFieldBasicValue<FitUint8>,
    pub event_timestamp: FitFieldAdjustedValue<FitUint32>,
    pub event_timestamp_12: FitFieldAdjustedValue<FitUint8>,
}

impl fmt::Display for FitMessageHr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FitMessageHr")?;
        fmt_message_field!(self.timestamp, "timestamp", false, f)?;
        fmt_message_field!(self.fractional_timestamp, "fractional_timestamp", true, f)?;
        fmt_message_field!(self.time256, "time256", true, f)?;
        fmt_message_field!(self.filtered_bpm, "filtered_bpm", false, f)?;
        fmt_message_field!(self.event_timestamp, "event_timestamp", true, f)?;
        fmt_message_field!(self.event_timestamp_12, "event_timestamp_12", true, f)?;

        fmt_unknown_fields!(self, f);
        fmt_developer_fields!(self, f);
        fmt_raw_bytes!(self, f);
        Ok(())
    }
}

impl FitMessageHr {
    pub fn field_name(field_number: u8) -> &'static str {
        match field_number {
            253 => "timestamp",
            0 => "fractional_timestamp",
            1 => "time256",
            6 => "filtered_bpm",
            9 => "event_timestamp",
            10 => "event_timestamp_12",
            _ => "unknown",
        }
    }

    fn parse<'a>(
        input: &'a [u8],
        header: FitRecordHeader,
        parsing_state: &mut FitParsingState,
        _timestamp: Option<FitFieldDateTime>,
    ) -> Result<(Rc<FitMessageHr>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageHr {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            unknown_fields: HashMap::new(),
            raw_bytes: Vec::with_capacity(definition_message.message_size),
            message_name: "FitMessageHr",
            timestamp: FitFieldBasicValue::new_single("".to_string()),
            //timestamp: fit_field_value!(""),
            fractional_timestamp: FitFieldAdjustedValue::new_single("s".to_string(), 32768.0, 0.0),
            //fractional_timestamp: fit_field_value!("s", 32768_f64),
            time256: FitFieldAdjustedValue::new_single("s".to_string(), 256.0, 0.0),
            //time256: fit_field_value!("s", 256_f64),
            filtered_bpm: FitFieldBasicValue::new_vec("bpm".to_string()),
            //filtered_bpm: fit_field_value!("bpm"),
            event_timestamp: FitFieldAdjustedValue::new_vec("s".to_string(), 1024.0, 0.0),
            //event_timestamp: fit_field_value!("s", 1024_f64),
            //event_timestamp_12: fit_field_value!("s", 1024_f64),
            event_timestamp_12: FitFieldAdjustedValue::new_vec("s".to_string(), 1024.0, 0.0),

            /*
            timestamp: FitFieldValue {
                value: None,
                units: "".to_string(),
                scale: None,
                offset: None,
                parser: None,
            },
            fractional_timestamp: FitFieldValue {
                value: None,
                units: "s".to_string(),
                scale: Some(32768_f64),
                offset: None,
                parser: None,
            },
            time256: FitFieldValue {
                value: None,
                units: "s".to_string(),
                scale: Some(256_f64),
                offset: None,
                parser: None,
            },
            filtered_bpm: FitFieldValue {
                value: None,
                units: "bpm".to_string(),
                scale: None,
                offset: None,
                parser: None,
            },
            event_timestamp: FitFieldValue {
                value: None,
                units: "s".to_string(),
                scale: Some(1024_f64),
                offset: None,
                parser: None,
            },
            event_timestamp_12: FitFieldValue {
                value: None,
                units: "s".to_string(),
                scale: Some(1024_f64),
                //scale: Some(
                //    1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024_f64,
                //),
                offset: None,
                parser: None,
            },
            */
        };

        let o = main_parse_message!(input, message, parsing_state, FitMessageHr);

        parsing_state_set_timestamp!(message, _timestamp, parsing_state);

        let mut inp2 = o;
        parse_developer_fields!(inp2, message, parsing_state);

        Ok((Rc::new(message), inp2))
    }

    fn parse_internal<'a>(
        message: &mut FitMessageHr,
        input: &'a [u8],
        tz_offset: f64,
    ) -> Result<&'a [u8]> {
        let mut inp = input;
        let mut saved_outp = input;
        for field in &message.definition_message.field_definitions {
            let mut actions: Vec<FitParseConfig> = vec![FitParseConfig::new(
                *field,
                message.definition_message.endianness,
                tz_offset,
            )];

            //let mut actions: Vec<(FitFieldDefinition, Option<(usize, usize)>)> =
            //    vec![(*field, None)];

            while actions.len() > 0 {
                let parse_config = actions.remove(0);
               
                let alternate_input: Vec<u8>; // = Vec::with_capacity(parse_config.field_size());
                let mut parse_input = inp;

                if let Some((start, num_bits)) = parse_config.bit_range {
                    alternate_input = subset_with_pad(&inp[0..parse_config.field_size()], 
                        start, num_bits, parse_config.endianness())?;
                    parse_input = &alternate_input;
                };
                /*
                let parse_input = match parse_config.bit_range {
                    None => inp,
                    Some((start, num_bits)) => &subset_with_pad(
                        &inp[0..parse_config.field_size()],
                        start,
                        num_bits,
                        parse_config.endianness(),
                    )?,
                };
                */

                //let (f, components_bit_range) = actions.remove(0);

                //let parse_config = fit_parse_config!(message.definition_message.endianness, f.field_size);

                //let _parse_result: Result<()> = match f.definition_number {
                match parse_config.field_definition_number() {
                    253 => {
                        // timestamp
                        let _components = message.timestamp.parse(parse_input, parse_config)?;
                        saved_outp = &inp[parse_config.field_size()..];
                    }

                    0 => {
                        // fractional_timestamp
                        let _components = message
                            .fractional_timestamp
                            .parse(parse_input, parse_config)?;
                        saved_outp = &inp[parse_config.field_size()..];
                    }

                    1 => { // time256
                        let _components = message.time256.parse(parse_input, parse_config)?;
                        saved_outp = &inp[parse_config.field_size()..];

                        actions.push(FitParseConfig {
                            field_definition: Some(FitFieldDefinition {
                                definition_number: 0,
                                field_size: 2,
                                base_type: 0,
                            }),
                            endianness: parse_config.endianness,
                            tz_offset_secs: parse_config.tz_offset_secs,
                            bit_range: Some((0, 8)),
                        });
                    }

                    6 => { // filtered_bpm
                        let _components = message.filtered_bpm.parse(parse_input, parse_config)?;
                        saved_outp = &inp[parse_config.field_size()..];
                    }

                    9 => { // event_timestamp
                        let _components = message.event_timestamp.parse(parse_input, parse_config)?;
                        saved_outp = &inp[parse_config.field_size()..];
                    }

                    10 => { // event_timestamp_12
                        let _components = message.event_timestamp_12.parse(parse_input, parse_config)?;
                        saved_outp = &inp[parse_config.field_size()..];
                
                        // special stuff for FitMessageHr
                        let (current_timestamp, scale) = match message.event_timestamp {
                            FitFieldAdjustedValue {
                                value: AdjustedValue::Vec(ref cts),
                                parsed_value: _,
                                units: _,
                                scale: s,
                                offset: _,
                            } => {
                                if cts.len() == 0 {
                                    return Err(Error::hr_message_timestamp())
                                } else {
                                    (<f64>::from(cts[cts.len() - 1].clone()), s)
                                }
                            },
                            _ => return Err(Error::hr_message_timestamp())
                        };

                        let range = vec![0, 12, 24, 36, 48, 60, 72, 84, 96, 108];
                        let f = FitFieldDefinition {
                            definition_number: 9,
                            field_size: 4,
                            base_type: 0,
                        };

                        for i in 0..range.len() {
                            let bytes = subset_with_pad(
                                &inp[0..f.field_size],
                                range[i],
                                range[i] + 12,
                                message.definition_message.endianness,
                            )?;
                            let val = field_parser_base_type!("uint32", &bytes, parse_config)?;

                            let new_ts = current_timestamp + (val as f64 / scale);
                            match message.event_timestamp {
                                FitFieldAdjustedValue {
                                    value: AdjustedValue::Vec(ref mut v),
                                    parsed_value: _,
                                    units: _,
                                    scale: _,
                                    offset: _,
                                } => v.push(FitFloat64::new(new_ts)),
                                _ => return Err(Error::hr_message_timestamp()),
                            }
                        }
                    },

                    unknown_field_num => {
                        //let parse_config = FitParseConfig::new(
                        //    f,
                        //    message.definition_message.endianness,
                        //    tz_offset,
                        //);
                        let val = FitBaseValue::parse(inp, parse_config)?;
                        message.unknown_fields.insert(unknown_field_num, val);
                        saved_outp = &inp[parse_config.field_size()..];
                    }
                };
            }
            inp = saved_outp;
        }
        Ok(inp)
    }
}

impl FitRecord for FitMessageHr {
    fn message_name(&self) -> &'static str {
        return "FitMessageHr";
    }
}

impl From<FitFieldFitBaseType> for u8 {
    fn from(base_type: FitFieldFitBaseType) -> Self {
        match base_type {
            FitFieldFitBaseType::Enum => 0,
            FitFieldFitBaseType::Sint8 => 1,
            FitFieldFitBaseType::Uint8 => 2,
            FitFieldFitBaseType::Sint16 => 131,
            FitFieldFitBaseType::Uint16 => 132,
            FitFieldFitBaseType::Sint32 => 133,
            FitFieldFitBaseType::Uint32 => 134,
            FitFieldFitBaseType::String => 7,
            FitFieldFitBaseType::Float32 => 136,
            FitFieldFitBaseType::Float64 => 137,
            FitFieldFitBaseType::Uint8z => 10,
            FitFieldFitBaseType::Uint16z => 139,
            FitFieldFitBaseType::Uint32z => 140,
            FitFieldFitBaseType::Byte => 13,
            FitFieldFitBaseType::Sint64 => 142,
            FitFieldFitBaseType::Uint64 => 143,
            FitFieldFitBaseType::Uint64z => 144,
            FitFieldFitBaseType::UnknownToSdk => 3, // ?
            FitFieldFitBaseType::FitBaseType(x) => x, // ?
            FitFieldFitBaseType::InvalidFieldValue => 4 // ?
        }
    }
}
