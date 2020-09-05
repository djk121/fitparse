#!/usr/bin/env - python3

import csv
import sys
import inflect # for number -> words conversion

from jinja2 import Template, Environment

FIT_TYPE_MAP_RUST_TYPES = {
     'enum': 'u8',
     'uint8': 'u8',
     'uint8z': 'u8',
     'uint16': 'u16',
     'uint16z': 'u16',
     'uint32': 'u32',
     'uint32z': 'u32',
     'string': 'String',
     'sint8': 'i8',
     'sint16': 'i16',
     'sint32': 'i32',
     'bool': 'bool',
     'byte': 'Vec<u8>',
     'float32': 'f32',
     'float64': 'f64',
}

FIT_TYPE_MAP = {
    'enum': 'FitEnum',
    'uint8': 'FitUint8',
    'uint8z': 'FitUint8z',
    'uint16': 'FitUint16',
    'uint16z': 'FitUint16z',
    'uint32': 'FitUint32',
    'uint32z': 'FitUint32z',
    'string': 'FitString',
    'sint8': 'FitSint8',
    'sint16': 'FitSint16',
    'sint32': 'FitSint32',
    'bool': 'FitBool',
    'byte': 'FitByte',
    'float32': 'FitFloat32',
    'float64': 'FitFloat64',
}


RUST_TYPE_SIZE_MAP = {
    'enum': 1,
    'uint8': 1,
    'uint8z': 1,
    'uint16': 2,
    'uint16z': 2,
    'uint32': 4,
    'uint32z': 4,
    'string': 4,
    'sint8': 4,
    'sint16': 4,
    'sint32': 4,
    'bool': 1,
    'byte': 1,
    'float32': 4,
    'float64': 8,
}

FIT_BASE_TYPE_NUM = {
    'enum': 0,
    'uint8': 2,
    'uint8z': 10,
    'uint16': 132,
    'uint16z': 139,
    'uint32': 134,
    'uint32z': 140,
    'string': 7,
    'sint8': 1,
    'sint16': 131,
    'sint32': 133,
    'byte': 13,
    'float32': 136,
    'float64': 137,
    'sint64': 142,
    'uint64': 143,
    'uint64z': 144
}

FIT_BASE_TYPES = ['uint8', 'uint8z', 'uint16', 'uint16z', 'uint32', 'uint32z', 'sint8', 'sint16', 'sint32', 'float32', 'float64', 'string', 'byte']
FIT_BASE_NUMERIC_TYPES = ['uint8', 'uint8z', 'uint16', 'uint16z', 'uint32', 'uint32z', 'sint8', 'sint16', 'sint32', 'float32', 'float64']

ENUM_TYPE_TEMPLATE = """
{% set base_type = types[this_type]["base_type"] %}
{% set base_rust_type = fit_type_map_rust_types[base_type] %}
{% set fields = types[this_type]["fields"] %}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum {{ type_name }} { // fit base type: {{ base_type }}
{%- for field in fields if not field["value_name"][0].isdigit() %}
    {{ rustify_name(field["value_name"]) }} = {{ field["value"]}},
    {%- if field["comment"] %}  // {{ field["comment"] }}{% endif %}
{%- endfor %}
    InvalidFieldValue = -1,
    UnknownToSdk = -2,
}

impl fmt::Display for {{ type_name }} {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            {%- for field in fields if not field["value_name"][0].isdigit() %}
            {{ type_name }}::{{ rustify_name(field["value_name"]) }} => write!(f, "{}", "{{ rustify_name(field["value_name"]) }}"),
            {%- endfor %}
            {{ type_name }}::InvalidFieldValue => write!(f, "InvalidFieldValue"),
            {{ type_name }}::UnknownToSdk => write!(f, "UnknownToSdk")
        }
    }
}

impl FitFieldParseable for {{ type_name }} {
    fn parse(input: &[u8], parse_config: &FitParseConfig) -> Result<{{ type_name }}> {
        let val = parse_{{ base_type }}(input, parse_config);
        match val {
            Ok(v) => Ok({{ type_name }}::from(v)),
            Err(_) => Ok({{ type_name }}::InvalidFieldValue)
        }
    }
}

vec_fit_field_parseable!({{ type_name }});


impl From<{{ base_rust_type }}> for {{ type_name }} {
    fn from(code: {{ base_rust_type }}) -> Self {
        match code {
        {%- for field in fields %}
            {{ field['value'] }} => {{ type_name }}::{{ field['rustified_value_name'] }},
        {%- endfor %}
            _ => {{ type_name }}::UnknownToSdk,
        }
    }
}
"""


NON_ENUM_TYPE_TEMPLATE = """
{% set base_type = types[this_type]["base_type"] %}
{% set base_rust_type = fit_type_map_rust_types[base_type] %}
{% set fields = types[this_type]["fields"] %}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum {{ type_name }} { // fit base type: {{ base_type }}
{%- for field in fields if not field["value_name"][0].isdigit() %}
    {{ rustify_name(field["value_name"]) }}, // {{ field["value"] }}
    {%- if field["comment"] %}  {{ field["comment"] }}{% endif %}
{%- endfor %}
    {{ rustify_name(this_type) }}({{ base_rust_type }}),
    InvalidFieldValue,
    UnknownToSdk,
}

impl fmt::Display for {{ type_name }} {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            {%- for field in fields if not field["value_name"][0].isdigit() %}
            {{ type_name }}::{{ rustify_name(field["value_name"]) }} => write!(f, "{}", "{{ rustify_name(field["value_name"]) }}"),
            {%- endfor %}
            {{ type_name }}::{{ rustify_name(this_type) }}(x) => write!(f, "{}({})", "{{ rustify_name(this_type) }}", x),
            {{ type_name }}::InvalidFieldValue => write!(f, "InvalidFieldValue"),
            {{ type_name }}::UnknownToSdk => write!(f, "UnknownToSdk")
        }
    }
}

impl FitFieldParseable for {{ type_name }} {
    fn parse(input: &[u8], parse_config: &FitParseConfig) -> Result<{{ type_name }}> {
        let val = parse_{{ base_type }}(input, parse_config);
        match val {
            Err(_) => Ok({{ type_name }}::InvalidFieldValue),
            Ok(v) => match v {
                {% for field in fields if not field["value_name"][0].isdigit() %}
                {{ field["value"] }} => Ok({{ type_name }}::from(v)),
                {%- endfor %}
                v => Ok({{ type_name }}::{{ rustify_name(this_type) }}(v)),
            }
        }
    }
}

vec_fit_field_parseable!({{ type_name }});

impl From<{{ base_rust_type }}> for {{ type_name }} {
    fn from(code: {{ base_rust_type }}) -> Self {
        match code {
        {%- for field in fields %}
            {{ field['value'] }} => {{ type_name }}::{{ field['rustified_value_name'] }},
        {%- endfor %}
            _ => {{ type_name }}::UnknownToSdk,
        }
    }
}
"""

def resolve_field_size(field_type, types):
    if field_type in FIT_BASE_TYPES:
        return RUST_TYPE_SIZE_MAP[field_type]
    else:
        return RUST_TYPE_SIZE_MAP[types[field_type]['base_type']]

def resolve_field_type(field_type, types):
    if field_type in FIT_BASE_TYPES:
        return FIT_BASE_TYPE_NUM[field_type]
    else:
        return FIT_BASE_TYPE_NUM[types[field_type]['base_type']]


def rustify_name(name):
    rustified = [name[0].upper(),]
    i = 1
    while i < len(name):
        if name[i] != '_':
            rustified.append(name[i])
        else:
            i += 1
            rustified.append(name[i].upper())
        i += 1
    return ''.join(rustified)

def output_types(types):
    # everything in the types list gets mapped to a rust enum
    # that looks like this:
    #
    # enum <type> {
    #   <option1> = <option1 value>, // option1 comment
    # }
    #
    # impl <type> {
    #   fn new(input: &[u8]) -> Result<(&[u8], <type>), &'static str> {
    #       let (o, f) = match <parse function for type>(input) {
    #           <option1 value> => <type>::<option1 value>,
    #           _ => Err("unable to decode <type>"),
    #       }
    #       Ok((o,f))
    #    }
    # }

    special_types = r"""
use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;

use {FitFieldBasicValue, FitFieldAdjustedValue};
use FitRecord;
use FitRecordHeader;
use FitDefinitionMessage;
use FitFieldDefinition;
use FitFieldDeveloperData;
use FitGlobalMesgNum;
use FitMessageUnknownToSdk;
use FitBaseValue;
use FitParseConfig;
use FitFieldParseable;
use fitparsingstate::FitParsingState;
use fitparsers::{parse_enum, parse_uint8, parse_uint8_as_bytes, parse_uint8z, parse_uint16, parse_uint16_as_bytes, parse_uint32, parse_uint32_as_bytes, parse_uint32z, parse_byte_as_bytes};

use {vec_fit_field_parseable, fmt_message_field, fmt_raw_bytes, fmt_unknown_fields, fmt_developer_fields}; 
use fittypes_utils::{FitFieldDateTime, FitFieldLocalDateTime};
use {FitBool, FitUint8, FitUint8z, FitSint8, FitUint16, 
     FitUint16z, FitSint16, FitUint32, FitUint32z, FitSint32,
     FitFloat32, FitFloat64,
     FitByte, FitString};
use BasicValue;

use errors;
use errors::{FitParseError, Result};
"""

    sys.stdout.write(special_types)
    sys.stdout.write("\n")

    for this_type in types:
        # these need special handling, see above
        if this_type in ['date_time', 'local_date_time']:
            continue

        type_name = "FitField{}".format(rustify_name(this_type))

        template = None,
        if types[this_type]['base_type'] == 'enum':
            template = Environment().from_string(ENUM_TYPE_TEMPLATE, globals={'rustify_name': rustify_name, 'field_name': field_name})
        else:
            template = Environment().from_string(NON_ENUM_TYPE_TEMPLATE, globals={'rustify_name': rustify_name, 'field_name': field_name})

        content = template.render(this_type=this_type, type_name=type_name, types=types, fit_type_map=FIT_TYPE_MAP, fit_type_map_rust_types=FIT_TYPE_MAP_RUST_TYPES)
        sys.stdout.write(content)


def parse_types_file(types_file_name):
    types = {}

    f = open(sys.argv[1], 'rt')
    lines = []
    csvreader = csv.reader(f)
    for row in csvreader:
        lines.append(row)

    current_type = None
    for line in lines[1:]:
        if all([x == '' for x in line]):
            continue

        if line[0] != '':
            type_name, base_type, _, _, _ = line
            current_type = type_name
            types[current_type] = {"base_type": base_type.strip(),
                                   "fields": [],
                                   }
        else:
            _, _, value_name, value, comment = line

            # these are lies; in mesg_num, but with no defintion in the Messages table
            if current_type == 'mesg_num' and value_name == 'pad':
                continue
            # deprecated (in a comment field, lol)
            elif current_type == 'weather_report' and value_name == 'forecast':
                continue

            # 4iiiis and 1partcarbon are not valid rust identifiers
            elif current_type == 'manufacturer' and value_name[0].isdigit():
                continue

            if value_name[0].isdigit():
                new_value_name = ''
                i = 0
                temp = ''
                while value_name[i].isdigit():
                    temp += value_name[i]
                    i = i + 1

                p = inflect.engine()
                as_words = p.number_to_words(temp)
                as_words = as_words.replace("-", "_")

                value_name = as_words + value_name[i:]

            types[current_type]["fields"].append({"value": int(value.strip(), 0),
                                                  "comment": comment.strip(),
                                                  "value_name": value_name.strip(),
                                                  "rustified_value_name": rustify_name(value_name.strip())})

    return types

def is_fit_base_type(t):
    return t in FIT_TYPE_MAP.keys()

def field_name(field):
    if field.type != 'enum' and field.type in FIT_TYPE_MAP.keys():
        return FIT_TYPE_MAP[field.type]
    else:
        return "FitField{}".format(rustify_name(field.type))


FDM_TEMPLATE = """
#[derive(Debug)]
pub enum FitDataMessage {
    {% for message in messages %}
    {{ message.short_name }}(Rc<{{ message.full_name }}>),
    {%- endfor %}
    UnknownToSdk(Rc<FitMessageUnknownToSdk>),
    ParseError(FitParseError)
}

impl fmt::Display for FitDataMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            {%- for message in messages -%}
            FitDataMessage::{{ message.short_name }}(m) => write!(f, "{}", m),
            {% endfor -%}
            FitDataMessage::ParseError(e) => write!(f, "{}", e),
            FitDataMessage::UnknownToSdk(m) => write!(f, "{}", m)
        }
    }
}

impl FitDataMessage {

    pub fn field_name(global_mesg_num: &FitGlobalMesgNum, field_number: u8) -> &'static str {
        match *global_mesg_num {
            {% for message in messages %}
            FitGlobalMesgNum::Known(FitFieldMesgNum::{{ message.short_name }}) => {
                {{ message.full_name}}::field_name(field_number)
            },
            {% endfor %}
            FitGlobalMesgNum::Unknown(_) => "unknown",
            _ => "unexpected",
        }
    }

    pub fn parse<'a>(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState, timestamp: Option<FitFieldDateTime>) -> Result<(FitDataMessage, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        match definition_message.global_mesg_num {
            {% for message in messages %}
            FitGlobalMesgNum::Known(FitFieldMesgNum::{{ message.short_name }}) => {
                let mut m = {{ message.full_name }}::new(header, parsing_state)?;
                match m.parse(input, parsing_state, timestamp) {
                    Err(e) => {
                        Ok((FitDataMessage::ParseError(e), &input[definition_message.message_size..]))
                    }
                    Ok(o) => {
                        Ok((FitDataMessage::{{ message.short_name }}(Rc::new(m)), o))
                    }
                }
                //let o = m.parse(input, parsing_state, timestamp)?;
                //Ok((FitDataMessage::{{ message.short_name }}(Rc::new(m)), o))
            }
            {#
            FitGlobalMesgNum::Known(FitFieldMesgNum::{{ message.short_name }}) => {
                let (val, o) =
                    {{ message.full_name }}::parse(input, header, parsing_state, timestamp)?;
                Ok((FitDataMessage::{{ message.short_name }}(val), o))
            }
            #}
            {%- endfor %}
            FitGlobalMesgNum::Known(FitFieldMesgNum::MesgNum(number)) => {
                let (val, o) = FitMessageUnknownToSdk::parse(number, input, header, parsing_state, timestamp)?;
                Ok((FitDataMessage::UnknownToSdk(val), o))
            }
            FitGlobalMesgNum::Known(FitFieldMesgNum::MfgRangeMin) => {
                Err(errors::field_mfg_range_min())
            }
            FitGlobalMesgNum::Known(FitFieldMesgNum::MfgRangeMax) => {
                Err(errors::field_mfg_range_max())
            }
            FitGlobalMesgNum::Known(FitFieldMesgNum::InvalidFieldValue) => {
                Err(errors::field_invalid_value())
            }
            FitGlobalMesgNum::Known(FitFieldMesgNum::UnknownToSdk) => {
                Err(errors::field_unknown_to_sdk())
            }
            FitGlobalMesgNum::Unknown(number) => {
                let (val, o) = FitMessageUnknownToSdk::parse(number, input, header, parsing_state, timestamp)?;
                Ok((FitDataMessage::UnknownToSdk(val), o))
            }
            //_ => Ok((None, &input[definition_message.message_size..])),
        }
    }

    pub fn message_name(&self) -> &'static str {
        match self {
            {%- for message in messages -%}
            FitDataMessage::{{ message.short_name }}(_) => "{{ message.short_name }}",
            {% endfor -%}
            FitDataMessage::ParseError(_) => "ParseError",
            FitDataMessage::UnknownToSdk(_) => "UnknownToSdk",
        }
    }

    {% for message in messages %}
    pub fn is_{{ message.profile_name }}(&self) -> bool {
        match *self {
            FitDataMessage::{{ message.short_name }}(_) => true,
            _ => false,
        }
    }
    {% endfor %}

    pub fn is_unknown(&self) -> bool {
        match *self {
            FitDataMessage::UnknownToSdk(_) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
#[path = "./fitmessagerecord_test.rs"]
mod fitmessagerecord_test;
"""

def output_messages(messages, types):
    messages = sorted(messages.items(), key=lambda m: m[0])

    for this_message, message_def in messages:
        message_def.output()

    message_names = sorted([m[1].rustified_name for m in messages])
    mesgs = sorted([rustify_name(mnf['value_name']) for mnf in types['mesg_num']['fields'] if mnf['value_name'] not in ('mfg_range_min', 'mfg_range_max')])
    template = Environment().from_string(FDM_TEMPLATE,
                                         globals={'messages': [x[1] for x in messages]})

    sys.stdout.write(template.render())

class Message(object):
    STRUCT_TEMPLATE = """
#[derive(Debug)]
pub struct {{ message_name }} {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData>,
    unknown_fields: HashMap<u8, FitBaseValue>,
    pub raw_bytes: Vec<u8>,
    pub subfield_field_numbers: Vec<u8>,
    pub message_name: &'static str,
    {% for field in fields -%}
    {%- if field.has_subfields -%}
    pub {{ field.name }}_subfield_bytes: Vec<u8>,
    {% endif -%}
    pub {{ field.name }}: {{ field.output_field_option() }},  {% if field.comment %}// {{ field.comment }}{% endif %}
    {% endfor %}
}
"""

    IMPL_TEMPLATE = """

impl fmt::Display for {{ message_name }} {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{ message_name}}")?;
        {% for field in fields -%}
        {%- if field.has_subfields -%}
        writeln!(f, "  {: >28}: {:?}", "{{ field.name }}_subfield_bytes", self.{{ field.name }}_subfield_bytes)?;
        writeln!(f, "  {: >28}: {:?}", "{{ field.name }}", self.{{field.name }})?;
        {% else -%}
        fmt_message_field!(self.{{ field.name }}, "{{ field.name }}", f);
        {% endif -%}
        {% endfor %}
            
        fmt_unknown_fields!(self, f);
        fmt_developer_fields!(self, f);
        fmt_raw_bytes!(self, f);
        Ok(())
    }
}

impl {{ message_name }} {

    pub fn field_name(field_number: u8) -> &'static str {
        match field_number {
            {% for field in fields -%}
            {{ field.number }} => "{{ field.name }}",
            {% endfor -%}
            _ => "unknown"
        }
    }

    pub fn new(header: FitRecordHeader, parsing_state: &FitParsingState) -> Result<{{ message_name }}> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        {% if has_components %}let endianness = definition_message.endianness;{% endif %}
        let message = {{ message_name }} {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            unknown_fields: HashMap::new(),
            raw_bytes: Vec::with_capacity(definition_message.message_size),
            subfield_field_numbers: vec![{{ subfield_field_numbers }}],
            message_name: "{{ message_name }}",
            {% for field in fields %}
            {%- if field.has_subfields -%}
            {{ field.name }}_subfield_bytes: vec![],
            {% endif -%}
            {{ field.name }}: {{ field.output_message_field() }},
            {% endfor %}
        };

        Ok(message)
    }

    fn parse_developer_fields<'a, 'b>(&'a mut self, input: &'b [u8], parsing_state: &FitParsingState) -> Result<&'b [u8]> {
        let mut inp = input;

        for dev_field in &self.definition_message.developer_field_definitions {
            let dev_data_definition =
                parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;
            let field_description =
                dev_data_definition.get_field_description(dev_field.definition_number)?;

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
                _ => return Err(errors::unknown_error()),
            };

            let def_num = <u8>::from(field_description.field_definition_number.get_single()?);

            let parse_config = FitParseConfig::new(
                FitFieldDefinition::new(def_num, dev_field.field_size, base_type_num)?,
                self.definition_message.endianness,
                0.0
            );

            let dd = FitFieldDeveloperData::parse(inp, field_description.clone(), &parse_config)?;
            self.developer_fields.push(dd);
            
            // we can run out of input before all fields are consumed. according
            // to the spec, buffering with zero-padded fields is appropriate
            
            if inp.len() < parse_config.field_size() {
                inp = &inp[inp.len()..];
            } else {
                inp = &inp[parse_config.field_size()..];
            }
        }
        Ok(inp)
    }

    fn parse<'a, 'b>(&'a mut self, input: &'b [u8], parsing_state: &mut FitParsingState, _timestamp: Option<FitFieldDateTime>) -> Result<&'b [u8]> {
        let to_copy = &input[..self.definition_message.message_size];
        let mut inp = input;

        if parsing_state.retain_bytes == true {
            self
                .raw_bytes
                .resize(self.definition_message.message_size, 0);
            self.raw_bytes.copy_from_slice(to_copy);
        }
        let tz_offset = parsing_state.get_timezone_offset();
        let outp = match self.parse_internal(inp, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                return Err(errors::message_parse_failed(
                    stringify!({{ message_name }}).to_string(), 
                    self.definition_message.clone(),
                    inp[..self.definition_message.message_size].to_vec(),
                    e
                ))
            }
        };
        inp = outp;

        {% if has_timestamp_field %}
        match _timestamp {
            Some(ts) => {
                self.timestamp.value = BasicValue::Single(ts);
            }
            None => {
                if self.timestamp.is_parsed() {
                    let ts = self.timestamp.get_single()?;
                    parsing_state.set_last_timestamp(ts);
                }
            }
        }
        {% endif %}

        let inp = self.parse_developer_fields(inp, parsing_state)?;
        Ok(inp)
    }

    fn parse_one_field<'a, 'b>(&'a mut self, input: &'b [u8], parse_config: &FitParseConfig) -> Result<Vec<FitParseConfig>> {
        let alternate_input: Vec<u8>;
        let mut parse_input = input;

        if parse_config.use_stored_input() {
            alternate_input = parse_config.get_stored_input()?;
            parse_input = &alternate_input;
        }

        let new_actions = match parse_config.field_definition_number() {

        {% for field in fields -%}
            {{ field.number }} => {  // {{ field.name }}
                {% if field.has_subfields -%}
                self.{{ field.name }}_subfield_bytes = parse_{{ field.type }}_as_bytes(parse_input, parse_config)?;
                vec![]
                {% else -%}
                self.{{ field.name }}.parse(parse_input, parse_config)?
                {% endif -%}
            },
        {% endfor %}
            unknown_field_num => {
                let val = FitBaseValue::parse(parse_input, parse_config)?;
                self.unknown_fields.insert(unknown_field_num, val);
                vec![]
            }
        };

        Ok(new_actions)
    }

    {% if has_subfields %}
    fn parse_one_subfield<'a>(&'a mut self, parse_config: &FitParseConfig) -> Result<Vec<FitParseConfig>> {
        let new_actions = match parse_config.field_definition_number() {
            {% for field in fields if field.has_subfields %}
            {{ field.number }} => {
                let (val, new_actions) = {{ field.output_subfield_parser()}}?;
                self.{{ field.name }} = val;
                new_actions
            },
            {% endfor %}
            bad_number => { return Err(errors::bad_subfield_field_number(bad_number)) }
        };
        Ok(new_actions)
    }
    {% endif %}

    fn parse_internal<'a, 'b>(&'a mut self, input: &'b [u8], tz_offset: f64) -> Result<&'b [u8]> {
        let mut inp = input;
       
        // first parse things according to the definitions, don't deep-parse the subfields
        let mut actions = vec![];
        for field in &self.definition_message.field_definitions {
            actions.push(FitParseConfig::new(*field, self.definition_message.endianness, tz_offset));
        }
        {% if has_subfields %}
        let mut subfields: Vec<u8> = actions.iter()
            .map(|action| action.field_definition_number())
            .filter(|field_num| self.subfield_field_numbers.contains(field_num))
            .collect();
        {% endif %} 

        loop {
            while actions.len() > 0 {
                let this_action = actions.remove(0);
                let mut new_actions = self.parse_one_field(inp, &this_action)?;
                {% if has_subfields %}
                let new_subfields: Vec<u8> = new_actions.iter()
                    .map(|action| action.field_definition_number())
                    .filter(|field_num| self.subfield_field_numbers.contains(field_num))
                    .collect();
                {% endif %} 
                
                // new actions here go to the front of the list
                new_actions.reverse();
                while new_actions.len() > 0 {
                    actions.insert(0, new_actions.remove(0))
                }

                {% if has_subfields %}subfields.extend(new_subfields);{% endif %}

                if this_action.use_stored_input() == false {
                    inp = &inp[this_action.field_size()..];
                }
                {#
                // only move inp forward if this was a regular field parse (not a subfield, not a component)
                if let None = this_action.bit_range {
                    inp = &inp[this_action.field_size()..];
                }
                #}

            }

            {% if has_subfields %}
            while subfields.len() > 0 {
                let this_subfield = subfields.remove(0);
                let fds: Vec<_> = self.definition_message.field_definitions.iter().filter(|f| f.definition_number == this_subfield).collect();
                let mut new_actions = vec![];

                // FIXME(should error if this fails)
                if fds.len() == 1 {
                    let parse_config = FitParseConfig::new(*fds[0], self.definition_message.endianness, tz_offset);
                    new_actions.extend(self.parse_one_subfield(&parse_config)?);
                }

                actions.extend(new_actions);
            }
            {% endif %}

            if actions.len() == 0 {
                break;
            }
        }

        Ok(inp)
    }
}

impl FitRecord for {{ message_name }} {
    fn message_name(&self) -> &'static str {
        return "{{ message_name }}";
    }
}

"""

    def __init__(self, name, comment):
        self.name = name
        self.comment = comment
        self.fields = []
        self.has_subfields = False

    @property
    def lifetime_spec(self):
        has_byte_field = False
        for field in self.fields:
            if field.type == 'byte':
                has_byte_field = True
                break

        if has_byte_field:
            return "<'a>"
        else:
            return ""

    @property
    def message_name(self):
        return "FitMessage{}".format(rustify_name(self.name))

    @property
    def full_name(self):
        return "FitMessage{}".format(rustify_name(self.name))

    @property
    def short_name(self):
        return rustify_name(self.name)

    @property
    def profile_name(self):
        return self.name

    @property
    def rustified_name(self):
        return rustify_name(self.name)

    @property
    def has_timestamp_field(self):
        return True in [f.name == 'timestamp' for f in self.fields]

    @property
    def has_components(self):
        for field in self.fields:
            if len(field.components) > 0:
                return True
        return False

    def subfield_field_numbers(self):
        numbers = []
        for field in self.fields:
            if len(field.subfields) > 0:
                numbers.append(field.number)
        return numbers

    def add_field(self, field):
        field.message = self
        self.fields.append(field)

    def add_subfield(self, subfield):
        subfield.message = self
        self.fields[-1].subfields.append(subfield)
        self.has_subfields = True

    def output(self):
        if self.has_subfields:
            self.output_subfields()

        self.output_struct()
        self.output_impl()


    def output_struct(self):
        template = Environment().from_string(self.STRUCT_TEMPLATE,
                                             globals={'fields': self.fields,
                                                      'message_name': self.message_name})
        content = template.render()
        sys.stdout.write(content)

    def output_impl(self):
        template = Environment().from_string(self.IMPL_TEMPLATE,
                                             globals={'message_name': self.message_name,
                                                      'fields': self.fields,
                                                      'has_timestamp_field': self.has_timestamp_field,
                                                      'has_subfields': self.has_subfields,
                                                      'has_components': self.has_components,
                                                      'subfield_field_numbers': ','.join(["{}".format(x) for x in self.subfield_field_numbers()])})
        sys.stdout.write(template.render())

    def output_subfields(self):
        for field in self.fields:
            if not field.has_subfields:
                continue

            sys.stdout.write(field.output_subfield_definition())

class FieldBase(object):
    def __init__(self, *args, **kwargs):
        super(FieldBase, self).__init__(self, *args, **kwargs)


class Field(object):
    FIELD_OPTION_SUBFIELD = """FitMessage{{ message_name }}Subfield{{ field_name }}"""
    FIELD_OPTION_BASE_TYPE = """FitFieldBasicValue<{% if is_vec %}Vec<{% endif %}{{ fit_type }}{% if is_vec %}>{% endif %}>"""
    FIELD_OPTION_FIT_TYPE = """FitFieldBasicValue<{% if is_vec %}Vec<{% endif %}FitField{{ field_type }}{% if is_vec %}>{% endif %}>"""


    def __init__(self, number, name, type, array, components, bits, scale, offset, units, comment, types):
        self.number = int(number)
        self.name = name
        self.type = type
        self.array = (array != '' and type != 'byte')
        self.scale = None
        self.offset = None
        self.units = ''
        self.comment = comment
        self.subfields = []
        self.message = None
        self.types = types
        self.is_adjusted = False

        self.components = []

        # if name == "time_zone_offset":
        #     print("components: {}".format(components), file=sys.stderr)
        #     print("scale: {}".format(scale), file=sys.stderr)

        #     print("offset: {}".format(offset), file=sys.stderr)

        # only base types can have scale/offset. the weight message
        # type screws this up in the fit profile, so protect here
        if not self.type in FIT_TYPE_MAP.keys():
            return

                    
        if len(components) in [0,1]:
            if len(scale):
                self.scale = float(scale[0])
                self.is_adjusted = True
                if len(offset):
                    self.offset = float(offset[0])
                else:
                    self.offset = 0.0

            if len(units):
                self.units = units[0]

            if len(components) == 1:
                self.components.append((components[0], set_bit_ranges(bits)[0], self.scale, self.offset, self.units))
        else:
            # more than one component, so this field is not adjusted, and we need to pass scale/offset along
            self.is_adjusted = False
            bit_ranges = set_bit_ranges(bits)
            for i in range(0, len(components)):
                this_scale = None
                this_offset = None
                this_units = ''

                if i <= len(scale) - 1:
                    this_scale = float(scale[i])

                if i <= len(offset) - 1:
                    this_offset = float(offset[i])

                if i <= len(units) - 1:
                    this_units = units[i]

                self.components.append((components[i], bit_ranges[i], this_scale, this_offset, this_units))





        # components, scale, offset = rationalize_components_scale_offset(components, scale, offset)

        # # units apply to this field        
        # if len(units):
        #     self.units = units[0]

        # if len(components) > 0:
        #     bit_ranges = set_bit_ranges(bits)

        #     for i in range(0, len(components)):
        #         self.components.append((components[i], bit_ranges[i]))
        #         self.scale = scale
        #         self.offset = offset 

        # else:

        #     if len(scale) == 0 and len(offset) == 0:
        #         self.is_adjusted = False
        #     elif len(scale) == 1 and len(offset) == 0:
        #         self.scale = float(scale[0])
        #         self.offset = 0.0
        #         self.is_adjusted = True
        #     elif len(scale) == 0 and len(offset) == 1:
        #         self.offset = float(offset[0])
        #         self.scale = 0.0
        #         self.is_adjusted = True
        #     elif len(scale) == 1 and len(offset) == 1:
        #         self.offset = float(offset[0])
        #         self.scale = float(scale[0])
        #         self.is_adjusted = True
       

    @property
    def output_units(self):
        if self.is_semicircles:
            return "deg"
        else:
            if self.units is None:
                return ""
            else:
                return self.units

    @property
    def lifetime_spec(self):
        return ""

    @property
    def rustified_name(self):
        return rustify_name(self.name)

    @property
    def has_subfields(self):
        return self.subfields != []

    @property
    def subfields_have_compoments(self):
        if len(self.components) > 0:
            return True
        for subfield in self.subfields:
            if len(subfield.components) > 0:
                return True
        return False

    @property
    def field_name(self):
        return rustify_name(self.name)

    @property
    def field_type_name(self):
        return field_name(self)
    
    @property
    def has_components(self):
        return len(self.components) > 0

    @property
    def is_semicircles(self):
        if self.units == 'semicircles':
            return True
        return False


    FIELD_PARSER_SUBFIELD_BYTES = """parse_byte({{ bytes_from }}, field.field_size)"""
    FIELD_PARSER_SUBFIELD = """FitMessage{{ message_name }}Subfield{{ field_name }}::parse(&self, {{ bytes_from }}, &parse_config)"""


    def output_subfield_parser(self):
        bytes_from = "&self.{}_subfield_bytes".format(self.name)
        template = Environment().from_string(self.FIELD_PARSER_SUBFIELD,
                                             globals={'message_name': self.message.rustified_name,
                                                      'field_name': self.rustified_name,
                                                      'bytes_from': bytes_from})
        return template.render()

    def output_field_parser(self, bytes_from, only_default_case=False, field_variable_name='field'):
        if self.subfields and not only_default_case:
            template = Environment().from_string(self.FIELD_PARSER_SUBFIELD_BYTES,
                                                 globals={'message_name': self.message.rustified_name,
                                                          'field_name': self.rustified_name,
                                                          'bytes_from': bytes_from})
            return template.render()

        elif self.type in FIT_TYPE_MAP.keys():
            return ""
            if self.type not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']:
                return "field_parser_base_type!(\"{}\", {}, f, message)".format(self.type, bytes_from)
            else:
                return "field_parser_base_type!(\"{}\", {}, f)".format(self.type, bytes_from)


        else:
            endianness = self.types[self.type]['base_type'] not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']
            local_date_time = self.type == 'local_date_time'
          
            if endianness and local_date_time:
                return "field_parser_fit_type!(FitField{}, {}, f, message, _tz_offset)".format(rustify_name(self.type), bytes_from)
            elif endianness:
                return "field_parser_fit_type!(FitField{}, {}, f, message)".format(rustify_name(self.type), bytes_from)
            else:
                return "field_parser_fit_type!(FitField{}, {}, f)".format(rustify_name(self.type), bytes_from)

   
    # def calculate_components_vec(self):
    #     # if both components and scale/offset are present, the scale/offset
    #     # apply to the components, not this field.       
    #     components_parts = []
    #     if len(self.components):
    #         for i in range(len(self.components)):
    #             for field in self.message.fields:
    #                 if self.components[i][0] == field.name:
    #                     field_size = resolve_field_size(field.type, self.types)
    #                     base_type_num = resolve_field_type(field.type, self.types)

    #                     (bit_start, bit_len) = self.components[i][1]

    #                     components_parts.append("FitParseConfig::new_from_component({}, {}, {}, message.definition_message.endianness, {}, {})".format(field.number, field_size, base_type_num, bit_start, bit_len))
    #     return "vec![{}]".format(",".join(components_parts))
    def calculate_components_vec(self):
        return calculate_components_vec(self.components, self.message, self.types)

    def output_message_field(self):
        if self.subfields:
            return "FitMessage{}Subfield{}::NotYetParsed".format(rustify_name(self.message.name), rustify_name(self.name))
        
        new_spec = '_single'
        if self.array:
            new_spec = '_vec'

        ret = ''
     
        if ((self.scale or self.offset) and self.type in FIT_TYPE_MAP.keys()) or self.is_semicircles:
            scale = "0.0"
            if self.scale:
                scale = self.scale
            
            offset = "0.0"
            if self.offset:
                offset = self.offset

            ret = "FitFieldAdjustedValue::new{}(\"{}\".to_string(), {}, {})".format(new_spec, self.output_units, scale, offset)
        else:
            ret = "FitFieldBasicValue::new{}(\"{}\".to_string())".format(new_spec, self.output_units)
        
       
        if self.components:
            ret += ".add_components({})".format(self.calculate_components_vec())

        return ret

    def output_field_option(self):
        content = ""
        if self.subfields:
            template = Environment().from_string(self.FIELD_OPTION_SUBFIELD,
                                                 globals={'message_name': self.message.rustified_name,
                                                          'field_name': self.rustified_name})
            return template.render()

        elif self.type != 'enum' and self.type in FIT_TYPE_MAP.keys():
            name = "Fit{}".format(rustify_name(self.type))
            
            if (self.scale or self.is_semicircles) and self.type != 'byte':
                content = "FitFieldAdjustedValue<{}>".format(name)
            else:
                content = "FitFieldBasicValue<{}>".format(name)
            
            return content

        else:
            return "FitFieldBasicValue<FitField{}>".format(rustify_name(self.type))

    def output_field_container(self):
        array_spec = "_single"
        if self.array:
            array_spec = "_vec"
        if self.is_adjusted or self.is_semicircles:
            return "FitFieldAdjustedValue::new{}(\"{}\".to_string(), {}, {})".format(array_spec, self.units, self.scale, self.offset)
        else:
            #print("units: {}".format(self.units), file=sys.stderr)
            return "FitFieldBasicValue::new{}(\"{}\".to_string())".format(array_spec, self.units)

    def output_parsed_field_assignment(self):
        # we're just capturing the bytes here, the parse will happpen
        # after the rest of the message has been parsed.
        if self.has_subfields:
            return "if let Some(v) = val {{ message.{}_subfield_bytes = v.into(); }}".format(self.name)

        if self.scale and is_fit_base_type(self.type) and not self.subfields and self.type != 'byte':

            offset = 0
            if self.offset:
                offset = self.offset

            if self.array:
                return "scale_and_offset_parse_assignment!(\"vec\", val, message.{}, {}, {});".format(self.name, self.scale, offset)
            else:
                return "scale_and_offset_parse_assignment!(val, message.{}, {}, {});".format(self.name, self.scale, offset)

        if self.array or is_fit_base_type(self.type) is False or self.has_subfields:
            return "message.{}.value = Some(val);".format(self.name)

        else:
            if self.is_semicircles:
                return "deg_parse_assignment!(val, message.{});".format(self.name)
            return "message.{}.value = val;".format(self.name)


    SUBFIELD_TEMPLATE = """
#[derive(Debug)]
pub enum {{ subfield_name }} {
    NotYetParsed,
    Default({{ subfield_default_option }}),
    {%- for sf in subfield_enum_options %}
    {{ sf[0] }}({{ sf[1] }}),
    {%- endfor %}
}

impl {{ subfield_name }} {
    fn parse<'a>(message: &{{ message_name }}, inp: &'a [u8], parse_config: &FitParseConfig) -> Result<({{ subfield_name }}, Vec<FitParseConfig>)> {
        {% if has_components %}let endianness = parse_config.endianness();{% endif %}
        {% for sf_name in subfield_ref_names %}
        if message.{{ sf_name }}.is_parsed() {
            match message.{{ sf_name }}.get_single()? {
            {% for sf in subfield_options[sf_name] %}
                FitField{{ sf.ref_field_type_rustified }}::{{ sf.ref_field_value_rustified }} => {
                    let mut parser = {{ sf.output_field_parser(field.types, field.message.rustified_name) }};
                    parser.parse(inp, parse_config)?;
                    {% if sf.is_adjusted %}
                    let val = <FitFloat64>::from(parser.get_single()?);
                    {% else %}
                    let val = parser.get_single()?;
                    {% endif %}
                    let new_actions: Vec<FitParseConfig> = {{ sf.calculate_components_vec() }}.iter().map(|action: &FitParseConfig| action.add_bytes_to_parse(&inp)).collect();
                    
                    return Ok(({{ subfield_name }}::{{ sf.field_name_rustified }}(val), new_actions))
                },
            {% endfor %}
                _ => (),
            }
        } 
        {% endfor %}
        let mut parser = {{ subfield_default_parser }};
        parser.parse(inp, parse_config)?;
        {% if subfield_default_is_adjusted %}
        let val = <FitFloat64>::from(parser.get_single()?);
        {% else %}
        let val = parser.get_single()?;
        {% endif %}
        // AFAICT, the top-level subfield can never have components
        Ok(({{ subfield_name }}::Default(val), vec![]))
    }
}
"""

    def output_subfield_definition(self):
        subfield_name = "FitMessage{}Subfield{}".format(self.message.rustified_name, self.rustified_name)
        subfield_ref_names = set([sf.ref_field_name for sf in self.subfields])
        subfield_options = {}
        for srn in subfield_ref_names:
            subfield_options[srn] = [sf for sf in self.subfields if sf.ref_field_name == srn]        
        
        #if self.type in FIT_TYPE_MAP:
        #    subfield_default_parser = "Fit{}::parse(inp, parse_config)".format(rustify_name(self.type))
        #else:
        #    subfield_default_parser = "FitField{}::parse(inp, parse_config)".format(rustify_name(self.type))
        
        
        subfield_enum_options = []
        for sf in self.subfields:

            ftn = ''
            if sf.type in FIT_TYPE_MAP:
                ftn = "{}".format(FIT_TYPE_MAP[sf.type])
            else:
                ftn = "{}".format(sf.field_type_name)

            if sf.is_adjusted:
                ftn = "FitFloat64"
            #    ftn = "FitFieldAdjustedValue<{}>".format(ftn)
            #else:
            #    ftn = "FitFieldBasicValue<{}>".format(ftn)

            subfield_enum_options.append((sf.field_name_rustified, ftn))

        subfield_default_option = ''
        if self.type in FIT_TYPE_MAP:
            if self.is_adjusted:
                subfield_default_option = "FitFloat64"
            else:
                subfield_default_option = "{}".format(self.field_type_name)
        else:
            subfield_default_option = "FitField{}".format(self.field_type_name)

        #if self.is_adjusted:
        #    subfield_default_option = "FitFieldAdjustedValue<{}>".format(subfield_default_option)
        #else:
        #    subfield_default_option = "FitFieldBasicValue<{}>".format(subfield_default_option)

        subfield_default_parser = ''
        if self.is_adjusted:
            #subfield_default_parser = "FitFieldAdjustedValue::<{}>::parse(inp, parse_config)".format(rustify_name(self.type))
            subfield_default_parser = "FitFieldAdjustedValue::<{}>::new_single(\"\".to_string(), {}, {})".format(subfield_default_option, self.scale, self.offset)

        else:
            #subfield_default_parser = "FitFieldBasicValue::<{}>::parse(inp, parse_config)".format(rustify_name(self.type))
            subfield_default_parser = "FitFieldBasicValue::<{}>::new_single(\"\".to_string())".format(subfield_default_option)

        lifetime_spec = ''
        if self.subfield_needs_lifetime_spec():
            lifetime_spec = "<'a>"

        template = Environment().from_string(self.SUBFIELD_TEMPLATE,
                                             globals={'subfield_name': subfield_name,
                                                      'subfield_default_option': subfield_default_option,
                                                      'field': self,
                                                      'subfield_options': subfield_options,
                                                      'message_name': self.message.message_name,
                                                      'subfield_enum_options': set(subfield_enum_options),
                                                      'subfield_ref_names': subfield_ref_names,
                                                      'lifetime_spec': lifetime_spec,
                                                      'subfield_default_parser': subfield_default_parser,
                                                      'subfield_default_is_adjusted': self.is_adjusted,
                                                      'has_components': self.subfields_have_compoments})
        return template.render()


    def subfield_needs_lifetime_spec(self):
        for subfield in self.subfields:
            if subfield.needs_lifetime_spec:
                return True

        if self.type == 'byte':
            return True

        return False

FIELD_PARSER_BASE_TYPE = """parse_{{ field_type }}(&{{ bytes_from }}[0..f.field_size]
{%- if field_size -%}, {{ field_name }}.field_size{%- endif -%}
{%- if endianness -%}, message.definition_message.endianness{%- endif -%}
)"""

def set_bit_ranges(bits): 
    bit_ranges = []
    range_begin = 0
    for bit_amt in bits:
        range_end = range_begin + bit_amt
        bit_ranges.append((range_begin, bit_amt))
        range_begin = range_end

    return bit_ranges

def rationalize_components_scale_offset(components, scale, offset):
    # the standard says that scale/offset must be defined for each component
    # but the fit spec frequently violates the standard, so we need
    # to set scale/offset to 1.0 and 0.0 when they are omitted

    scale_out = []
    offset_out = []

    if len(components) == 0:
        return components, scale, offset

    for i in range(len(components)):
        if i > (len(scale) - 1):
            scale_out.append(1.0)
        else:
            scale_out.append(float(scale[i]))
        
        if i > (len(offset) - 1):
            offset_out.append(0.0)
        else:
            offset_out.append(float(offset[i]))

    return components, scale_out, offset_out

def calculate_components_vec(components, message, types):   

    components_parts = []
    if len(components):
        for i in range(len(components)):

            # this_scale = scale[i]
            # this_offset = offset[i]

            # components[i], bit_ranges[i], scale, offset, units
            if components[i][2] is None: # scale
                scale_and_offset = "None"
            else:
                offset = components[i][3]
                if offset is None:
                    offset = "0.0"
                
                scale_and_offset = "Some(({}, {}))".format(components[i][2], offset)

            if components[i][4] is None: # units
                units = "None"
            else:
                units = "Some(\"{}\".to_string())".format(components[i][4])

            for field in message.fields:
                if components[i][0] == field.name:
                    field_size = resolve_field_size(field.type, types)
                    base_type_num = resolve_field_type(field.type, types)

                    (bit_start, bit_len) = components[i][1]

                    components_parts.append("FitParseConfig::new_from_component({}, {}, {}, endianness, {}, {}, {}, {})?".format(field.number, field_size, base_type_num, bit_start, bit_len, scale_and_offset, units))
    return "vec![{}]".format(",".join(components_parts))

class Subfield(object):
    def __init__(self, field_name, field_type, ref_field_name, ref_field_value, components, bits, scale, offset, units, types):
        self.field_name = field_name
        self.type = field_type
        self.ref_field_name = ref_field_name
        self.ref_field_type = None
        self.ref_field_value = ref_field_value
        self.components = []
        self.bits = bits
        self.scale = None
        self.offset = None
        self.units = ''
        self.message = None
        self.types = types
        self.is_adjusted = False

        # if len(units):
        #     self.units = units[0]

        # components, scale, offset = rationalize_components_scale_offset(components, scale, offset)

        # if len(components) == 0:
        #     if len(scale) > 0:
        #         self.scale = float(scale[0])
        #         self.offset = 0.0

        #     if len(offset) > 0:
        #         self.offset = float(offset[0])
        # else:
        #     self.components = components
        #     self.scale = scale
        #     self.offset = offset

        if len(components) in [0,1]:
            if len(scale):
                self.scale = float(scale[0])
                self.is_adjusted = True
                if len(offset):
                    self.offset = float(offset[0])
                else:
                    self.offset = 0.0

            if len(units):
                self.units = units[0]

            if len(components) == 1:
                self.components.append((components[0], set_bit_ranges(bits)[0], self.scale, self.offset, self.units))
        else:
            # more than one component, so this field is not adjusted, and we need to pass scale/offset along
            self.is_adjusted = False
            bit_ranges = set_bit_ranges(bits)
            for i in range(0, len(components)):
                this_scale = None
                this_offset = None
                this_units = ''

                if i <= len(scale) - 1:
                    this_scale = float(scale[i])

                if i <= len(offset) - 1:
                    this_offset = float(offset[i])

                if i <= len(units) - 1:
                    this_units = units[i]

                self.components.append((components[i], bit_ranges[i], this_scale, this_offset, this_units))


    @property
    def has_components(self):
        return len(self.components) > 0

    @property
    def needs_lifetime_spec(self):
        return self.type == 'byte'

    @property
    def field_type_name(self):
        return field_name(self)

    @property
    def field_name_rustified(self):
        return rustify_name(self.field_name)

    @property
    def ref_field_type_rustified(self):
        return rustify_name(self.ref_field_type)

    @property
    def is_fit_base_type(self):
        return self.type in FIT_TYPE_MAP.keys()

    @property
    def ref_field_value_rustified(self):
        return rustify_name(self.ref_field_value)

    def calculate_components_vec(self):
        return calculate_components_vec(self.components, self.message, self.types)

    def output_field_parser(self, types, message_name):
        # fixme: are these ever arrays?

        this_type = ''
        if self.type in FIT_TYPE_MAP:
            this_type = "Fit{}".format(rustify_name(self.type))
        else:
            this_type = "FitField{}".format(rustify_name(self.type))

        # FIXME: components in subfields
        if self.is_adjusted:
            return "FitFieldAdjustedValue::<{}>::new_single(\"{}\".to_string(), {}, {})".format(this_type, self.units, self.scale, self.offset)
        else:
            return "FitFieldBasicValue::<{}>::new_single(\"{}\".to_string())".format(this_type, self.units)


    #def output_field_parser(self, types, message_name):
    #
    #    if self.type in FIT_TYPE_MAP.keys():
    #        return "Fit{}::parse(inp, parse_config)".format(rustify_name(self.type))
    #    else:
    #        return "FitField{}::parse(inp, parse_config)".format(rustify_name(self.type))
           

def parse_messages_file(messages_file_name, types):

    messages = {}

    f = open(messages_file_name, 'rt')
    lines = []
    csvreader = csv.reader(f)
    for row in csvreader:
        lines.append(row)

    current_message = None
    for line in lines[1:]:
        # section headers in the CSV
        if "MESSAGES" in line[3]:
            continue
        # empty lines
        if all([x == '' for x in line]):
            continue

        if line[0] != '':

            # before moving on from the current message, resolve any
            # subfield types

            if current_message:
                for field in messages[current_message].fields:
                    if not field.subfields:
                        continue
                    for sf in field.subfields:
                        ref_field = [f for f in messages[current_message].fields if f.name == sf.ref_field_name]
                        if not len(ref_field):
                            print("error {}").format(current_message)
                            sys.exit(1)
                        else:
                            sf.ref_field_type = ref_field[0].type

            current_message = line[0]
            message_comment = line[13]

            messages[current_message] = Message(current_message, message_comment)

        else:
            _, field_number, field_name, field_type, array, \
            components, scale, offset, units, bits, _, \
            ref_field_name, ref_field_value, comment, _, _, _, _, _ = line

            parsed_components = []
            parsed_bits = []
            if components:
                parsed_components = [x.strip() for x in components.split(',')]
                parsed_bits = [int(x) for x in bits.split(',')]

            parsed_scale = []
            if scale:
                parsed_scale = [x.strip() for x in scale.split(',')]

            parsed_offset = []
            if offset:
                parsed_offset = [x.strip() for x in offset.split(',')]

            parsed_units = []
            if units:
                parsed_units = [x.strip() for x in units.split(',')]

            if field_number == '':
                if ref_field_name != '':  # this is a dynamic field
                    ref_fields = [x.strip() for x in ref_field_name.split(',')]
                    ref_values = [x.strip() for x in ref_field_value.split(',')]

                    for i in range(0, len(ref_fields)):

                        messages[current_message].add_subfield(
                            Subfield(field_name, field_type, ref_fields[i], ref_values[i], parsed_components, parsed_bits, parsed_scale, parsed_offset, parsed_units, types)
                        )
                continue

            if field_name == 'type':  # reserved word in rust
                field_name = 'ftype'

            messages[current_message].add_field(
                Field(field_number, field_name, field_type, array.strip(), parsed_components, parsed_bits, parsed_scale, parsed_offset, parsed_units, comment, types)
            )

    return messages

def main():
    types_file_path = sys.argv[1]
    messages_file_path = sys.argv[2]

    types = parse_types_file(types_file_path)
    messages = parse_messages_file(messages_file_path, types)

    output_types(types)
    output_messages(messages, types)

if __name__ == '__main__':
    main()
