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
    fn parse(input: &[u8], parse_config: FitParseConfig) -> Result<{{ type_name }}> {
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
    fn parse(input: &[u8], parse_config: FitParseConfig) -> Result<{{ type_name }}> {
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

use nom::Endianness;

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
use fitparsers::{parse_enum, parse_uint8, parse_uint8z, parse_sint8, parse_bool, parse_sint16, parse_uint16, parse_uint16z, parse_uint32, parse_uint32z, parse_sint32, parse_byte, parse_string, parse_float32};

use {vec_fit_field_parseable, fmt_message_field, fmt_raw_bytes, fmt_unknown_fields, fmt_developer_fields, parsing_state_set_timestamp, parse_developer_fields, main_parse_message, parse_subfields, deg_parse_assignment, scale_and_offset_parse_assignment, field_parser_fit_type, field_parser_base_type}; 
use fittypes_utils::{FitFieldDateTime, FitFieldLocalDateTime};
use {FitBool, FitBoolVec, FitEnum, FitEnumVec, FitUint8, FitUint8Vec, FitUint8z, FitUint8zVec, FitSint8, FitSint8Vec, FitUint16, FitUint16Vec, 
     FitUint16z, FitUint16zVec, FitSint16, FitSint16Vec, FitUint32, FitUint32Vec, FitUint32z, FitUint32zVec, FitSint32, FitSint32Vec,
     FitFloat32, FitFloat32Vec, FitUint64, FitUint64Vec, FitUint64z, FitUint64zVec, FitSint64, FitSint64Vec, FitFloat64, FitFloat64Vec,
     FitByte, FitByteVec, FitString, FitStringVec};

use {BasicValue, PreAdjustedValue, AdjustedValue};

use subset_with_pad;

use errors::{Error, Result};
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
}

impl fmt::Display for FitDataMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            {%- for message in messages -%}
            FitDataMessage::{{ message.short_name }}(m) => write!(f, "{}", m),
            {% endfor -%}
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

    pub fn parse<'a>(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState, timestamp: Option<FitFieldDateTime>) -> Result<(Option<FitDataMessage>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        match definition_message.global_mesg_num {
            {% for message in messages %}
            FitGlobalMesgNum::Known(FitFieldMesgNum::{{ message.short_name }}) => {
                let (val, o) =
                    {{ message.full_name }}::parse(input, header, parsing_state, timestamp)?;
                Ok((Some(FitDataMessage::{{ message.short_name }}(val)), o))
            }
            {%- endfor %}
            FitGlobalMesgNum::Unknown(number) => {
                let (val, o) = FitMessageUnknownToSdk::parse(number, input, header, parsing_state, timestamp)?;
                Ok((Some(FitDataMessage::UnknownToSdk(val)), o))
            }
            _ => Ok((None, &input[definition_message.message_size..])),
        }
    }

    pub fn message_name(&self) -> &'static str {
        match self {
            {%- for message in messages -%}
            FitDataMessage::{{ message.short_name }}(_) => "{{ message.short_name }}",
            {% endfor -%}
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
#[path = "./fittypes_test.rs"]
mod fittypes_test;
"""

def output_messages(messages, types):
    messages = sorted(messages.items(), key=lambda m: m[0])

    for this_message, message_def in messages:
        message_def.output()

    message_names = sorted([m[1].rustified_name for m in messages])
    mesgs = sorted([rustify_name(mnf['value_name']) for mnf in types['mesg_num']['fields'] if mnf['value_name'] not in ('mfg_range_min', 'mfg_range_max')])
    template = Environment().from_string(FDM_TEMPLATE,
                                         globals={'messages': [x[1] for x in messages]})
                                         #globals={'message_names': message_names,
                                         #         'mesgs': mesgs})
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
        {% else %}
        fmt_message_field!(self.{{ field.name }}, "{{ field.name }}", {% if field.is_adjusted or field.is_semicircles %}true{% else %}false{% endif %}, f)?;
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

    pub fn parse<'a>(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState, _timestamp: Option<FitFieldDateTime>) -> Result<(Rc<{{ message_name }}>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = {{ message_name }} {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            unknown_fields: HashMap::new(),
            raw_bytes: Vec::with_capacity(definition_message.message_size),
            message_name: "{{ message_name }}",
            {% for field in fields %}
            {%- if field.has_subfields -%}
            {{ field.name }}_subfield_bytes: vec![],
            {% endif -%}
            {{ field.name }}: {{ field.output_message_field() }},
            {% endfor %}
        };

        let o = main_parse_message!(input, message, parsing_state, {{ message_name }});

        {% if has_subfields %}
        parse_subfields!(message, parsing_state, {{ message_name }});
        {% endif %}

        {% if has_timestamp_field %}
        parsing_state_set_timestamp!(message, _timestamp, parsing_state);
        {% endif %}

        let mut inp2 = o;
        parse_developer_fields!(inp2, message, parsing_state);

        Ok((Rc::new(message), inp2))
    }

    {% if has_subfields %}
    fn parse_subfields(message: &mut {{ message_name }}, tz_offset: f64) -> Result<()> {
        {%- for field in fields -%}
        {%- if field.has_subfields %}
        let fds: Vec<_> = message.definition_message.field_definitions.iter().filter(|f| f.definition_number == {{ field.number }}).collect();
        if fds.len() == 1 {
            // let field = fds[0];
            let parse_config = FitParseConfig::new(*fds[0], message.definition_message.endianness, tz_offset);
            let val = {{ field.output_subfield_parser()}}?;
            message.{{ field.name }} = val;
        }
        {%- endif -%}
        {%- endfor %}

        Ok(())
    }
    {% endif %}

    fn parse_internal<'a>(message: &mut {{ message_name }}, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        //let mut saved_outp = input;
        for field in &message.definition_message.field_definitions {

            let orig_field_size = field.field_size;
            let mut actions = vec![FitParseConfig::new(*field, message.definition_message.endianness, tz_offset)];
            //let should_advance_inp = false;

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
                    Some((start, num_bits)) => &subset_with_pad(&inp[0..parse_config.field_size()], 
                        start, num_bits, parse_config.endianness())?
                };
                */

                //println!("parse_config: {:?}", parse_config);
                //println!("parse_input: {:?}", parse_input);

                match parse_config.field_definition_number() {
                {% for field in fields if not field.has_subfields %}
                    {{ field.number }} => {  // {{ field.name }}
                        message.{{ field.name }}.parse(parse_input, parse_config)?;
                        {% if field.has_components %}
                        //let components = {{ field.calculate_components_vec() }};
                        //let actions_extend: Vec<(FitParseConfig, bool)> = components.into_iter().map(|c| (c, false)).collect();
                        actions.extend({{ field.calculate_components_vec() }});
                        {% endif %}
                        //saved_outp = &inp[parse_config.field_size()..];
                    },
                {% endfor %}
                    unknown_field_num => {
                        let val = FitBaseValue::parse(inp, parse_config)?;
                        message.unknown_fields.insert(unknown_field_num, val);
                        //saved_outp = &inp[parse_config.field_size()..];
                    }
                };

                if actions.len() == 0 {
                    //println!("advancing inp by {}", orig_field_size);
                    inp = &inp[orig_field_size..];
                }
            }
            //inp = saved_outp;
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

    def add_field(self, field):
        field.message = self
        self.fields.append(field)

    def add_subfield(self, subfield):
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
                                                      'has_subfields': self.has_subfields})
        sys.stdout.write(template.render())

    def output_subfields(self):
        for field in self.fields:
            if not field.has_subfields:
                continue

            sys.stdout.write(field.output_subfield_definition())


class Field(object):
    #FIELD_OPTION_SUBFIELD = """FitFieldBasicValue<FitMessage{{ message_name }}Subfield{{ field_name }}>"""
    FIELD_OPTION_SUBFIELD = """FitMessage{{ message_name }}Subfield{{ field_name }}"""
    FIELD_OPTION_BASE_TYPE = """FitFieldBasicValue<{% if is_vec %}Vec<{% endif %}{{ fit_type }}{% if is_vec %}>{% endif %}>"""
    #FIELD_OPTION_FIT_TYPE = """FitFieldValue<{% if is_vec %}Vec<{% endif %}FitField{{ field_type }}{% if is_vec %}>{% endif %}>"""
    FIELD_OPTION_FIT_TYPE = """FitFieldBasicValue<{% if is_vec %}Vec<{% endif %}FitField{{ field_type }}{% if is_vec %}>{% endif %}>"""


    def __init__(self, number, name, type, array, components, bits, scale, offset, units, comment, types):
        self.number = int(number)
        self.name = name
        self.type = type
        self.array = (array != '' and type != 'byte')
        #self.components = components
        #self.bits = None
        self.scale = None
        self.offset = None
        self.units = ''
        self.comment = comment
        self.subfields = []
        self.message = None
        self.types = types
        #self.message_name = message_name
        self.is_adjusted = False

        
        self.components = []
        if len(components) > 0:
            #if not (len(components) == len(scale) == len(units)):
            #    print("error parsing field {}: different numbers of components ({}), scale ({}), units ({})".format(name, components, scale, units), file=sys.stderr)
            #    sys.exit(1)

            bit_ranges = self.set_bit_ranges(bits)

            #self.units = []
            for i in range(0, len(components)):
                self.components.append((components[i], bit_ranges[i]))
                
                # not all components have scale/offset
                #this_scale = ''
                #if i+1 <= len(scale):
                #    this_scale = scale[i]
                #this_units = ''
                #if i+1 <= len(units):
                #    this_units = units[i]

                #self.components.append((components[i], this_scale, this_units))
        
        # units apply to this field
        if len(units) == 1:        
            self.units = units[0]

            # only base types can have scale/offset. the weight message
            # type screws this up in the fit profile, so protect here
            if not self.type in FIT_TYPE_MAP.keys():
                return

            if len(scale) == 0 and len(offset) == 0:
                self.is_adjusted = False
            elif len(scale) == 1 and len(offset) == 0:
                self.scale = float(scale[0])
                self.offset = 0.0
                self.is_adjusted = True
            elif len(scale) == 0 and len(offset) == 1:
                self.offset = float(offset[0])
                self.scale = 0.0
                self.is_adjusted = True
            elif len(scale) == 1 and len(offset) == 1:
                self.offset = float(offset[0])
                self.scale = float(scale[0])
                self.is_adjusted = True
            #else:
            #    print("whoops, messed up scale/offset w/o components", file=sys.stderr)
            #    print("field: {}, number: {}".format(self.name, self.number), file=sys.stderr)
            #    sys.exit(1)  

    def set_bit_ranges(self, bits): 
        bit_ranges = []
        range_begin = 0
        for bit_amt in bits:
            range_end = range_begin + bit_amt
            bit_ranges.append((range_begin, bit_amt))
            range_begin = range_end

        return bit_ranges

    @property
    def output_units(self):
        if self.is_semicircles:
            return "deg"
        else:
            if self.units is None:
                return ""
            else:
                #return ",".join(self.units)
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


    #FIELD_PARSER_SUBFIELDS = """FitMessage{{ message_name }}Subfield{{ field_name }}::parse(message, {{ bytes_from }}, &field, _tz_offset)"""
    FIELD_PARSER_SUBFIELD_BYTES = """parse_byte({{ bytes_from }}, field.field_size)"""
    FIELD_PARSER_SUBFIELD = """FitMessage{{ message_name }}Subfield{{ field_name }}::parse(message, {{ bytes_from }}, parse_config)"""


    def output_subfield_parser(self):
        bytes_from = "&message.{}_subfield_bytes".format(self.name)
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

   
    def calculate_components_vec(self):
        # if both components and scale/offset are present, the scale/offset
        # apply to the components, not this field.       
        print("ccv, components = {}".format(self.components), file=sys.stderr)
        components_parts = []
        if len(self.components):
            for i in range(len(self.components)):
                for field in self.message.fields:
                    if self.components[i][0] == field.name:
                        print("found target field: {}".format(field.name), file=sys.stderr)
                        field_size = resolve_field_size(field.type, self.types)
                        base_type_num = resolve_field_type(field.type, self.types)
                        #bit_start = 0
                        #x = 0
                        #while x < i:
                        #    bit_start += self.bits[x]
                        #    x = x + 1
                        (bit_start, bit_len) = self.components[i][1]

                        #components_parts.append("FitParseConfig::new_from_component({}, {}, {}, {}, {})".format(field.number, field_size, field_size, bit_start, self.bits[i]))
                        components_parts.append("FitParseConfig::new_from_component({}, {}, {}, message.definition_message.endianness, {}, {})".format(field.number, field_size, base_type_num, bit_start, bit_len))
        return "vec![{}]".format(",".join(components_parts))

    def output_message_field(self):
        if self.subfields:
            return "FitMessage{}Subfield{}::NotYetParsed".format(rustify_name(self.message.name), rustify_name(self.name))
        
        new_spec = '_single'
        if self.array:
            new_spec = '_vec'

        if ((self.scale or self.offset) and self.type in FIT_TYPE_MAP.keys()) or self.is_semicircles:
            scale = "0.0"
            if self.scale:
                scale = self.scale
            
            offset = "0.0"
            if self.offset:
                offset = self.offset

            return "FitFieldAdjustedValue::new{}(\"{}\".to_string(), {}, {})".format(new_spec, self.output_units, scale, offset)
            #return "FitFieldAdjustedValue {{ value: None, units: \"{}\".to_string(), scale: {}, offset: {} }}".format(self.output_units, scale, offset)
        else:
            return "FitFieldBasicValue::new{}(\"{}\".to_string())".format(new_spec, self.output_units)
            # return "FitFieldBasicValue {{ value: None, units: \"{}\".to_string() }}".format(self.output_units)

    def output_field_option(self):
        content = ""
        if self.subfields:

            lifetime_spec = ""
            if self.subfield_needs_lifetime_spec():
                lifetime_spec = "<'a>"

            template = Environment().from_string(self.FIELD_OPTION_SUBFIELD,
                                                 globals={'message_name': self.message.rustified_name,
                                                          'field_name': self.rustified_name,
                                                          'lifetime_spec': lifetime_spec})
            return template.render()
        #elif self.array:
        #    if (self.scale or self.units == 'semicircles'):
        #        return "Vec<FitFieldAdjustedValue<Fit{}>>".format(rustify_name(self.type))
        #    else:
        #        return "Vec<FitFieldBasicValue<Fit{}>>".format(rustify_name(self.type))

        elif self.type != 'enum' and self.type in FIT_TYPE_MAP.keys():
            name = "Fit{}".format(rustify_name(self.type))
            #if self.array:
            #    name = "Vec<{}>".format(name)
            
            if (self.scale or self.is_semicircles) and self.type != 'byte':
                content = "FitFieldAdjustedValue<{}>".format(name)
            else:
                content = "FitFieldBasicValue<{}>".format(name)
            
            return content

        else:
            return "FitFieldBasicValue<FitField{}>".format(rustify_name(self.type))

            #template = Environment().from_string(self.FIELD_OPTION_FIT_TYPE,
            #                                     globals={'field_type': rustify_name(self.type),
            #                                              'is_vec': self.array})
            #return template.render()


    def output_field_container(self):
        array_spec = "_single"
        if self.array:
            array_spec = "_vec"
        if self.is_adjusted or self.is_semicircles:
            return "FitFieldAdjustedValue::new{}(\"{}\".to_string(), {}, {})".format(array_spec, self.units, self.scale, self.offset)
        else:
            print("units: {}".format(self.units), file=sys.stderr)
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
    fn parse<'a>(message: &{{ message_name }}, inp: &'a [u8], parse_config: FitParseConfig) -> Result<{{ subfield_name }}> {
        {% for sf_name in subfield_ref_names %}
        match message.{{ sf_name }}.get_single()? {
        {% for sf in subfield_options[sf_name] %}
            FitField{{ sf.ref_field_type_rustified }}::{{ sf.ref_field_value_rustified }} => {
                let val = {{ sf.output_field_parser(field.types, field.message.rustified_name) }}?;
                return Ok({{ subfield_name }}::{{ sf.field_name_rustified }}(val))
            },
        {% endfor %}
            _ => (),
        }
        {% endfor %}
        let val = {{ subfield_default_parser }}?;
        Ok({{ subfield_name }}::Default(val))
    }
}
"""

    def output_subfield_definition(self):
        subfield_name = "FitMessage{}Subfield{}".format(self.message.rustified_name, self.rustified_name)
        subfield_ref_names = set([sf.ref_field_name for sf in self.subfields])
        subfield_options = {}
        for srn in subfield_ref_names:
            subfield_options[srn] = [sf for sf in self.subfields if sf.ref_field_name == srn]

        #subfield_default_parser = self.output_field_parser('inp', only_default_case=True, field_variable_name='f')
        
        subfield_default_parser = ''
        if self.type in FIT_TYPE_MAP:
            subfield_default_parser = "Fit{}::parse(inp, parse_config)".format(rustify_name(self.type))
        else:
            subfield_default_parser = "FitField{}::parse(inp, parse_config)".format(rustify_name(self.type))
        subfield_enum_options = []
        for sf in self.subfields:

            ftn = ''
            if sf.type in FIT_TYPE_MAP:
                ftn = "{}".format(FIT_TYPE_MAP[sf.type])
            else:
                ftn = "{}".format(sf.field_type_name)

            #ftn = "Option<{}>".format(sf.field_type_name)
            #if sf.type == 'byte':
            #    ftn = "Option<Vec<u8>>"
                #ftn += "<'a>"
            subfield_enum_options.append((sf.field_name_rustified, ftn))

        lifetime_spec = ''
        if self.subfield_needs_lifetime_spec():
            lifetime_spec = "<'a>"

        template = Environment().from_string(self.SUBFIELD_TEMPLATE,
                                             globals={'subfield_name': subfield_name,
                                                      'subfield_default_option': self.field_type_name,
                                                      'field': self,
                                                      'subfield_options': subfield_options,
                                                      'message_name': self.message.message_name,
                                                      'subfield_enum_options': set(subfield_enum_options),
                                                      'subfield_ref_names': subfield_ref_names,
                                                      'lifetime_spec': lifetime_spec,
                                                      'subfield_default_parser': subfield_default_parser})
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

class Subfield(object):
    def __init__(self, field_name, field_type, ref_field_name, ref_field_value, components, bits, scale, offset):
        self.field_name = field_name
        self.type = field_type
        self.ref_field_name = ref_field_name
        self.ref_field_type = None
        self.ref_field_value = ref_field_value
        self.components = components
        self.bits = bits
        self.scale = scale
        self.offset = offset


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

    def output_field_parser(self, types, message_name):


        if self.type in FIT_TYPE_MAP.keys():
            return "Fit{}::parse(inp, parse_config)".format(rustify_name(self.type))
            #return "parse_{}(inp, parse_config)".format(self.type)
            #if self.type not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']:
            #    return "field_parser_base_type!(\"{}\", inp, f, message)".format(self.type)
            #else:
            #    return "field_parser_base_type!(\"{}\", inp, f)".format(self.type)


        else:
            return "FitField{}::parse(inp, parse_config)".format(rustify_name(self.type))
            #endianness = types[self.type]['base_type'] not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']
            #local_date_time = self.type == 'local_date_time'
           
            #if endianness and local_date_time:
            #    return "field_parser_fit_type!(FitField{}, inp, f, message, _tz_offset)".format(rustify_name(self.type))
            #elif endianness:
            #    return "field_parser_fit_type!(FitField{}, inp, f, message)".format(rustify_name(self.type))
            #else:
            #    return "field_parser_fit_type!(FitField{}, inp, f)".format(rustify_name(self.type))




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
                    #print "field: {}".format(field.name)
                    if not field.subfields:
                        continue
                    for sf in field.subfields:
                        #print "sf rfn: {}".format(sf.ref_field_name)
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
                if components == 'enhanced_max_speed':
                    print("enhanced_max_speed", file=sys.stderr)
                parsed_components = [x.strip() for x in components.split(',')]
                if components == 'enhanced_max_speed':
                    print("parsed_components: {}".format(parsed_components), file=sys.stderr)
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
                            Subfield(field_name, field_type, ref_fields[i], ref_values[i], components, bits, scale, offset)
                        )
                continue

            if field_name == 'type':  # reserved word in rust
                field_name = 'ftype'

            #if field_name == 'bottom_time':
            #    print("bottom_time array: '{}'".format(array), file=sys.stderr)

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
