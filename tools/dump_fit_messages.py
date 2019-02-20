#!/usr/bin/env - python

import csv
import sys
import inflect # for number -> words conversion

from jinja2 import Template, Environment

FIT_TYPE_MAP = {
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
    #'byte': "&'a [u8]",
    'float32': 'f32',
    'float64': 'f64',

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

FIT_BASE_TYPES = ['uint8', 'uint8z', 'uint16', 'uint16z', 'uint32', 'uint32z', 'sint8', 'sint16', 'sint32', 'float32', 'float64', 'string', 'byte']
FIT_BASE_NUMERIC_TYPES = ['uint8', 'uint8z', 'uint16', 'uint16z', 'uint32', 'uint32z', 'sint8', 'sint16', 'sint32', 'float32', 'float64']


TYPE_TEMPLATE = """
{% set base_type = types[this_type]["base_type"] %}
{% set base_rust_type = fit_type_map[base_type] %}
{% set fields = types[this_type]["fields"] %}

{% if base_type not in ['string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8'] -%}
    {% set endianness_clause_full = ', endianness: Endianness' -%}
    {% set endianness_clause_pass = ', endianness' -%}
{% else -%}
    {% set endianness_clause_full = '' -%}
    {% set endianness_clause_pass = '' -%}
{% endif %}

#[derive(Debug, PartialEq)]
pub enum {{ type_name }} { // fit base type: {{ base_type }}
{%- for field in fields if not field["value_name"][0].isdigit() %}
    {{ rustify_name(field["value_name"]) }} = {{ field["value"]}},
    {%- if field["comment"] %}  // {{ field["comment"] }}{% endif %}
{%- endfor %}
}

impl {{ type_name }} {
    pub fn parse(input: &[u8]{{ endianness_clause_full}}) -> Result<({{ type_name }}, &[u8])> {
        let (val, o) = parse_{{ base_type }}(input{{ endianness_clause_pass }})?;
        {% if base_type[-1] == 'z' -%}
        match val {
            Some(nonzero_val) => Ok(({{ type_name }}::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        {% else -%}
        Ok(({{ type_name }}::from(val), o))
        {%- endif %}
    }
}

impl From<{{ base_rust_type }}> for {{ type_name }} {
    fn from(code: {{ base_rust_type }}) -> Self {
        match code {
        {%- for field in fields %}
            {{ field['value'] }} => {{ type_name }}::{{ field['rustified_value_name'] }},
        {%- endfor %}
            invalid_field_num => panic!(format!("invalid field_num {} for {{ type_name }}", invalid_field_num))
        }
    }
}
"""

def resolve_field_size(field_type, types):
    if field_type in FIT_BASE_TYPES:
        return RUST_TYPE_SIZE_MAP[field_type]
    else:
        return RUST_TYPE_SIZE_MAP[types[field_type]['base_type']]

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

use std::rc::Rc;

use nom::Endianness;

use chrono::{DateTime, UTC, FixedOffset, TimeZone, Duration};

use FitRecordHeader;
use FitDefinitionMessage;
use FitFieldDefinition;
use FitFieldDeveloperData;
use fitparsingstate::FitParsingState;
use fitparsers::{parse_enum, parse_uint8, parse_uint8z, parse_sint8, parse_bool, parse_sint16, parse_uint16, parse_uint16z, parse_uint32, parse_uint32z, parse_sint32, parse_byte, parse_string, parse_float32, parse_date_time};

use subset_with_pad;

use errors::{Error, Result};

#[derive(Copy, Clone, Debug)]
pub struct FitFieldDateTime {
    seconds_since_garmin_epoch: u32,
    rust_time: DateTime<UTC>,
}

impl FitFieldDateTime {
    fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldDateTime, &[u8])> {
        let (dt, garmin_epoch_offset, o) = parse_date_time(input, endianness)?;
        Ok((FitFieldDateTime{
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: dt
        }, o))
    }

    #[allow(dead_code)]
    fn new_from_offset(&self, _offset_secs: u8) -> FitFieldDateTime {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let garmin_epoch_offset = self.seconds_since_garmin_epoch + (_offset_secs as u32);
        let rust_time = garmin_epoch + Duration::seconds(garmin_epoch_offset.into());
        FitFieldDateTime{
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: rust_time,
        }
    }

}

#[derive(Debug)]
pub struct FitFieldLocalDateTime {
    seconds_since_garmin_epoch: u32,
    rust_time: DateTime<FixedOffset>,
}

impl FitFieldLocalDateTime {
    fn parse(input: &[u8], endianness: Endianness, _offset_secs: f64) -> Result<(FitFieldLocalDateTime, &[u8])> {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let (garmin_epoch_offset, o) = parse_uint32(input, endianness)?;
        let local_dt = FixedOffset::east(_offset_secs as i32).timestamp(
            (garmin_epoch + Duration::seconds(garmin_epoch_offset.into())).timestamp(),
            0 // nanosecs
        );

        Ok((FitFieldLocalDateTime{
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: local_dt
        }, o))
    }
}

    """

    sys.stdout.write(special_types)
    sys.stdout.write("\n")

    for this_type in types:
        # these need special handling, see above
        if this_type in ['date_time', 'local_date_time']:
            continue

        type_name = "FitField{}".format(rustify_name(this_type))

        template = Environment().from_string(TYPE_TEMPLATE, globals={'rustify_name': rustify_name, 'field_name': field_name})
        content = template.render(this_type=this_type, type_name=type_name, types=types, fit_type_map=FIT_TYPE_MAP)
        sys.stdout.write(content)


def parse_types_file(types_file_name):
    types = {}

    f = open(sys.argv[1], 'rb')
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
    {% for mn in message_names %}
    {{ mn }}(Rc<FitMessage{{ mn }}>),
    {%- endfor %}
}

impl FitDataMessage {
    pub fn parse<'a>(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState, _offset_secs: Option<u8>) -> Result<(FitDataMessage, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        match definition_message.global_mesg_num {
            {% for mesg in mesgs %}
            FitFieldMesgNum::{{ mesg }} => {
                let (val, o) = FitMessage{{ mesg }}::parse(input, header, parsing_state, _offset_secs)?;
                Ok((FitDataMessage::{{ mesg }}(val), o))
            },
            {%- endfor %}
            _ => Err(Error::unknown_error())
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
                                         globals={'message_names': message_names,
                                                  'mesgs': mesgs})
    sys.stdout.write(template.render())

class Message(object):
    STRUCT_TEMPLATE = """
#[derive(Debug)]
pub struct {{ message_name }} {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData>,
    pub raw_bytes: Vec<u8>,
    pub message_name: &'static str,
    {% for field in fields -%}
    pub {{ field.name }}: {{ field.output_field_option() }},  {% if field.comment %}// {{ field.comment }}{% endif %}
    {% endfor %}
}
"""

    IMPL_TEMPLATE = """
impl {{ message_name }} {

    pub fn parse<'a>(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState, _offset_secs: Option<u8>) -> Result<(Rc<{{ message_name }}>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = {{ message_name }} {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            raw_bytes: Vec::with_capacity(definition_message.message_size),
            message_name: "{{ message_name }}",
            {%- for field in fields %}
            {{ field.name }}: None,
            {%- endfor %}
        };

        let inp = &input[..(message.definition_message.message_size)];
        message.raw_bytes.resize(message.definition_message.message_size, 0);
        message.raw_bytes.copy_from_slice(inp);
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match {{ message_name }}::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing {{ message_name }}:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        {% if has_timestamp_field %}
        match _offset_secs {
            Some(os) => {
                message.timestamp = Some(parsing_state.get_last_timestamp()?.new_from_offset(os));
            },
            None => {
                match message.timestamp {
                    Some(ts) => {
                        parsing_state.set_last_timestamp(ts);
                    },
                    None => return Err(Error::missing_timestamp_field())
                }
            }
        }
        {% endif %}

        let mut inp2 = o;
        for dev_field in &message.definition_message.developer_field_definitions {
            let dev_data_definition = parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;
            let field_description = dev_data_definition.get_field_description(dev_field.definition_number)?;
            let (dd, outp) = FitFieldDeveloperData::parse(inp2, field_description.clone(), message.definition_message.endianness, dev_field.field_size)?;
            message.developer_fields.push(dd);
            inp2 = outp;
        }

        Ok((Rc::new(message), inp2))
    }

    fn parse_internal<'a>(message: &mut {{ message_name }}, input: &'a [u8], _tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        let mut saved_outp = input;
        for field in &message.definition_message.field_definitions {
            let mut actions: Vec<(FitFieldDefinition, Option<(usize, usize)>)> = vec![(*field, None)];

            while actions.len() > 0 {

                let (f, components_bit_range) = actions.remove(0);

                let _parse_result: Result<()> = match f.definition_number {
                {% for field in fields %}
                    {{ field.number }} => {  // {{ field.name }}
                        let val = match components_bit_range {
                            Some((bit_range_start, num_bits)) => {
                                let bytes = subset_with_pad(&inp[0..f.field_size], bit_range_start, num_bits)?;
                                let (val, _) = {{ field.output_field_parser('&bytes') }}?;
                                val
                            },
                            None => {
                                let (val, outp) = {{ field.output_field_parser('inp') }}?;
                                saved_outp = outp;
                                val
                            }
                        };
                        {{ field.output_parsed_field_assignment() }};
                        {% for action_push in field.output_components_action_pushes() -%}
                        {{ action_push }}
                        {% endfor -%}
                        Ok(())
                    },
                {% endfor %}
                    invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
                };
            }
            inp = saved_outp;
        }
        Ok(inp)
    }
}
"""

    def __init__(self, name, comment):
        self.name = name
        self.comment = comment
        self.fields = []

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

    def output(self):

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
                                                      'fields': self.fields})
        sys.stdout.write(template.render())

    def output_subfields(self):
        for field in self.fields:
            if not field.has_subfields:
                continue

            sys.stdout.write(field.output_subfield_definition())


class Field(object):
    FIELD_OPTION_SUBFIELD = """Option<FitMessage{{ message_name }}Subfield{{ field_name }}>"""
    FIELD_OPTION_BASE_TYPE = """Option<{{ fit_type }}>"""
    FIELD_OPTION_FIT_TYPE = """Option<FitField{{ field_type }}>"""

    def __init__(self, number, name, type, array, components, bits, scale, offset, comment, types):
        self.number = int(number)
        self.name = name
        self.type = type
        self.array = array
        self.components = components
        self.bits = bits
        self.scale = scale
        self.offset = offset
        self.comment = comment
        self.subfields = []
        self.message = None
        self.types = types
        #self.message_name = message_name

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


    FIELD_PARSER_SUBFIELDS = """FitMessage{{ message_name }}Subfield{{ field_name }}::parse(message, {{ bytes_from }}, &field, _tz_offset)"""

    def output_field_parser(self, bytes_from, only_default_case=False, field_variable_name='field'):
        if self.subfields and not only_default_case:
            template = Environment().from_string(self.FIELD_PARSER_SUBFIELDS,
                                                 globals={'message_name': self.message.rustified_name,
                                                          'field_name': self.rustified_name,
                                                          'bytes_from': bytes_from})
            return template.render()

        elif self.type in FIT_TYPE_MAP.keys():
            field_size = self.type in ['byte', 'string']
            endianness = self.type not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']
            template = Environment().from_string(FIELD_PARSER_BASE_TYPE,
                                                 globals={'field_type': self.type,
                                                          'field_size': field_size,
                                                          'endianness': endianness,
                                                          'bytes_from': bytes_from,
                                                          'field_name': field_variable_name})
            return template.render()

        else:
            endianness = self.types[self.type]['base_type'] not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']
            local_date_time = self.type == 'local_date_time'
            template = Environment().from_string(FIELD_PARSER_FIT_TYPE,
                                                 globals={'field_type': rustify_name(self.type),
                                                          'endianness': endianness,
                                                          'local_date_time': local_date_time,
                                                          'bytes_from': bytes_from})
            return template.render()

    def output_components_action_pushes(self):
        if not self.components:
            return ""

        action_pushes = []
        for i in range(len(self.components)):
            for field in self.message.fields:
                if self.components[i] == field.name:

                    print >>sys.stderr, "components[{}]: {}".format(i, self.components[i])
                    print >>sys.stderr, "bits: {}".format(self.bits)

                    field_size = resolve_field_size(field.type, self.types)

                    bit_start = 0
                    x = 0
                    while x < i:
                        bit_start += self.bits[x]
                        x = x + 1

                    action_pushes.append("actions.push( (FitFieldDefinition{{definition_number: {}, field_size: {}, base_type: 0}}, Some(({}, {})) ));".format(field.number, field_size, bit_start, self.bits[i]))
                    # action_push += "actions.push( (message.definition_message.get_field_definition({})?, Some(({}, {})) ));\n".format(field.number, bit_start, self.bits[i])

        return action_pushes

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
            content = template.render()
        elif self.type != 'enum' and self.type in FIT_TYPE_MAP.keys():
            if self.scale and self.type != 'byte':
                content = "Option<f64>"
            else:
                template = Environment().from_string(self.FIELD_OPTION_BASE_TYPE,
                                                     globals={'fit_type': FIT_TYPE_MAP[self.type]})
                content = template.render()

        else:
            template = Environment().from_string(self.FIELD_OPTION_FIT_TYPE,
                                                 globals={'field_type': rustify_name(self.type)})
            content = template.render()

        return content

    def output_parsed_field_assignment(self):
        if self.type[-1] == 'z':
            return "message.{} = val".format(self.name)

        if self.scale and is_fit_base_type(self.type) and not self.subfields and self.type != 'byte':
            if self.offset:
                return "message.{} = Some((val as f64 / {} as f64) - ({} as f64))".format(self.name, self.scale, self.offset)
            else:
                return "message.{} = Some(val as f64 / {} as f64)".format(self.name, self.scale)


        return "message.{} = Some(val)".format(self.name)

    SUBFIELD_TEMPLATE = """
#[derive(Debug)]
pub enum {{ subfield_name }} {
    Default({{ subfield_default_option }}),
    {%- for sf in subfield_enum_options %}
    {{ sf[0] }}({{ sf[1] }}),
    {%- endfor %}
}

impl {{ subfield_name }} {
    fn parse<'a>(message: &{{ message_name }}, inp: &'a [u8], _field: &FitFieldDefinition, _tz_offset: f64) -> Result<({{ subfield_name }},  &'a [u8])> {
        {% for sf_name in subfield_ref_names %}
        match message.{{ sf_name }} {
        {% for sf in subfield_options[sf_name] %}
            Some(FitField{{ sf.ref_field_type_rustified }}::{{ sf.ref_field_value_rustified }}) => {
                let (val, o) = {{ sf.output_field_parser(field.types, field.message.rustified_name) }}?;
                return Ok(({{ subfield_name }}::{{ sf.field_name_rustified }}(val), o))
            },
        {% endfor %}
            _ => (),
        }
        {% endfor %}
        let (val, o) = {{ subfield_default_parser }}?;
        Ok(({{ subfield_name }}::Default(val),o))
    }
}
"""

    def output_subfield_definition(self):
        subfield_name = "FitMessage{}Subfield{}".format(self.message.rustified_name, self.rustified_name)
        subfield_ref_names = set([sf.ref_field_name for sf in self.subfields])
        subfield_options = {}
        for srn in subfield_ref_names:
            subfield_options[srn] = [sf for sf in self.subfields if sf.ref_field_name == srn]

        subfield_default_parser = self.output_field_parser('inp', only_default_case=True, field_variable_name='_field')
        subfield_enum_options = []
        for sf in self.subfields:
            ftn = sf.field_type_name
            if sf.type == 'byte':
                ftn = "Vec<u8>"
                #ftn += "<'a>"
            subfield_enum_options.append((sf.field_name_rustified, ftn))

        lifetime_spec = ''
        # FIXME: this should recurse to all of the base types

        #if self.type == 'byte':
        #    lifetime_spec = "<'a>"
        #else:
        #    for sf in self.subfields:
        #        if sf.ref_field_type == 'byte':
        #            lifetime_spec = "<'a>"
        #            break

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

FIELD_PARSER_BASE_TYPE = """parse_{{ field_type }}({{ bytes_from }}
{%- if field_size -%}, {{ field_name }}.field_size{%- endif -%}
{%- if endianness -%}, message.definition_message.endianness{%- endif -%}
)"""

FIELD_PARSER_FIT_TYPE = """FitField{{ field_type }}::parse({{ bytes_from }}
{%- if endianness -%}, message.definition_message.endianness{%- endif -%}
{%- if local_date_time -%}, _tz_offset{%- endif -%}
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
    def ref_field_value_rustified(self):
        return rustify_name(self.ref_field_value)

    def output_field_parser(self, types, message_name):
        if self.type in FIT_TYPE_MAP.keys():
            field_size = self.type in ['byte', 'string']
            endianness = self.type not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']
            template = Environment().from_string(FIELD_PARSER_BASE_TYPE, globals={'field_type': self.type,
                                                                                  'field_size': field_size,
                                                                                  'endianness': endianness,
                                                                                  'bytes_from': 'inp',
                                                                                  'field_name': '_field'})
            return template.render()

        else:
            endianness = types[self.type]['base_type'] not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']
            local_date_time = self.type == 'local_date_time'
            template = Environment().from_string(FIELD_PARSER_FIT_TYPE, globals={'field_type': rustify_name(self.type),
                                                                                 'endianness': endianness,
                                                                                 'local_date_time': local_date_time,
                                                                                 'bytes_from': 'inp'})
            return template.render()


def parse_messages_file(messages_file_name, types):

    messages = {}

    f = open(messages_file_name, 'rb')
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
                            print "error {}".format(current_message)
                            sys.exit(1)
                        else:
                            sf.ref_field_type = ref_field[0].type

            current_message = line[0]
            message_comment = line[13]

            messages[current_message] = Message(current_message, message_comment)


            # messages[current_message] = {
            #    "comment": message_comment,
            #    "fields": [],
            #    "needs_lifetime_spec": False,
            #    #"global_message_number": FIT_GLOBAL_MESG_NUM[current_message]
            #}

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

            if field_number == '':
                if ref_field_name != '':  # this is a dynamic field
                    ref_fields = [x.strip() for x in ref_field_name.split(',')]
                    ref_values = [x.strip() for x in ref_field_value.split(',')]

                    for i in range(0, len(ref_fields)):

                        messages[current_message].add_subfield(
                            Subfield(field_name, field_type, ref_fields[i], ref_values[i], components, bits, scale, offset)
                        )

                        #messages[current_message]['fields'][-1]['subfields'].append({
                        #    'field_name': field_name,
                        #    'rustified_field_name': rustify_name(field_name),
                        #    'field_type': field_type,
                        #    'ref_field_name': ref_fields[i],
                        #    'ref_field_value': ref_values[i],
                        #    'ref_field_type': None,
                        #    'components': parsed_components,
                        #    'bits': parsed_bits,
                        #    'scale': parsed_scale,
                        #    'offset': offset,
                        #})
                continue

            if field_name == 'type':  # reserved word in rust
                field_name = 'ftype'

            #if field_type == 'byte':
            #    messages[current_message]['needs_lifetime_spec'] = True

            messages[current_message].add_field(
                Field(field_number, field_name, field_type, array, parsed_components, parsed_bits, scale, offset, comment, types)
            )

            #messages[current_message]["fields"].append({
            #    "field_number": int(field_number),
            #    "field_name": field_name,
            #    "field_type": field_type,
            #    "array": array,
            #    "components": parsed_components,
            #    "bits": parsed_bits,
            #    "scale": parsed_scale,
            #    "offset": offset,
            #    "field_comment": comment,
            #    "subfields": []
            #})

    return messages

    #for message in messages.keys():
    #    print "%s (global_mesg_num: %d)" % (message, messages[message]["global_message_number"])
    #    for field in messages[message]["fields"]:
    #        print "  %s" % (field)


def main():
    types_file_path = sys.argv[1]
    messages_file_path = sys.argv[2]

    types = parse_types_file(types_file_path)
    messages = parse_messages_file(messages_file_path, types)

    output_types(types)
    output_messages(messages, types)

if __name__ == '__main__':
    main()
