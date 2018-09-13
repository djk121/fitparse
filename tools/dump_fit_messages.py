#!/usr/bin/env - python

import csv
import sys

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
    'byte': "&'a [u8]",
    'float32': 'f32',
    'float64': 'f64',

}

FIT_BASE_TYPES = ['uint8', 'uint8z', 'uint16', 'uint16z', 'uint32', 'uint32z', 'sint8', 'sint16', 'sint32', 'float32', 'float64', 'string', 'byte']
FIT_BASE_NUMERIC_TYPES = ['uint8', 'uint8z', 'uint16', 'uint16z', 'uint32', 'uint32z', 'sint8', 'sint16', 'sint32', 'float32', 'float64']

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
use FitFieldDeveloperData;
use fitparsingstate::FitParsingState;
use fitparsers::{parse_enum, parse_uint8, parse_uint8z, parse_sint8, parse_bool, parse_sint16, parse_uint16, parse_uint16z, parse_uint32, parse_uint32z, parse_sint32, parse_byte, parse_string, parse_float32, parse_date_time};

use errors::{Error, ErrorKind, Result};

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

    fn new_from_offset(&self, offset_secs: u8) -> FitFieldDateTime {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let garmin_epoch_offset = self.seconds_since_garmin_epoch + (offset_secs as u32);
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
    fn parse(input: &[u8], endianness: Endianness, offset_secs: i32) -> Result<(FitFieldLocalDateTime, &[u8])> {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let (garmin_epoch_offset, o) = parse_uint32(input, endianness)?;
        let local_dt = FixedOffset::east(offset_secs).timestamp(
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

        base_type = types[this_type]["base_type"]
        sys.stdout.write("#[derive(Debug)]\n")
        sys.stdout.write("pub enum FitField%s { // fit base type: %s\n" % (rustify_name(this_type), base_type))
        num_fields = len(types[this_type]["fields"])
        for i in range(0, num_fields):

            field = types[this_type]["fields"][i]
            if field['value_name'][0].isdigit():
                continue
            sys.stdout.write("    %s = %s" % (rustify_name(field["value_name"]), field["value"]))
            if i < (num_fields-1):
                sys.stdout.write(",")
            if field["comment"] != '':
                sys.stdout.write(" // %s" % (field["comment"]))
            sys.stdout.write("\n")
        sys.stdout.write("}\n")
        sys.stdout.write("\n")


        pass_endianness = False
        if base_type not in ['string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']:
            pass_endianness = True

        sys.stdout.write("")
        sys.stdout.write("impl FitField{} {{\n".format(rustify_name(this_type)))
        sys.stdout.write("{}pub fn parse(input: &[u8]".format(" "*4))
        if pass_endianness:
            sys.stdout.write(", endianness: Endianness")
        sys.stdout.write(") -> Result<(FitField{}, &[u8])> {{\n".format(rustify_name(this_type)))
        sys.stdout.write("{}let (val, o) = parse_{}(input".format(" "*8, base_type))
        if pass_endianness:
            sys.stdout.write(", endianness")
        sys.stdout.write(")?;\n")
        if base_type[-1] == 'z':
            sys.stdout.write("{}match val {{\n".format(" "*8))
            sys.stdout.write("{}Some(nonzero_val) => Ok((FitField{}::from(nonzero_val),o)),\n".format(" "*12, rustify_name(this_type)))
            sys.stdout.write("{}None => Err(Error::parse_zero())\n".format(" "*12))
            sys.stdout.write("{}}}\n".format(" "*8))
        else:
            sys.stdout.write("{}Ok((FitField{}::from(val),o))\n".format(" "*8, rustify_name(this_type)))


        #sys.stdout.write("{}let res = match val {{\n".format(" "*8))
        #for i in range(0, num_fields):
        #    field = types[this_type]["fields"][i]

        #    if field['value_name'][0].isdigit():
        #        continue

        #    sys.stdout.write("            %d => FitField%s::%s,\n" % (field["value"], rustify_name(this_type), rustify_name(field["value_name"])))

        #sys.stdout.write("{}invalid_val => return Err(Error::invalid_field_value(invalid_val.to_string()))\n".format(" "*12))
        #sys.stdout.write("{}}};\n\n".format(" "*8))
        #sys.stdout.write("{}Ok((res, o))\n".format(" "*8))
        sys.stdout.write("{}}}\n".format(" "*4))
        sys.stdout.write("}\n")

        sys.stdout.write("impl From<{}> for FitField{} {{\n".format(FIT_TYPE_MAP[base_type], rustify_name(this_type)))
        sys.stdout.write("{}fn from(code: {}) -> Self {{\n".format(" "*4, FIT_TYPE_MAP[base_type]))
        sys.stdout.write("{}match code {{\n".format(" "*8))
        for i in range(0, num_fields):
            field = types[this_type]["fields"][i]

            if field['value_name'][0].isdigit():
                continue

            sys.stdout.write("{}{} => FitField{}::{},\n".format(" "*12, field["value"], rustify_name(this_type), rustify_name(field["value_name"])))
        sys.stdout.write("{}invalid_field_num => panic!(format!(\"invalid field num {{}} for FitField{}\", invalid_field_num))\n".format(" "*12, rustify_name(this_type)))
        sys.stdout.write("{}}}\n".format(" "*8))
        sys.stdout.write("{}}}\n".format(" "*4))
        sys.stdout.write("}\n\n")

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

            types[current_type]["fields"].append({"value": int(value.strip(), 0),
                                                  "comment": comment.strip(),
                                                  "value_name": value_name.strip()})

    return types

def is_fit_base_type(t):
    return t in FIT_TYPE_MAP.keys()

def fit_base_type_parser(f):

    parse = "parse_{}(inp".format(f['field_type'])
    if f['field_type'] in ['string', 'byte']:
        parse += ", field.field_size"
    elif f['field_type'] not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']:
        parse += ", message.definition_message.endianness"
    parse += ")"
    return parse

def fit_type_parser(f, types):
    # FIXME: don't need endianness for 1-byte types :(

    fit_base_type = types[f['field_type']]['base_type']

    parse = "FitField{}::parse(inp".format(rustify_name(f['field_type']))
    if fit_base_type not in ['bool', 'string', 'byte', 'enum', 'uint8', 'uint8z', 'sint8']:
        parse += ", message.definition_message.endianness"

    if f['field_type'] == 'local_date_time':
        parse += ", tz_offset"
    parse += ")"
    return parse

def output_messages(messages, types):

    messages = sorted(messages.items(), key=lambda m: m[0])

    for this_message, message_def in messages:
        fields = sorted(message_def['fields'], key=lambda f: f['field_number'])
        has_timestamp = 'timestamp' in [f['field_name'] for f in fields]

        # if a struct needs the lifetime spec, it's parse
        lifetime_spec = "<'a>"
        #if message_def['needs_lifetime_spec']:
        #    lifetime_spec = "<'a>"

        sys.stdout.write("#[derive(Debug)]\n")
        sys.stdout.write("pub struct FitMessage{}{} {{\n".format(rustify_name(this_message), lifetime_spec))
        #sys.stdout.write("pub struct FitMessage{}".format(rustify_name(this_message)))
        #if contains_ref:
        #    sys.stdout.write("<'a>")
        #sys.stdout.write(" {\n")
        sys.stdout.write("{}header: FitRecordHeader,\n".format(" "*4))
        sys.stdout.write("{}definition_message: Rc<FitDefinitionMessage>,\n".format(" "*4))
        sys.stdout.write("{}developer_fields: Vec<FitFieldDeveloperData{}>,\n".format(" "*4, lifetime_spec))
        for field in message_def['fields']:
            #sys.stdout.write("{}{}: ".format(" "*4, field['field_name']))
            #if field['field_type'] == 'byte':
            #    sys.stdout.write("&'a ")
            #sys.stdout.write("Option<")
            sys.stdout.write("{}pub {}: Option<".format(" "*4, field['field_name']))
            if field['field_type'] != 'enum' and field['field_type'] in FIT_TYPE_MAP.keys():
                sys.stdout.write("{}".format(FIT_TYPE_MAP[field['field_type']]))
            else:
                sys.stdout.write("FitField{}".format(rustify_name(field['field_type'])))

            sys.stdout.write(">,")
            if field['field_comment'] is not None:
                sys.stdout.write("  // {}".format(field['field_comment']))
            sys.stdout.write("\n")
        sys.stdout.write("}")

        sys.stdout.write("\n")

        sys.stdout.write("impl{} FitMessage{}{} {{\n".format(lifetime_spec,
                                                             rustify_name(this_message),
                                                             lifetime_spec))

        sys.stdout.write("{}pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &'a mut FitParsingState, offset_secs: Option<u8>) -> Result<(Rc<FitMessage{}<'a>>, &'a [u8])> {{\n".format(" "*4, rustify_name(this_message), lifetime_spec))
        #if lifetime_spec != '':
        #    sys.stdout.write("{}pub fn parse<'b>(input: &'a [u8], header: FitRecordHeader, parsing_state: &'b mut FitParsingState, offset_secs: Option<u8>) -> Result<(Rc<FitMessage{}<'a>>, &'a [u8])> {{\n".format(" "*4, rustify_name(this_message), lifetime_spec))
        #else:
        #    sys.stdout.write("{}pub fn parse<'a, 'b>(input: &'a [u8], header: FitRecordHeader, parsing_state: &'b mut FitParsingState, offset_secs: Option<u8>) -> Result<(Rc<FitMessage{}>, &'a [u8])> {{\n".format(" "*4, rustify_name(this_message)))

        sys.stdout.write("{}let definition_message = parsing_state.get(header.local_mesg_num())?;\n".format(" "*8))
        sys.stdout.write("{}let mut message = FitMessage{} {{\n".format(" "*8, rustify_name(this_message)))
        sys.stdout.write("{}header: header,\n".format(" "*12))
        sys.stdout.write("{}definition_message: Rc::clone(&definition_message),\n".format(" "*12))
        sys.stdout.write("{}developer_fields: vec![],\n".format(" "*12))
        for field in message_def['fields']:
            sys.stdout.write("{}{}: None,\n".format(" "*12, field['field_name']))
        sys.stdout.write("{}}};\n".format(" "*8))
        sys.stdout.write("\n")
        sys.stdout.write("{}let inp = &input[..(message.definition_message.message_size)];\n".format(" "*8))
        sys.stdout.write("{}let tz_offset = parsing_state.get_timezone_offset();\n".format(" "*8))
        sys.stdout.write("{}let o = match FitMessage{}::parse_internal(&mut message, input, tz_offset) {{\n".format(" "*8, rustify_name(this_message)))
        sys.stdout.write("{}Ok(o) => o,\n".format(" "*12))
        sys.stdout.write("{}Err(e) => {{\n".format(" "*12))
        sys.stdout.write("{}let mut err_string = String::from(\"Error parsing FitMessage{}:\");\n".format(" "*16, rustify_name(this_message)))
        sys.stdout.write("{}err_string.push_str(&format!(\"  parsing these bytes: '{{:x?}}'\", inp));\n".format(" "*16))
        sys.stdout.write("{}err_string.push_str(&format!(\"  specific error: {{:?}}\", e));\n".format(" "*16))
        sys.stdout.write("{}return Err(Error::message_parse_failed(err_string))\n".format(" "*16))
        sys.stdout.write("{}}}\n".format(" "*12))
        sys.stdout.write("{}}};\n\n".format(" "*8))

        # Compressed Timestamp Header, timestamp won't be provided in the message.
        # If this is a Normal Header (i.e., offset_secs is None), and we got
        # a timestamp field after parsing, update the parsing state. Otherwise,
        # it's an error.
        if has_timestamp:
            sys.stdout.write("{}match offset_secs {{\n".format(" "*8))
            sys.stdout.write("{}Some(os) => {{\n".format(" "*12))
            sys.stdout.write("{}message.timestamp = Some(parsing_state.get_last_timestamp()?.new_from_offset(os));\n".format(" "*16))
            #sys.stdout.write("{}()".format(" "*16))
            sys.stdout.write("{}}},\n".format(" "*12))
            sys.stdout.write("{}None => {{\n".format(" "*12))
            sys.stdout.write("{}match message.timestamp {{\n".format(" "*16))
            sys.stdout.write("{}Some(ts) => {{\n".format(" "*20))

            sys.stdout.write("{}parsing_state.set_last_timestamp(ts);\n".format(" "*24))
            #sys.stdout.write("{}()".format(" "*24))

            sys.stdout.write("{}}},\n".format(" "*20))
            sys.stdout.write("{}None => return Err(Error::missing_timestamp_field())\n".format(" "*20))
            sys.stdout.write("{}}}\n".format(" "*16))
            sys.stdout.write("{}}}\n".format(" "*12))
            sys.stdout.write("{}}}\n".format(" "*8))

        sys.stdout.write("{}let mut inp2 = o;\n".format(" "*8))
        sys.stdout.write("{}for dev_field in &message.definition_message.developer_field_definitions {{\n".format(" "*8))
        sys.stdout.write("{}let dev_data_definition = parsing_state.get_developer_data_definition(dev_field.developer_data_index)?;\n".format(" "*12))
        sys.stdout.write("{}let field_description = dev_data_definition.get_field_description(dev_field.definition_number)?;\n".format(" "*12))
        sys.stdout.write("{}let (dd, outp) = FitFieldDeveloperData::parse(inp, field_description.clone(), message.definition_message.endianness, dev_field.field_size)?;\n".format(" "*12))
        sys.stdout.write("{}message.developer_fields.push(dd);\n".format(" "*12))
        sys.stdout.write("{}inp2 = outp;\n".format(" "*12))
        sys.stdout.write("{}}}\n\n".format(" "*8))



        sys.stdout.write("{}Ok((Rc::new(message), inp2))\n".format(" "*8))

        sys.stdout.write("{}}}\n".format(" "*4))


        sys.stdout.write("{}fn parse_internal(message: &mut FitMessage{}<'a>, input: &'a [u8], tz_offset: i32) -> Result<&'a [u8]> {{\n".format(" "*4, rustify_name(this_message)))

        #if lifetime_spec != '':
        #    sys.stdout.write("{}fn parse_internal<'b>(message: &mut FitMessage{}<'a>, input: &'a [u8], parsing_state: &'b mut FitParsingState, offset_secs: Option<u8>) -> Result<&'a [u8]> {{\n".format(" "*4, rustify_name(this_message)))
        #else:
        #    sys.stdout.write("{}fn parse_internal<'a, 'b>(message: &mut FitMessage{}, input: &'a [u8], parsing_state: &'b mut FitParsingState, offset_secs: Option<u8>) -> Result<&'a [u8]> {{\n".format(" "*4, rustify_name(this_message)))


        sys.stdout.write("{}let mut inp = input;\n".format(" "*8, ))

        sys.stdout.write("{}for field in &message.definition_message.field_definitions {{\n".format(" "*8))
        sys.stdout.write("{}let parse_result: Result<()> = match field.definition_number {{\n".format(" "*12))

        for field in fields:

            sys.stdout.write("{}{} => {{ // {}\n".format(" "*16, field['field_number'], field['field_name']))

            # fields in Fit Messages can be either base types (uint16, string) or
            # previously-defined Fit types (MessageIndex). These need to be parsed
            # differently.

            parse_command = ''
            if is_fit_base_type(field['field_type'].lower()):
                parse_command = fit_base_type_parser(field)
            else:
                parse_command = fit_type_parser(field, types)

            sys.stdout.write("{}let (val, outp) = {}?;\n".format(" "*20, parse_command))
            sys.stdout.write("{}inp = outp;\n".format(" "*20))
            if field['field_type'][-1] == 'z':
                sys.stdout.write("{}message.{} = val;\n".format(" "*20, field['field_name']))
            else:
                sys.stdout.write("{}message.{} = Some(val);\n".format(" "*20, field['field_name']))

            sys.stdout.write("{}Ok(())\n".format(" "*20))
            sys.stdout.write("{}}},\n".format(" "*16))


        sys.stdout.write("{}invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))\n".format(" "*16))
        sys.stdout.write("{}}};\n".format(" "*12))
        sys.stdout.write("{}}}\n\n".format(" "*8))



        sys.stdout.write("{}Ok(inp)\n".format(" "*8))
        sys.stdout.write("{}}}\n".format(" "*4))


        #sys.stdout.write("{}fn has_timestamp(&self) -> bool {{\n".format(" "*4))
        #if not has_timestamp:
        #    sys.stdout.write("{}false\n".format(" "*8))
        #else:
        #    sys.stdout.write("{}match self.timestamp {{\n".format(" "*8))
        #    sys.stdout.write("{}Some(ts) => true,\n".format(" "*12))
        #    sys.stdout.write("{}None => false,\n".format(" "*12))
        #    sys.stdout.write("{}}}\n".format(" "*8))
        #sys.stdout.write("{}}}\n".format(" "*4))

        #if has_timestamp:
        #    sys.stdout.write("{}fn get_timestamp(&self) -> Result<FitFieldDateTime> {{\n".format(" "*4))
        #    sys.stdout.write("{}match self.timestamp {{\n".format(" "*8))
        #    sys.stdout.write("{}Some(ts) => Ok(ts),\n".format(" "*12))
        #    sys.stdout.write("{}None => Err(Error::timestamp_not_set_on_message())\n".format(" "*12))
        #    sys.stdout.write("{}}}".format(" "*8))
        #    sys.stdout.write("{}}}\n\n".format(" "*4))


        sys.stdout.write("}\n")
        sys.stdout.write("\n\n")

    sys.stdout.write("#[derive(Debug)]\n")
    sys.stdout.write("pub enum FitDataMessage<'a> {\n")
    for this_message, message_def in messages:
        lifetime_spec = "<'a>"
        #if message_def['needs_lifetime_spec']:
        #    lifetime_spec = "<'a>"
        name = rustify_name(this_message)
        sys.stdout.write("{}{}(Rc<FitMessage{}{}>),\n".format(" "*4, name, name, lifetime_spec))
    sys.stdout.write("}\n")

    sys.stdout.write("impl<'a> FitDataMessage<'a> {\n")
    sys.stdout.write("{}pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &'a mut FitParsingState, offset_secs: Option<u8>) -> Result<(FitDataMessage<'a>, &'a [u8])> {{\n".format(" "*4))
    sys.stdout.write("{}let definition_message = parsing_state.get(header.local_mesg_num())?;\n".format(" "*8))
    sys.stdout.write("{}match definition_message.global_mesg_num {{\n".format(" "*8))

    for mesg_num_field in types['mesg_num']['fields']:

        if mesg_num_field['value_name'] in ['mfg_range_min', 'mfg_range_max']:
            continue

        sys.stdout.write("{}FitFieldMesgNum::{} => {{\n".format(" "*12, rustify_name(mesg_num_field['value_name'])))
        name = rustify_name(mesg_num_field['value_name'])

        sys.stdout.write("{}let (val, o) = FitMessage{}::parse(input, header, parsing_state, offset_secs)?;\n".format(" "*16, name))
        sys.stdout.write("{}Ok((FitDataMessage::{}(val), o))\n".format(" "*16, name))
        sys.stdout.write("{}}},\n".format(" "*12))

    sys.stdout.write("{}_ => Err(Error::unknown_error())\n".format(" "*12))
    sys.stdout.write("{}}}\n".format(" "*8))
    sys.stdout.write("{}}}\n".format(" "*4))
    sys.stdout.write("}\n")

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
            current_message = line[0]
            message_comment = line[13]
            messages[current_message] = {
                "comment": message_comment,
                "fields": [],
                "needs_lifetime_spec": False,
                #"global_message_number": FIT_GLOBAL_MESG_NUM[current_message]
            }

        else:
            _, field_number, field_name, field_type, array, \
            _, _, _, _, _, _, _, _, comment, _, _, _, _, _ = line

            if field_number == '':
                continue

            if field_name == 'type':
                field_name = 'ftype'

            if field_type == 'byte':
                messages[current_message]['needs_lifetime_spec'] = True

            messages[current_message]["fields"].append({
                "field_number": int(field_number),
                "field_name": field_name,
                "field_type": field_type,
                #"rust_type": rust_type,
                "array": array,
                "field_comment": comment,
            })

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
