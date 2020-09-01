use super::*;

use std::rc::Rc;

use nom::number::Endianness;

use FitBaseValue;
use FitDeveloperFieldDefinition;
use FitNormalRecordHeader;
use FitNormalRecordHeaderMessageType;

use {AdjustedValue, PreAdjustedValue};

use FitFloat64;

fn make_definition_message_record() -> FitDefinitionMessage {
    FitDefinitionMessage {
        header: FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        },
        endianness: Endianness::Little,
        global_mesg_num: FitGlobalMesgNum::Known(FitFieldMesgNum::Record),
        num_fields: 11,
        message_size: 28,
        field_definitions: make_field_definitions(vec![
            (253, 4, 134),
            (0, 4, 133),
            (1, 4, 133),
            (5, 4, 134),
            (2, 2, 131),
            (6, 2, 131),
            (7, 2, 131),
            (3, 1, 2),
            (4, 1, 2),
            (39, 2, 131),
            (41, 2, 131),
        ]),
        num_developer_fields: 0,
        developer_field_definitions: vec![],
    }
}

fn make_definition_message_event() -> FitDefinitionMessage {
    FitDefinitionMessage {
        header: FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        },
        endianness: Endianness::Big,
        global_mesg_num: FitGlobalMesgNum::Known(FitFieldMesgNum::Event),
        num_fields: 3,
        message_size: 9,
        field_definitions: make_field_definitions(vec![
            (253, 4, 134), // timestamp, uint32
            (0, 1, 0), // event, enum
            (3, 4, 134), // data, uint32
        ]),
        num_developer_fields: 0,
        developer_field_definitions: vec![],
    }
}

fn make_field_definitions(definitions: Vec<(u8, usize, u8)>) -> Vec<FitFieldDefinition> {
    definitions
        .iter()
        .map(
            |(definition_number, field_size, base_type)| {
                println!("dn: {}, fs: {}, bt: {}", definition_number, field_size, base_type);
                FitFieldDefinition::new(*definition_number, *field_size, *base_type).unwrap()
            }
        )
        .collect()
}

macro_rules! ffbv {
    ("unparsed", $ty:ty, "single") => {
        FitFieldBasicValue {
            value: BasicValue::<$ty>::NotYetParsedSingle,
            units: "".to_string(),
            components: vec![],
        }
    };
    ("unparsed", $ty:ty, "vec") => {
        FitFieldBasicValue {
            value: BasicValue::<$ty>::NotYetParsedVec,
            units: "".to_string(),
            components: vec![],
        }
    };
    ($s:expr, $ty:ty, "single") => {
        FitFieldBasicValue {
            value: BasicValue::<$ty>::Single($s),
            units: "".to_string(),
            components: vec![],
        }
    };
    ($s:expr, $ty:ty, "vec") => {
        FitFieldBasicValue {
            value: BasicValue::<$ty>::Vec($s),
            units: "".to_string(),
            components: vec![],
        }
    };
    ($s:expr, $ty:ty, $u:expr, "single") => {
        FitFieldBasicValue {
            value: BasicValue::<$ty>::Single($s),
            units: $u.to_string(),
            components: vec![],
        }
    };
    ($s:expr, $ty:ty, $u:expr, "vec") => {
        FitFieldBasicValue {
            value: BasicValue::<$ty>::Vec($s),
            units: $u.to_string(),
            components: vec![],
        }
    };
}

macro_rules! ffav {
    ("unparsed", $ty:ty, $scale:expr, $offset:expr, "single") => {
        FitFieldAdjustedValue {
            value: AdjustedValue::NotYetParsedSingle,
            parsed_value: PreAdjustedValue::<$ty>::NotYetParsedSingle,
            units: "".to_string(),
            scale: $scale,
            offset: $offset,
            components: vec![],
        }
    };
    ("unparsed", $ty:ty, $scale:expr, $offset:expr, "vec") => {
        FitFieldAdjustedValue {
            value: AdjustedValue::NotYetParsedVec,
            parsed_value: PreAdjustedValue::<$ty>::NotYetParsedVec,
            units: "".to_string(),
            scale: $scale,
            offset: $offset,
            components: vec![],
        }
    };
    ($preadjusted_value:expr, $value:expr, $ty:ty, $scale:expr, $offset:expr, $units:expr, "single") => {
        FitFieldAdjustedValue {
            value: AdjustedValue::Single($value),
            parsed_value: PreAdjustedValue::<$ty>::Single($preadjusted_value),
            units: $units,
            scale: $scale,
            offset: $offset,
            components: vec![],
        }
    };
    ($preadjusted_value:expr, $value:expr, $ty:ty, $scale:expr, $offset:expr, $units:expr, "vec") => {
        FitFieldAdjustedValue {
            value: AdjustedValue::Vec($value),
            parsed_value: PreAdjustedValue::<$ty>::Vec($preadjusted_value),
            units: $units,
            scale: $scale,
            offset: $offset,
            components: vec![],
        }
    };
}

fn make_field_description(
    field_name: Vec<FitString>,
    units: Vec<FitString>,
    field_definition_number: FitUint8,
    native_mesg_num: Option<FitFieldMesgNum>,
    fit_base_type_id: FitFieldFitBaseType,
    field_definitions: Vec<(u8, usize, u8)>,
) -> Rc<FitMessageFieldDescription> {
    let nmn = match native_mesg_num {
        None => ffbv!("unparsed", FitFieldMesgNum, "single"),
        Some(x) => ffbv!(x, FitFieldMesgNum, "single"),
    };

    Rc::new(FitMessageFieldDescription {
        header: FitRecordHeader::Normal(FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Data,
            developer_fields_present: false,
            local_mesg_num: 0,
        }),
        definition_message: Rc::new(FitDefinitionMessage {
            header: FitNormalRecordHeader {
                message_type: FitNormalRecordHeaderMessageType::Definition,
                developer_fields_present: false,
                local_mesg_num: 0,
            },
            endianness: Endianness::Little,
            global_mesg_num: FitGlobalMesgNum::Known(FitFieldMesgNum::FieldDescription),
            num_fields: field_definitions.len() as u8,
            message_size: field_definitions
                .iter()
                .map(|(_, field_size, _)| field_size)
                .sum(),
            field_definitions: make_field_definitions(field_definitions),
            num_developer_fields: 0,
            developer_field_definitions: vec![],
        }),
        developer_fields: vec![],
        unknown_fields: HashMap::new(),
        raw_bytes: vec![],
        subfield_field_numbers: vec![],
        message_name: "FitMessageFieldDescription",
        developer_data_index: ffbv!(FitUint8::new(0), FitUint8, "single"),
        field_definition_number: ffbv!(field_definition_number, FitUint8, "single"),
        fit_base_type_id: ffbv!(fit_base_type_id, FitFieldFitBaseType, "single"),
        field_name: ffbv!(field_name, FitString, "vec"),
        array: ffbv!("unparsed", FitUint8, "single"),
        components: ffbv!("unparsed", FitString, "single"),
        scale: ffbv!("unparsed", FitUint8, "single"),
        offset: ffbv!("unparsed", FitSint8, "single"),
        units: ffbv!(units, FitString, "vec"),
        bits: ffbv!("unparsed", FitString, "single"),
        accumulate: ffbv!("unparsed", FitString, "single"),
        fit_base_unit_id: ffbv!("unparsed", FitFieldFitBaseUnit, "single"),
        native_mesg_num: nmn,
        native_field_num: ffbv!("unparsed", FitUint8, "single"),
    })
}

#[test]
fn fit_message_record() {
    let definition_message = Rc::new(make_definition_message_record());
    let data = [
        0b00101011, 0b00111100, 0b10101001, 0b00110011, 0b10100000, 0b01111001, 0b01000101,
        0b00011110, 0b11000000, 0b01111101, 0b01111110, 0b11001101, 0b10011000, 0b00001000,
        0b00010011, 0b00000000, 0b01010001, 0b00001000, 0b01010111, 0b00001101, 0b11010001,
        0b00000000, 0b10010111, 0b10110110, 0b10010111, 0b00000010, 0b00010000, 0b00001001,
    ];

    let mut parsing_state = FitParsingState::new();
    parsing_state.add(0, definition_message.clone());

    let header = FitRecordHeader::Normal(FitNormalRecordHeader {
        message_type: FitNormalRecordHeaderMessageType::Data,
        developer_fields_present: false,
        local_mesg_num: 0,
    });

    let mut rec = FitMessageRecord::new(header, &mut parsing_state).unwrap();
    rec.parse(&data, &mut parsing_state, None).unwrap();

    assert_eq!(
        rec.position_lat,
        ffav!(
            FitSint32::new(507869600),
            FitFloat64::new(42.56913810968399),
            FitSint32,
            0.0,
            0.0,
            "deg".to_string(),
            "single"
        )
    );
    assert_eq!(
        rec.position_long,
        ffav!(
            FitSint32::new(-847348288),
            FitFloat64::new(-71.02391302585602),
            FitSint32,
            0.0,
            0.0,
            "deg".to_string(),
            "single"
        )
    );
    assert_eq!(
        rec.heart_rate,
        ffbv!(FitUint8::new(151), FitUint8, "bpm", "single")
    );
    assert_eq!(
        rec.power,
        ffbv!(FitUint16::new(209), FitUint16, "watts", "single")
    );
}

#[test]
fn fit_message_record_with_developer_fields() {
    let mut definition_message = make_definition_message_record();

    definition_message.num_fields = 11;
    definition_message.message_size = 42;
    definition_message.num_developer_fields = 4;
    definition_message.developer_field_definitions = vec![
        FitDeveloperFieldDefinition {
            definition_number: 8,
            field_size: 2,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 9,
            field_size: 4,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 6,
            field_size: 4,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 5,
            field_size: 4,
            developer_data_index: 0,
        },
    ];

    let definition_message_final = Rc::new(definition_message);

    let data = [
        0b00101011, 0b00111100, 0b10101001, 0b00110011, 0b10100000, 0b01111001, 0b01000101,
        0b00011110, 0b11000000, 0b01111101, 0b01111110, 0b11001101, 0b10011000, 0b00001000,
        0b00010011, 0b00000000, 0b01010001, 0b00001000, 0b01010111, 0b00001101, 0b11010001,
        0b00000000, 0b10010111, 0b10110110, 0b10010111, 0b00000010, 0b00010000, 0b00001001,
        0b00111001, 0b00000000, 0b00000000, 0b00000000, 0b00001000, 0b01000001, 0b00000000,
        0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000,
    ];

    let mut parsing_state = FitParsingState::new();
    parsing_state.add(0, definition_message_final.clone());

    let developer_field_descriptions = vec![
        (
            vec![FitString::new("Form Power".to_string())],
            vec![FitString::new("Watts".to_string())],
            FitUint8::new(8),
            None,
            FitFieldFitBaseType::Uint16,
            vec![(0, 1, 2), (1, 1, 2), (2, 1, 2), (3, 11, 7), (8, 6, 7)],
        ),
        (
            vec![FitString::new("Leg Spring Stiffness".to_string())],
            vec![FitString::new("KN/m".to_string())],
            FitUint8::new(9),
            None,
            FitFieldFitBaseType::Float32,
            vec![(0, 1, 2), (1, 1, 2), (2, 1, 2), (3, 21, 7), (8, 5, 7)],
        ),
        (
            vec![FitString::new("Distance".to_string())],
            vec![FitString::new("Meters".to_string())],
            FitUint8::new(6),
            Some(FitFieldMesgNum::SdmProfile),
            FitFieldFitBaseType::Uint32,
            vec![
                (0, 1, 2),
                (1, 1, 2),
                (14, 2, 4),
                (2, 1, 2),
                (3, 9, 7),
                (8, 7, 7),
            ],
        ),
        (
            vec![FitString::new("Speed".to_string())],
            vec![FitString::new("M/S".to_string())],
            FitUint8::new(5),
            Some(FitFieldMesgNum::BikeProfile),
            FitFieldFitBaseType::Float32,
            vec![
                (0, 1, 2),
                (1, 1, 2),
                (14, 2, 4),
                (2, 1, 2),
                (3, 6, 7),
                (8, 4, 7),
            ],
        ),
    ];

    for dfd_spec in developer_field_descriptions {
        println!("dfd_spec: {:?}", dfd_spec);
        let fd = make_field_description(
            dfd_spec.0, dfd_spec.1, dfd_spec.2, dfd_spec.3, dfd_spec.4, dfd_spec.5,
        );
        parsing_state.set_developer_data_definition(0, FitDataMessage::FieldDescription(fd));
    }

    let header = FitRecordHeader::Normal(FitNormalRecordHeader {
        message_type: FitNormalRecordHeaderMessageType::Data,
        developer_fields_present: false,
        local_mesg_num: 0,
    });

    let mut rec = FitMessageRecord::new(header, &mut parsing_state).unwrap();
    rec.parse(&data, &mut parsing_state, None).unwrap();
    //let (rec, _) = FitMessageRecord::parse(&data, header, &mut parsing_state, None).unwrap();
    assert_eq!(
        rec.position_lat,
        ffav!(
            FitSint32::new(507869600),
            FitFloat64::new(42.56913810968399),
            FitSint32,
            0.0,
            0.0,
            "deg".to_string(),
            "single"
        )
    );
    assert_eq!(
        rec.position_long,
        ffav!(
            FitSint32::new(-847348288),
            FitFloat64::new(-71.02391302585602),
            FitSint32,
            0.0,
            0.0,
            "deg".to_string(),
            "single"
        )
    );
    assert_eq!(
        rec.heart_rate,
        ffbv!(FitUint8::new(151), FitUint8, "bpm", "single")
    );
    assert_eq!(
        rec.power,
        ffbv!(FitUint16::new(209), FitUint16, "watts", "single")
    );

    let fp_field_name = FitString::new("Form Power".to_string());

    for ffdd in &rec.developer_fields {
        match ffdd.field_description.field_name {
            FitFieldBasicValue {
                value: BasicValue::<FitString>::Vec(ref field_names),
                ..
            } => {
                if field_names[0] == fp_field_name {
                    assert_eq!(ffdd.value, FitBaseValue::Uint16(FitUint16::new(57)));
                    return;
                }
            }
            _ => (),
        }
    }
    assert_eq!(1, 2);
}

#[test]
fn fit_message_event_with_subfield() {
    // test subfield that extracts components if a specific subfield is set

    let definition_message = Rc::new(make_definition_message_event());

    let data = [
        0b00101001, 0b10111011, 0b00001001, 0b01000000, // timestamp
        0b00101010, // event
        0b00100111, 0b00000001, 0b00001110, 0b00001000, // data
    ];

    let mut parsing_state = FitParsingState::new();
    parsing_state.add(0, definition_message.clone());

    let header = FitRecordHeader::Normal(FitNormalRecordHeader {
        message_type: FitNormalRecordHeaderMessageType::Data,
        developer_fields_present: false,
        local_mesg_num: 0,
    });

    let mut rec = FitMessageEvent::new(header, &mut parsing_state).unwrap();
    rec.parse(&data, &mut parsing_state, None).unwrap();

    assert_eq!(rec.event, ffbv!(FitFieldEvent::FrontGearChange, FitFieldEvent, "", "single"));
    assert_eq!(rec.rear_gear_num, ffbv!(FitUint8z::new(8), FitUint8z, "", "single"));
    assert_eq!(rec.rear_gear, ffbv!(FitUint8z::new(14), FitUint8z, "", "single"));
    assert_eq!(rec.front_gear_num, ffbv!(FitUint8z::new(1), FitUint8z, "", "single"));
    assert_eq!(rec.front_gear, ffbv!(FitUint8z::new(39), FitUint8z, "", "single"));
}
