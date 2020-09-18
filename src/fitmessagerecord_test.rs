use super::*;

use std::sync::Arc;

use nom::number::Endianness;

use FitBaseValue;
use FitDeveloperFieldDefinition;
use FitNormalRecordHeader;
use FitNormalRecordHeaderMessageType;

use {AdjustedValue, PreAdjustedValue};

use FitFloat64;

// the record in here is from activity 4275096187.fit, record #7831

// FitMessageRecord
//                      timestamp: seconds_since_garmin_epoch: 943374480, rust_time: 2019-11-22 16:28:00 UTC
//                   position_lat: 47.670721700415015 (deg)
//                  position_long: -122.34714366495609 (deg)
//                     heart_rate: 149 (bpm)
//                        cadence: 89 (rpm)
//                       distance: 11277.15 (m)
//                    temperature: 9 (C)
//           vertical_oscillation: 81 (mm)
//            stance_time_percent: 38 (percent)
//                    stance_time: 255 (ms)
//                  activity_type: Running
//             fractional_cadence: 0 (rpm)
//                 enhanced_speed: 3.275 (m/s)
//              enhanced_altitude: 49 (m)
//                 vertical_ratio: 7.31 (percent)
//            stance_time_balance: 48.81 (percent)
//                    step_length: 1072 (mm)
//                     unknown_88: 300
//                     unknown_90: -2
//                     unknown_87: 0
//                          Power: 272 [Watts]
//                        Cadence: 88 [RPM]
//                    Ground Time: 228 [Milliseconds]
//           Vertical Oscillation: 7 [Centimeters]
//                      Air Power: 4 [Watts]
//                     Form Power: 70 [Watts]
//           Leg Spring Stiffness: 11.625 [kN/m]
//                      raw_bytes: [10010000,11000000,00111010,00111000,10100111,00110000,11100110,00100001,11011000,01100001,11111111,10101000,00100011,00110101,00010001,00000000,11001011,00001100,00000000,00000000,10111001,00001010,00000000,00000000,00101010,00000011,11011000,00001110,11110110,00001001,11011011,00000010,00010001,00010011,11100000,00101001,00000000,00000000,00101100,00000001,10010101,01011001,00001001,00000001,00000000,11111110,00010000,00000001,01011000,11100100,00000000,00000000,00000000,11100000,01000000,00000100,00000000,01000110,00000000,00000000,00000000,00111010,01000001]

fn make_definition_message_record() -> FitDefinitionMessage {
    // from the reference record above, but with developer fields
    // omitted from the definition

    FitDefinitionMessage {
        header: FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        },
        endianness: Endianness::Little,
        global_mesg_num: FitGlobalMesgNum::Known(FitFieldMesgNum::Record),
        num_fields: 20,
        message_size: 46,
        field_definitions: make_field_definitions(vec![
            (253, 4, 134),    // timestamp, uint32
            (0, 4, 133),      // position_lat, sint32
            (1, 4, 133),      // position_long, sint32
            (5, 4, 134),      // distance, uint32
            (73, 4, 134),     // enhanced_speed, uint32
            (78, 4, 134),     // enhanced_altitude, uint32
            (39, 2, 131),     // vertical_oscillation, uint16
            (40, 2, 131),     // stance_time_percent, uint16
            (41, 2, 131),     // stance_time, uint16
            (83, 2, 131),     // vertical_ratio, uint16
            (84, 2, 131),     // stance_time_balance, uint16
            (85, 2, 131),     // step_length, uint16
            (87, 2, 131),     // unknown_87, uint16
            (88, 2, 131),     // unknown_88, uint16
            (3, 1, 2),        // heart_rate, uint8
            (4, 1, 2),        // cadence, uint8
            (13, 1, 1),       // temperature, sint8
            (42, 1, 0),       // activity_type, activity_type
            (53, 1, 2),       // fractional_cadence, uint8
            (90, 1, 1),       // unknown_90, sint8
        ]),
        num_developer_fields: 0,
        developer_field_definitions: vec![],
        developer_data_ids: HashMap::new(),
        developer_field_descriptions: HashMap::new()
    }
}

fn make_field_definitions(definitions: Vec<(u8, usize, u8)>) -> Vec<FitFieldDefinition> {
    definitions
        .iter()
        .map(
            |(definition_number, field_size, base_type)| {
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
) -> FitMessageFieldDescription {
    let nmn = match native_mesg_num {
        None => ffbv!("unparsed", FitFieldMesgNum, "single"),
        Some(x) => ffbv!(x, FitFieldMesgNum, "single"),
    };

    FitMessageFieldDescription {
        header: FitRecordHeader::Normal(FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Data,
            developer_fields_present: false,
            local_mesg_num: 0,
        }),
        definition_message: Arc::new(FitDefinitionMessage {
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
            developer_data_ids: HashMap::new(),
            developer_field_descriptions: HashMap::new(),
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
    }
}

#[test]
fn fit_message_record() {
    let definition_message = Arc::new(make_definition_message_record());
    
    let data_without_developer_fields = [
            0b10010000,0b11000000,0b00111010,0b00111000,0b10100111,0b00110000,0b11100110,0b00100001,
            0b11011000,0b01100001,0b11111111,0b10101000,0b00100011,0b00110101,0b00010001,0b00000000,
            0b11001011,0b00001100,0b00000000,0b00000000,0b10111001,0b00001010,0b00000000,0b00000000,
            0b00101010,0b00000011,0b11011000,0b00001110,0b11110110,0b00001001,0b11011011,0b00000010,
            0b00010001,0b00010011,0b11100000,0b00101001,0b00000000,0b00000000,0b00101100,0b00000001,
            0b10010101,0b01011001,0b00001001,0b00000001,0b00000000,0b11111110];

    let mut parsing_state = FitParsingState::new();
    parsing_state.add_definition(0, definition_message.clone());

    let header = FitRecordHeader::Normal(FitNormalRecordHeader {
        message_type: FitNormalRecordHeaderMessageType::Data,
        developer_fields_present: false,
        local_mesg_num: 0,
    });

    let mut rec = FitMessageRecord::new(header, &mut parsing_state).unwrap();
    rec.parse(&data_without_developer_fields, &mut parsing_state, None).unwrap();

    assert_eq!(
        rec.position_lat,
        ffav!(
            FitSint32::new(568733863),
            FitFloat64::new(47.670721700415015),
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
            FitSint32::new(-1459658280),
            FitFloat64::new(-122.34714366495609),
            FitSint32,
            0.0,
            0.0,
            "deg".to_string(),
            "single"
        )
    );
    assert_eq!(
        rec.heart_rate,
        ffbv!(FitUint8::new(149), FitUint8, "bpm", "single")
    );

    assert_eq!(
        rec.unknown_fields.get(&88).unwrap(),
        &FitBaseValue::Sint16(FitSint16::new(300))
    );
}

#[test]
fn fit_message_record_with_developer_fields() {
    let mut definition_message = make_definition_message_record();

    definition_message.message_size = 63;
    definition_message.num_developer_fields = 7;
    definition_message.developer_field_definitions = vec![
        FitDeveloperFieldDefinition {
            definition_number: 0,
            field_size: 2,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 2,
            field_size: 1,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 3,
            field_size: 2,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 4,
            field_size: 4,
            developer_data_index: 0,
        },
        FitDeveloperFieldDefinition {
            definition_number: 11,
            field_size: 2,
            developer_data_index: 0,
        },
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
    ];

    let definition_message_final = Arc::new(definition_message);
    let data_with_developer_fields = [
        0b10010000,0b11000000,0b00111010,0b00111000,0b10100111,0b00110000,0b11100110,0b00100001,
        0b11011000,0b01100001,0b11111111,0b10101000,0b00100011,0b00110101,0b00010001,0b00000000,
        0b11001011,0b00001100,0b00000000,0b00000000,0b10111001,0b00001010,0b00000000,0b00000000,
        0b00101010,0b00000011,0b11011000,0b00001110,0b11110110,0b00001001,0b11011011,0b00000010,
        0b00010001,0b00010011,0b11100000,0b00101001,0b00000000,0b00000000,0b00101100,0b00000001,
        0b10010101,0b01011001,0b00001001,0b00000001,0b00000000,0b11111110,0b00010000,0b00000001,
        0b01011000,0b11100100,0b00000000,0b00000000,0b00000000,0b11100000,0b01000000,0b00000100,
        0b00000000,0b01000110,0b00000000,0b00000000,0b00000000,0b00111010,0b01000001
    ];

    let mut parsing_state = FitParsingState::new();
    parsing_state.add_definition(0, definition_message_final.clone());
    
    let developer_field_descriptions = vec![
        (
            vec![FitString::new("Power".to_string())],
            vec![FitString::new("Watts".to_string())],
            FitUint8::new(0),
            None,
            FitFieldFitBaseType::Uint16,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)
            ],
        ),
        (
            vec![FitString::new("Cadence".to_string())],
            vec![FitString::new("RPM".to_string())],
            FitUint8::new(2),
            None,
            FitFieldFitBaseType::Uint8,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)  
            ],
        ),
        (
            vec![FitString::new("Ground Time".to_string())],
            vec![FitString::new("Milliseconds".to_string())],
            FitUint8::new(3),
            None,
            FitFieldFitBaseType::Uint16,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)  
            ],
        ),
        (
            vec![FitString::new("Vertical Oscillation".to_string())],
            vec![FitString::new("Centimeters".to_string())],
            FitUint8::new(4),
            None,
            FitFieldFitBaseType::Float32,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)  
            ],
        ),
        (
            vec![FitString::new("Air Power".to_string())],
            vec![FitString::new("Watts".to_string())],
            FitUint8::new(11),
            None,
            FitFieldFitBaseType::Uint16,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)  
            ],
        ),
        (
            vec![FitString::new("Form Power".to_string())],
            vec![FitString::new("Watts".to_string())],
            FitUint8::new(8),
            None,
            FitFieldFitBaseType::Uint16,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)  
            ],
        ),
        (
            vec![FitString::new("Leg Spring Stiffness".to_string())],
            vec![FitString::new("kN/m".to_string())],
            FitUint8::new(9),
            None,
            FitFieldFitBaseType::Float32,
            vec![
                (3, 64, 7),
                (8, 16, 7),
                (13, 2, 132),
                (14, 2, 132),
                (0, 1, 2),
                (1, 1, 2),
                (2, 1, 2),
                (6, 1, 2),
                (7, 1, 1),
                (15, 1, 2)  
            ],
        ),
    ];

    for dfd_spec in developer_field_descriptions {
        println!("dfd_spec: {:?}", dfd_spec);
        let field_number = u8::from(dfd_spec.2.clone());
        let fd = make_field_description(
            dfd_spec.0, dfd_spec.1, dfd_spec.2, dfd_spec.3, dfd_spec.4, dfd_spec.5,
        );
        parsing_state.add_developer_field_description(0, field_number, Arc::new(fd));
    }

    let header = FitRecordHeader::Normal(FitNormalRecordHeader {
        message_type: FitNormalRecordHeaderMessageType::Data,
        developer_fields_present: false,
        local_mesg_num: 0,
    });

    let mut rec = FitMessageRecord::new(header, &mut parsing_state).unwrap();
    rec.parse(&data_with_developer_fields, &mut parsing_state, None).unwrap();
    assert_eq!(
        rec.position_lat,
        ffav!(
            FitSint32::new(568733863),
            FitFloat64::new(47.670721700415015),
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
            FitSint32::new(-1459658280),
            FitFloat64::new(-122.34714366495609),
            FitSint32,
            0.0,
            0.0,
            "deg".to_string(),
            "single"
        )
    );

    let fp_field_name = FitString::new("Form Power".to_string());

    for ffdd in &rec.developer_fields {
        match ffdd.field_description.field_name {
            FitFieldBasicValue {
                value: BasicValue::<FitString>::Vec(ref field_names),
                ..
            } => {
                if field_names[0] == fp_field_name {
                    assert_eq!(ffdd.value, FitBaseValue::Uint16(FitUint16::new(70)));
                    return;
                }
            }
            _ => (),
        }
    }
}

