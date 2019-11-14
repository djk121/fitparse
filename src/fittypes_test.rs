use super::*;

use std::rc::Rc;

use nom::Endianness::Little;

use FitBaseValue;
use FitDeveloperFieldDefinition;
use FitNormalRecordHeader;
use FitNormalRecordHeaderMessageType;

fn make_definition_message() -> FitDefinitionMessage {
    FitDefinitionMessage {
        header: FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0,
        },
        endianness: Little,
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

fn make_field_definitions(definitions: Vec<(u8, usize, u8)>) -> Vec<FitFieldDefinition> {
    definitions
        .iter()
        .map(
            |(definition_number, field_size, base_type)| FitFieldDefinition {
                definition_number: *definition_number,
                field_size: *field_size,
                base_type: *base_type,
            },
        )
        .collect()
}

macro_rules! ffv {
    ($s:expr) => {
        FitFieldValue {
            value: $s,
            units: "".to_string(),
        }
    };
    ($s:expr, $u:expr) => {
        FitFieldValue {
            value: $s,
            units: $u.to_string(),
        }
    };
}

fn make_field_description(
    field_name: Vec<Option<String>>,
    units: Vec<Option<String>>,
    field_definition_number: u8,
    native_mesg_num: Option<FitFieldMesgNum>,
    fit_base_type_id: FitFieldFitBaseType,
    field_definitions: Vec<(u8, usize, u8)>,
) -> Rc<FitMessageFieldDescription> {
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
            endianness: Little,
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
        message_name: "FitMessageFieldDescription",
        developer_data_index: ffv!(Some(0)),
        field_definition_number: ffv!(Some(field_definition_number)),
        fit_base_type_id: ffv!(Some(fit_base_type_id)),
        field_name: ffv!(Some(field_name)),
        array: ffv!(None),
        components: ffv!(None),
        scale: ffv!(None),
        offset: ffv!(None),
        units: ffv!(Some(units)),
        bits: ffv!(None),
        accumulate: ffv!(None),
        fit_base_unit_id: ffv!(None),
        native_mesg_num: ffv!(native_mesg_num),
        native_field_num: ffv!(None),
    })
}

#[test]
fn fit_message_record() {
    let definition_message = Rc::new(make_definition_message());
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

    let (rec, _) = FitMessageRecord::parse(&data, header, &mut parsing_state, None).unwrap();
    assert_eq!(rec.position_lat, ffv!(Some(42.56913810968399), "deg"));
    assert_eq!(rec.position_long, ffv!(Some(-71.02391302585602), "deg"));
    assert_eq!(rec.heart_rate, ffv!(Some(151), "bpm"));
    assert_eq!(rec.power, ffv!(Some(209), "watts"));
}

#[test]
fn fit_message_record_with_developer_fields() {
    let mut definition_message = make_definition_message();

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
            vec![Some("Form Power".to_string())],
            vec![Some("Watts".to_string())],
            8,
            None,
            FitFieldFitBaseType::Uint16,
            vec![(0, 1, 2), (1, 1, 2), (2, 1, 2), (3, 11, 7), (8, 6, 7)],
        ),
        (
            vec![Some("Leg Spring Stiffness".to_string())],
            vec![Some("KN/m".to_string())],
            9,
            None,
            FitFieldFitBaseType::Float32,
            vec![(0, 1, 2), (1, 1, 2), (2, 1, 2), (3, 21, 7), (8, 5, 7)],
        ),
        (
            vec![Some("Distance".to_string())],
            vec![Some("Meters".to_string())],
            6,
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
            vec![Some("Speed".to_string())],
            vec![Some("M/S".to_string())],
            5,
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

    let (rec, _) = FitMessageRecord::parse(&data, header, &mut parsing_state, None).unwrap();
    assert_eq!(rec.position_lat, ffv!(Some(42.56913810968399), "deg"));
    assert_eq!(rec.position_long, ffv!(Some(-71.02391302585602), "deg"));
    assert_eq!(rec.heart_rate, ffv!(Some(151), "bpm"));
    assert_eq!(rec.power, ffv!(Some(209), "watts"));

    let fp_field_name = Some("Form Power".to_string());

    for ffdd in &rec.developer_fields {
        match ffdd.field_description.field_name {
            FitFieldValue {
                value: Some(ref field_names),
                units: _,
            } => {
                //Some(ref field_names) => {
                if field_names[0] == fp_field_name {
                    assert_eq!(ffdd.value, FitBaseValue::Uint16(Some(57)));
                    return;
                }
            }
            _ => (),
        }
    }
    assert_eq!(1, 2);
}
