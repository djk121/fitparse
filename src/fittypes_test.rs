use super::*;

use std::rc::Rc;

use FitNormalRecordHeader;
use nom::Endianness::Little;
use FitDeveloperFieldDefinition;
use FitNormalRecordHeaderMessageType;

#[test]
fn my_test() {
    assert_eq!(1, 1);
}

#[test]
fn basic_fit_message_record() {
    let definition_message = Rc::new(FitDefinitionMessage {
        header: FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Definition,
            developer_fields_present: false,
            local_mesg_num: 0
        },
        endianness: Little,
        global_mesg_num: FitFieldMesgNum::Record,
        num_fields: 11,
        message_size: 28,
        field_definitions: vec![
            FitFieldDefinition {
                definition_number: 253,
                field_size: 4,
                base_type: 6
            },
            FitFieldDefinition {
                definition_number: 0,
                field_size: 4,
                base_type: 5
            },
            FitFieldDefinition {
                definition_number: 1,
                field_size: 4,
                base_type: 5
            },
            FitFieldDefinition {
                definition_number: 5,
                field_size: 4,
                base_type: 6
            },
            FitFieldDefinition {
                definition_number: 2,
                field_size: 2,
                base_type: 4
            },
            FitFieldDefinition {
                definition_number: 6,
                field_size: 2,
                base_type: 4
            },
            FitFieldDefinition {
                definition_number: 7,
                field_size: 2,
                base_type: 4
            },
            FitFieldDefinition {
                definition_number: 3,
                field_size: 1,
                base_type: 2
            },
            FitFieldDefinition {
                definition_number: 4,
                field_size: 1,
                base_type: 2
            },
            FitFieldDefinition {
                definition_number: 39,
                field_size: 2,
                base_type: 4
            },
            FitFieldDefinition {
                definition_number: 41,
                field_size: 2,
                base_type: 4
            }
        ],
        num_developer_fields: 0,
        developer_field_definitions: vec![]
    });

    let data = [0b00101011, 0b00111100, 0b10101001, 0b00110011, 0b10100000, 0b01111001, 0b01000101, 0b00011110, 0b11000000, 0b01111101,
                0b01111110, 0b11001101, 0b10011000, 0b00001000, 0b00010011, 0b00000000, 0b01010001, 0b00001000, 0b01010111, 0b00001101,
                0b11010001, 0b00000000, 0b10010111, 0b10110110, 0b10010111, 0b00000010, 0b00010000, 0b00001001];

    let mut parsing_state = FitParsingState::new();
    parsing_state.add(0, definition_message.clone());

    let header = FitRecordHeader::Normal(
        FitNormalRecordHeader {
            message_type: FitNormalRecordHeaderMessageType::Data,
            developer_fields_present: false,
            local_mesg_num: 0
        }
    );

    let (rec, _) = FitMessageRecord::parse(&data, header, &mut parsing_state, None).unwrap();
    assert_eq!(rec.position_lat, Some(507869600));
    assert_eq!(rec.power, Some(209));

}
