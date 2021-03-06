extern crate fitparse;

use fitparse::fitfile::FitFile;
use fitparse::fittypes::FitDataMessage;
use fitparse::{AdjustedValue, BasicValue, FitFloat64, FitUint16, FitUint32, PreAdjustedValue};
use fitparse::{FitFieldAdjustedValue, FitFieldBasicValue, FitMessage, FitParseConfig};

extern crate nom;
use nom::number::Endianness;

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
    ($preadjusted_value:expr, $value:expr, $ty:ty, $scale:expr, $offset:expr, $units:expr, $components:expr, "single") => {
        FitFieldAdjustedValue {
            value: AdjustedValue::Single($value),
            parsed_value: PreAdjustedValue::<$ty>::Single($preadjusted_value),
            units: $units,
            scale: $scale,
            offset: $offset,
            components: $components,
        }
    };
    ($preadjusted_value:expr, $value:expr, $ty:ty, $scale:expr, $offset:expr, $units:expr, $components:expr, "vec") => {
        FitFieldAdjustedValue {
            value: AdjustedValue::Vec($value),
            parsed_value: PreAdjustedValue::<$ty>::Vec($preadjusted_value),
            units: $units,
            scale: $scale,
            offset: $offset,
            components: $components,
        }
    };
}

#[test]
fn activity_test() {
    let data: [u8; 771] = [
        0b00001100, 0b00010000, 0b01100100, 0b00000000, 0b11110101, 0b00000010, 0b00000000,
        0b00000000, 0b00101110, 0b01000110, 0b01001001, 0b01010100, 0b01000000, 0b00000000,
        0b00000001, 0b00000000, 0b00000000, 0b00000101, 0b00000011, 0b00000100, 0b10001100,
        0b00000100, 0b00000100, 0b10000110, 0b00000001, 0b00000010, 0b10000100, 0b00000010,
        0b00000010, 0b10000100, 0b00000000, 0b00000001, 0b00000000, 0b00000000, 0b01111111,
        0b11111111, 0b11111111, 0b11111111, 0b00101001, 0b11100110, 0b00000111, 0b00010010,
        0b00000000, 0b00001111, 0b00000000, 0b00000001, 0b00000100, 0b01000000, 0b00000000,
        0b00000001, 0b00000000, 0b00110001, 0b00000010, 0b00000000, 0b00000010, 0b10000100,
        0b00000001, 0b00000001, 0b00000010, 0b01000000, 0b00000000, 0b00000001, 0b00000000,
        0b00110001, 0b00000001, 0b00000000, 0b00000010, 0b10000100, 0b00000000, 0b00000000,
        0b11110000, 0b01000001, 0b00000000, 0b00000001, 0b00000000, 0b00010101, 0b00000101,
        0b11111101, 0b00000100, 0b10000110, 0b00000011, 0b00000100, 0b10000110, 0b00000000,
        0b00000001, 0b00000000, 0b00000001, 0b00000001, 0b00000000, 0b00000100, 0b00000001,
        0b00000010, 0b01000001, 0b00000000, 0b00000001, 0b00000000, 0b00010101, 0b00000101,
        0b11111101, 0b00000100, 0b10000110, 0b00000011, 0b00000001, 0b00000000, 0b00000000,
        0b00000001, 0b00000000, 0b00000001, 0b00000001, 0b00000000, 0b00000100, 0b00000001,
        0b00000010, 0b00000001, 0b00101001, 0b11100110, 0b00000111, 0b00010010, 0b00000000,
        0b00000000, 0b00000000, 0b00000000, 0b01000010, 0b00000000, 0b00000001, 0b00000000,
        0b00010100, 0b00000110, 0b11111101, 0b00000100, 0b10000110, 0b00000000, 0b00000100,
        0b10000101, 0b00000001, 0b00000100, 0b10000101, 0b00000101, 0b00000100, 0b10000110,
        0b00000010, 0b00000010, 0b10000100, 0b00000110, 0b00000010, 0b10000100, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00010010, 0b00011101, 0b10000101, 0b01100001,
        0b00101110, 0b11001011, 0b11111011, 0b10110100, 0b10010111, 0b00000000, 0b00000000,
        0b00000000, 0b00000010, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00010011, 0b00011101, 0b10000101, 0b01100001,
        0b00101110, 0b11001011, 0b11111011, 0b10110100, 0b10011000, 0b00000000, 0b00000000,
        0b00000000, 0b00000010, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00010100, 0b00011101, 0b10000101, 0b01100001,
        0b00101110, 0b11001011, 0b11111011, 0b10110100, 0b10011000, 0b00000000, 0b00000000,
        0b00000000, 0b00000010, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00010101, 0b00011101, 0b10000101, 0b01100001,
        0b00111001, 0b11001011, 0b11111011, 0b10110100, 0b10000010, 0b00000000, 0b00000000,
        0b00000000, 0b00010101, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00010110, 0b00011101, 0b10000101, 0b01100001,
        0b01000000, 0b11001011, 0b11111011, 0b10110100, 0b01111001, 0b00000000, 0b00000000,
        0b00000000, 0b00011100, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00010111, 0b00011101, 0b10000101, 0b01100001,
        0b01000110, 0b11001011, 0b11111011, 0b10110100, 0b01110010, 0b00000000, 0b00000000,
        0b00000000, 0b00100011, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011000, 0b00011101, 0b10000101, 0b01100001,
        0b01001010, 0b11001011, 0b11111011, 0b10110100, 0b01101100, 0b00000000, 0b00000000,
        0b00000000, 0b00101001, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011001, 0b00011101, 0b10000101, 0b01100001,
        0b01110111, 0b11001011, 0b11111011, 0b10110100, 0b00010100, 0b00000000, 0b00000000,
        0b00000000, 0b01110010, 0b00001111, 0b00110011, 0b00000000, 0b00000000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011010, 0b00011101, 0b10000101, 0b01100001,
        0b10001101, 0b11001011, 0b11111011, 0b10110011, 0b10110100, 0b00000000, 0b00000000,
        0b00000000, 0b10111001, 0b00001111, 0b00110011, 0b00000000, 0b01011100, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011011, 0b00011101, 0b10000101, 0b01100001,
        0b10101110, 0b11001011, 0b11111011, 0b10110011, 0b00111100, 0b00000000, 0b00000000,
        0b00000001, 0b00010011, 0b00001111, 0b00110011, 0b00000000, 0b10011000, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011100, 0b00011101, 0b10000101, 0b01100001,
        0b11001100, 0b11001011, 0b11111011, 0b10110010, 0b11010111, 0b00000000, 0b00000000,
        0b00000001, 0b01011111, 0b00001111, 0b00110011, 0b00000000, 0b11010001, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011101, 0b00011101, 0b10000101, 0b01100001,
        0b10101010, 0b11001011, 0b11111011, 0b10110010, 0b01111001, 0b00000000, 0b00000000,
        0b00000001, 0b10100110, 0b00001111, 0b00110011, 0b00000001, 0b00000110, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011110, 0b00011101, 0b10000101, 0b01100001,
        0b01011111, 0b11001011, 0b11111011, 0b10110010, 0b10001101, 0b00000000, 0b00000000,
        0b00000001, 0b11101101, 0b00001111, 0b00110011, 0b00000001, 0b00110011, 0b00000010,
        0b00101001, 0b11100110, 0b00000111, 0b00011111, 0b00011101, 0b10000101, 0b01100001,
        0b00010010, 0b11001011, 0b11111011, 0b10110010, 0b01010111, 0b00000000, 0b00000000,
        0b00000010, 0b00111101, 0b00001111, 0b00110011, 0b00000001, 0b01110000, 0b00000001,
        0b00101001, 0b11100110, 0b00000111, 0b00011111, 0b00000000, 0b00000000, 0b00000100,
        0b00000000, 0b01000011, 0b00000000, 0b00000001, 0b00000000, 0b00010011, 0b00010100,
        0b11111101, 0b00000100, 0b10000110, 0b00000010, 0b00000100, 0b10000110, 0b00000011,
        0b00000100, 0b10000101, 0b00000100, 0b00000100, 0b10000101, 0b00000101, 0b00000100,
        0b10000101, 0b00000110, 0b00000100, 0b10000101, 0b00000111, 0b00000100, 0b10000110,
        0b00001000, 0b00000100, 0b10000110, 0b00001001, 0b00000100, 0b10000110, 0b11111110,
        0b00000010, 0b10000100, 0b00001011, 0b00000010, 0b10000100, 0b00001100, 0b00000010,
        0b10000100, 0b00001101, 0b00000010, 0b10000100, 0b00001110, 0b00000010, 0b10000100,
        0b00010101, 0b00000010, 0b10000100, 0b00010110, 0b00000010, 0b10000100, 0b00000000,
        0b00000001, 0b00000000, 0b00000001, 0b00000001, 0b00000000, 0b00011000, 0b00000001,
        0b00000000, 0b00011001, 0b00000001, 0b00000000, 0b00000011, 0b00101001, 0b11100110,
        0b00000111, 0b10100011, 0b00101001, 0b11100110, 0b00000111, 0b00010010, 0b00011101,
        0b10000101, 0b01100001, 0b00101110, 0b11001011, 0b11111011, 0b10110100, 0b10010111,
        0b00011101, 0b10000101, 0b01100001, 0b00010010, 0b11001011, 0b11111011, 0b10110010,
        0b01010111, 0b00000000, 0b00000000, 0b00110101, 0b10110101, 0b00000000, 0b00000000,
        0b00110101, 0b10110101, 0b00000000, 0b00000000, 0b00000010, 0b00111101, 0b00000000,
        0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000001, 0b10100001,
        0b00000001, 0b01110000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00001001,
        0b00000001, 0b00000111, 0b00000001, 0b01000001, 0b00000000, 0b00000001, 0b00000000,
        0b00010101, 0b00000101, 0b11111101, 0b00000100, 0b10000110, 0b00000011, 0b00000100,
        0b10000110, 0b00000000, 0b00000001, 0b00000000, 0b00000001, 0b00000001, 0b00000000,
        0b00000100, 0b00000001, 0b00000010, 0b00000001, 0b00101001, 0b11100110, 0b00000111,
        0b10100011, 0b00000000, 0b00000000, 0b00000000, 0b00000001, 0b00001000, 0b00001001,
        0b00000001, 0b01000100, 0b00000000, 0b00000001, 0b00000000, 0b00010010, 0b00010101,
        0b11111101, 0b00000100, 0b10000110, 0b00000010, 0b00000100, 0b10000110, 0b00000011,
        0b00000100, 0b10000101, 0b00000100, 0b00000100, 0b10000101, 0b00000111, 0b00000100,
        0b10000110, 0b00001000, 0b00000100, 0b10000110, 0b00001001, 0b00000100, 0b10000110,
        0b11111110, 0b00000010, 0b10000100, 0b00001011, 0b00000010, 0b10000100, 0b00001101,
        0b00000010, 0b10000100, 0b00001110, 0b00000010, 0b10000100, 0b00001111, 0b00000010,
        0b10000100, 0b00010110, 0b00000010, 0b10000100, 0b00010111, 0b00000010, 0b10000100,
        0b00011001, 0b00000010, 0b10000100, 0b00011010, 0b00000010, 0b10000100, 0b00000000,
        0b00000001, 0b00000000, 0b00000001, 0b00000001, 0b00000000, 0b00000101, 0b00000001,
        0b00000000, 0b00000110, 0b00000001, 0b00000000, 0b00011100, 0b00000001, 0b00000000,
        0b00000100, 0b00101001, 0b11100110, 0b00000111, 0b10100011, 0b00101001, 0b11100110,
        0b00000111, 0b00010010, 0b00011101, 0b10000101, 0b01100001, 0b00101110, 0b11001011,
        0b11111011, 0b10110100, 0b10010111, 0b00000000, 0b00000000, 0b00110101, 0b10110101,
        0b00000000, 0b00000000, 0b00110101, 0b10110101, 0b00000000, 0b00000000, 0b00000010,
        0b00111101, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000,
        0b00000001, 0b10100001, 0b00000001, 0b01110000, 0b00000000, 0b00000000, 0b00000000,
        0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000001, 0b00001001, 0b00000001,
        0b00000001, 0b00000000, 0b00000000, 0b01000101, 0b00000000, 0b00000001, 0b00000000,
        0b00100010, 0b00000111, 0b11111101, 0b00000100, 0b10000110, 0b00000000, 0b00000100,
        0b10000110, 0b00000101, 0b00000100, 0b10000110, 0b00000001, 0b00000010, 0b10000100,
        0b00000010, 0b00000001, 0b00000000, 0b00000011, 0b00000001, 0b00000000, 0b00000100,
        0b00000001, 0b00000000, 0b00000101, 0b00101001, 0b11100110, 0b00000111, 0b10100011,
        0b00000000, 0b00000000, 0b00110101, 0b10110101, 0b00101001, 0b11100101, 0b11001111,
        0b01100011, 0b00000000, 0b00000001, 0b00000000, 0b00011010, 0b00000001, 0b11010101,
        0b10100001,
    ];

    let mut ff = FitFile::new(1024 * 1024 * 10, true);
    let slice: &mut &[u8] = &mut data.as_ref();

    println!("pre parse");

    match ff.parse(slice) {
        Err(e) => panic!("failed to parse file: {:?}", e),
        _ => (),
    }

    println!("post parse");
    assert_eq!(32, ff.messages.len());

    match ff.messages[29] {
        FitMessage::Data(FitDataMessage::Session(ref m)) => {
            assert_eq!(
                m.num_laps,
                ffbv!(FitUint16::new(1), FitUint16, "".to_string(), "single")
            );
            assert_eq!(
                m.avg_speed,
                ffav!(
                    FitUint16::new(417),
                    FitFloat64::new(0.417),
                    FitUint16,
                    1000.0,
                    0.0,
                    "m/s".to_string(),
                    vec![FitParseConfig::new_from_component(
                        124,
                        4,
                        134,
                        Endianness::Big,
                        0,
                        16,
                        Some((1000.0, 0.0)),
                        Some("m/s".to_string())
                    )
                    .unwrap(),],
                    "single"
                )
            );
            assert_eq!(
                m.max_speed,
                ffav!(
                    FitUint16::new(368),
                    FitFloat64::new(0.368),
                    FitUint16,
                    1000.0,
                    0.0,
                    "m/s".to_string(),
                    vec![FitParseConfig::new_from_component(
                        125,
                        4,
                        134,
                        Endianness::Big,
                        0,
                        16,
                        Some((1000.0, 0.0)),
                        Some("m/s".to_string())
                    )
                    .unwrap(),],
                    "single"
                )
            );
            assert_eq!(
                m.enhanced_max_speed,
                ffav!(
                    FitUint32::new(368),
                    FitFloat64::new(0.368),
                    FitUint32,
                    1000.0,
                    0.0,
                    "m/s".to_string(),
                    vec![],
                    "single"
                )
            );
        }
        _ => panic!("message 29 should be Activity with enhanced_max_speed = 0.368"),
    }

    match ff.messages[31] {
        FitMessage::Data(FitDataMessage::Activity(ref m)) => assert_eq!(
            m.total_timer_time,
            ffav!(
                FitUint32::new(13749),
                FitFloat64::new(13.749),
                FitUint32,
                1000.0,
                0.0,
                "s".to_string(),
                vec![],
                "single"
            )
        ),
        _ => panic!("message 31 should be Activity with total_timer_time = 13.749"),
    }
}
