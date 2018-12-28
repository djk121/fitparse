

use std::rc::Rc;

use nom::Endianness;

use chrono::{DateTime, UTC, FixedOffset, TimeZone, Duration};

use FitRecordHeader;
use FitDefinitionMessage;
use FitFieldDefinition;
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
    fn parse(input: &[u8], endianness: Endianness, offset_secs: f64) -> Result<(FitFieldLocalDateTime, &[u8])> {
        let garmin_epoch = UTC.ymd(1989, 12, 31).and_hms(0, 0, 0);
        let (garmin_epoch_offset, o) = parse_uint32(input, endianness)?;
        let local_dt = FixedOffset::east(offset_secs as i32).timestamp(
            (garmin_epoch + Duration::seconds(garmin_epoch_offset.into())).timestamp(),
            0 // nanosecs
        );

        Ok((FitFieldLocalDateTime{
            seconds_since_garmin_epoch: garmin_epoch_offset,
            rust_time: local_dt
        }, o))
    }
}

    







#[derive(Debug)]
pub enum FitFieldPowerPhaseType { // fit base type: enum
    PowerPhaseStartAngle = 0,
    PowerPhaseEndAngle = 1,
    PowerPhaseArcLength = 2,
    PowerPhaseCenter = 3,
}

impl FitFieldPowerPhaseType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldPowerPhaseType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldPowerPhaseType::from(val), o))
    }
}

impl From<u8> for FitFieldPowerPhaseType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldPowerPhaseType::PowerPhaseStartAngle,
            1 => FitFieldPowerPhaseType::PowerPhaseEndAngle,
            2 => FitFieldPowerPhaseType::PowerPhaseArcLength,
            3 => FitFieldPowerPhaseType::PowerPhaseCenter,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldPowerPhaseType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLeftRightBalance { // fit base type: uint8
    Mask = 127,  // % contribution
    Right = 128,  // data corresponds to right if set, otherwise unknown
}

impl FitFieldLeftRightBalance {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLeftRightBalance, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldLeftRightBalance::from(val), o))
    }
}

impl From<u8> for FitFieldLeftRightBalance {
    fn from(code: u8) -> Self {
        match code {
            127 => FitFieldLeftRightBalance::Mask,
            128 => FitFieldLeftRightBalance::Right,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLeftRightBalance", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWorkoutCapabilities { // fit base type: uint32z
    Interval = 1,
    Custom = 2,
    FitnessEquipment = 4,
    Firstbeat = 8,
    NewLeaf = 16,
    Tcx = 32,  // For backwards compatibility.  Watch should add missing id fields then clear flag.
    Speed = 128,  // Speed source required for workout step.
    HeartRate = 256,  // Heart rate source required for workout step.
    Distance = 512,  // Distance source required for workout step.
    Cadence = 1024,  // Cadence source required for workout step.
    Power = 2048,  // Power source required for workout step.
    Grade = 4096,  // Grade source required for workout step.
    Resistance = 8192,  // Resistance source required for workout step.
    Protected = 16384,
}

impl FitFieldWorkoutCapabilities {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldWorkoutCapabilities, &[u8])> {
        let (val, o) = parse_uint32z(input, endianness)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldWorkoutCapabilities::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u32> for FitFieldWorkoutCapabilities {
    fn from(code: u32) -> Self {
        match code {
            1 => FitFieldWorkoutCapabilities::Interval,
            2 => FitFieldWorkoutCapabilities::Custom,
            4 => FitFieldWorkoutCapabilities::FitnessEquipment,
            8 => FitFieldWorkoutCapabilities::Firstbeat,
            16 => FitFieldWorkoutCapabilities::NewLeaf,
            32 => FitFieldWorkoutCapabilities::Tcx,
            128 => FitFieldWorkoutCapabilities::Speed,
            256 => FitFieldWorkoutCapabilities::HeartRate,
            512 => FitFieldWorkoutCapabilities::Distance,
            1024 => FitFieldWorkoutCapabilities::Cadence,
            2048 => FitFieldWorkoutCapabilities::Power,
            4096 => FitFieldWorkoutCapabilities::Grade,
            8192 => FitFieldWorkoutCapabilities::Resistance,
            16384 => FitFieldWorkoutCapabilities::Protected,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWorkoutCapabilities", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSensorType { // fit base type: enum
    Accelerometer = 0,
    Gyroscope = 1,
    Compass = 2,  // Magnetometer
}

impl FitFieldSensorType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSensorType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSensorType::from(val), o))
    }
}

impl From<u8> for FitFieldSensorType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSensorType::Accelerometer,
            1 => FitFieldSensorType::Gyroscope,
            2 => FitFieldSensorType::Compass,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSensorType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldGender { // fit base type: enum
    Female = 0,
    Male = 1,
}

impl FitFieldGender {
    pub fn parse(input: &[u8]) -> Result<(FitFieldGender, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldGender::from(val), o))
    }
}

impl From<u8> for FitFieldGender {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldGender::Female,
            1 => FitFieldGender::Male,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldGender", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSessionTrigger { // fit base type: enum
    ActivityEnd = 0,
    Manual = 1,  // User changed sport.
    AutoMultiSport = 2,  // Auto multi-sport feature is enabled and user pressed lap button to advance session.
    FitnessEquipment = 3,  // Auto sport change caused by user linking to fitness equipment.
}

impl FitFieldSessionTrigger {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSessionTrigger, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSessionTrigger::from(val), o))
    }
}

impl From<u8> for FitFieldSessionTrigger {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSessionTrigger::ActivityEnd,
            1 => FitFieldSessionTrigger::Manual,
            2 => FitFieldSessionTrigger::AutoMultiSport,
            3 => FitFieldSessionTrigger::FitnessEquipment,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSessionTrigger", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldFileFlags { // fit base type: uint8z
    Read = 2,
    Write = 4,
    Erase = 8,
}

impl FitFieldFileFlags {
    pub fn parse(input: &[u8]) -> Result<(FitFieldFileFlags, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldFileFlags::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldFileFlags {
    fn from(code: u8) -> Self {
        match code {
            2 => FitFieldFileFlags::Read,
            4 => FitFieldFileFlags::Write,
            8 => FitFieldFileFlags::Erase,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldFileFlags", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldMesgCount { // fit base type: enum
    NumPerFile = 0,
    MaxPerFile = 1,
    MaxPerFileType = 2,
}

impl FitFieldMesgCount {
    pub fn parse(input: &[u8]) -> Result<(FitFieldMesgCount, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldMesgCount::from(val), o))
    }
}

impl From<u8> for FitFieldMesgCount {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldMesgCount::NumPerFile,
            1 => FitFieldMesgCount::MaxPerFile,
            2 => FitFieldMesgCount::MaxPerFileType,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldMesgCount", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldFitBaseType { // fit base type: uint8
    Enum = 0,
    Sint8 = 1,
    Uint8 = 2,
    Sint16 = 131,
    Uint16 = 132,
    Sint32 = 133,
    Uint32 = 134,
    String = 7,
    Float32 = 136,
    Float64 = 137,
    Uint8z = 10,
    Uint16z = 139,
    Uint32z = 140,
    Byte = 13,
    Sint64 = 142,
    Uint64 = 143,
    Uint64z = 144,
}

impl FitFieldFitBaseType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldFitBaseType, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldFitBaseType::from(val), o))
    }
}

impl From<u8> for FitFieldFitBaseType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldFitBaseType::Enum,
            1 => FitFieldFitBaseType::Sint8,
            2 => FitFieldFitBaseType::Uint8,
            131 => FitFieldFitBaseType::Sint16,
            132 => FitFieldFitBaseType::Uint16,
            133 => FitFieldFitBaseType::Sint32,
            134 => FitFieldFitBaseType::Uint32,
            7 => FitFieldFitBaseType::String,
            136 => FitFieldFitBaseType::Float32,
            137 => FitFieldFitBaseType::Float64,
            10 => FitFieldFitBaseType::Uint8z,
            139 => FitFieldFitBaseType::Uint16z,
            140 => FitFieldFitBaseType::Uint32z,
            13 => FitFieldFitBaseType::Byte,
            142 => FitFieldFitBaseType::Sint64,
            143 => FitFieldFitBaseType::Uint64,
            144 => FitFieldFitBaseType::Uint64z,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldFitBaseType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldTimerTrigger { // fit base type: enum
    Manual = 0,
    Auto = 1,
    FitnessEquipment = 2,
}

impl FitFieldTimerTrigger {
    pub fn parse(input: &[u8]) -> Result<(FitFieldTimerTrigger, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldTimerTrigger::from(val), o))
    }
}

impl From<u8> for FitFieldTimerTrigger {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldTimerTrigger::Manual,
            1 => FitFieldTimerTrigger::Auto,
            2 => FitFieldTimerTrigger::FitnessEquipment,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldTimerTrigger", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportEvent { // fit base type: enum
    Uncategorized = 0,
    Geocaching = 1,
    Fitness = 2,
    Recreation = 3,
    Race = 4,
    SpecialEvent = 5,
    Training = 6,
    Transportation = 7,
    Touring = 8,
}

impl FitFieldSportEvent {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportEvent, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSportEvent::from(val), o))
    }
}

impl From<u8> for FitFieldSportEvent {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSportEvent::Uncategorized,
            1 => FitFieldSportEvent::Geocaching,
            2 => FitFieldSportEvent::Fitness,
            3 => FitFieldSportEvent::Recreation,
            4 => FitFieldSportEvent::Race,
            5 => FitFieldSportEvent::SpecialEvent,
            6 => FitFieldSportEvent::Training,
            7 => FitFieldSportEvent::Transportation,
            8 => FitFieldSportEvent::Touring,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportEvent", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAutoscroll { // fit base type: enum
    None = 0,
    Slow = 1,
    Medium = 2,
    Fast = 3,
}

impl FitFieldAutoscroll {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAutoscroll, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldAutoscroll::from(val), o))
    }
}

impl From<u8> for FitFieldAutoscroll {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldAutoscroll::None,
            1 => FitFieldAutoscroll::Slow,
            2 => FitFieldAutoscroll::Medium,
            3 => FitFieldAutoscroll::Fast,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAutoscroll", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldStrokeType { // fit base type: enum
    NoEvent = 0,
    Other = 1,  // stroke was detected but cannot be identified
    Serve = 2,
    Forehand = 3,
    Backhand = 4,
    Smash = 5,
}

impl FitFieldStrokeType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldStrokeType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldStrokeType::from(val), o))
    }
}

impl From<u8> for FitFieldStrokeType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldStrokeType::NoEvent,
            1 => FitFieldStrokeType::Other,
            2 => FitFieldStrokeType::Serve,
            3 => FitFieldStrokeType::Forehand,
            4 => FitFieldStrokeType::Backhand,
            5 => FitFieldStrokeType::Smash,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldStrokeType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWeatherSevereType { // fit base type: enum
    Unspecified = 0,
    Tornado = 1,
    Tsunami = 2,
    Hurricane = 3,
    ExtremeWind = 4,
    Typhoon = 5,
    InlandHurricane = 6,
    HurricaneForceWind = 7,
    Waterspout = 8,
    SevereThunderstorm = 9,
    WreckhouseWinds = 10,
    LesSuetesWind = 11,
    Avalanche = 12,
    FlashFlood = 13,
    TropicalStorm = 14,
    InlandTropicalStorm = 15,
    Blizzard = 16,
    IceStorm = 17,
    FreezingRain = 18,
    DebrisFlow = 19,
    FlashFreeze = 20,
    DustStorm = 21,
    HighWind = 22,
    WinterStorm = 23,
    HeavyFreezingSpray = 24,
    ExtremeCold = 25,
    WindChill = 26,
    ColdWave = 27,
    HeavySnowAlert = 28,
    LakeEffectBlowingSnow = 29,
    SnowSquall = 30,
    LakeEffectSnow = 31,
    WinterWeather = 32,
    Sleet = 33,
    Snowfall = 34,
    SnowAndBlowingSnow = 35,
    BlowingSnow = 36,
    SnowAlert = 37,
    ArcticOutflow = 38,
    FreezingDrizzle = 39,
    Storm = 40,
    StormSurge = 41,
    Rainfall = 42,
    ArealFlood = 43,
    CoastalFlood = 44,
    LakeshoreFlood = 45,
    ExcessiveHeat = 46,
    Heat = 47,
    Weather = 48,
    HighHeatAndHumidity = 49,
    HumidexAndHealth = 50,
    Humidex = 51,
    Gale = 52,
    FreezingSpray = 53,
    SpecialMarine = 54,
    Squall = 55,
    StrongWind = 56,
    LakeWind = 57,
    MarineWeather = 58,
    Wind = 59,
    SmallCraftHazardousSeas = 60,
    HazardousSeas = 61,
    SmallCraft = 62,
    SmallCraftWinds = 63,
    SmallCraftRoughBar = 64,
    HighWaterLevel = 65,
    Ashfall = 66,
    FreezingFog = 67,
    DenseFog = 68,
    DenseSmoke = 69,
    BlowingDust = 70,
    HardFreeze = 71,
    Freeze = 72,
    Frost = 73,
    FireWeather = 74,
    Flood = 75,
    RipTide = 76,
    HighSurf = 77,
    Smog = 78,
    AirQuality = 79,
    BriskWind = 80,
    AirStagnation = 81,
    LowWater = 82,
    Hydrological = 83,
    SpecialWeather = 84,
}

impl FitFieldWeatherSevereType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWeatherSevereType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWeatherSevereType::from(val), o))
    }
}

impl From<u8> for FitFieldWeatherSevereType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWeatherSevereType::Unspecified,
            1 => FitFieldWeatherSevereType::Tornado,
            2 => FitFieldWeatherSevereType::Tsunami,
            3 => FitFieldWeatherSevereType::Hurricane,
            4 => FitFieldWeatherSevereType::ExtremeWind,
            5 => FitFieldWeatherSevereType::Typhoon,
            6 => FitFieldWeatherSevereType::InlandHurricane,
            7 => FitFieldWeatherSevereType::HurricaneForceWind,
            8 => FitFieldWeatherSevereType::Waterspout,
            9 => FitFieldWeatherSevereType::SevereThunderstorm,
            10 => FitFieldWeatherSevereType::WreckhouseWinds,
            11 => FitFieldWeatherSevereType::LesSuetesWind,
            12 => FitFieldWeatherSevereType::Avalanche,
            13 => FitFieldWeatherSevereType::FlashFlood,
            14 => FitFieldWeatherSevereType::TropicalStorm,
            15 => FitFieldWeatherSevereType::InlandTropicalStorm,
            16 => FitFieldWeatherSevereType::Blizzard,
            17 => FitFieldWeatherSevereType::IceStorm,
            18 => FitFieldWeatherSevereType::FreezingRain,
            19 => FitFieldWeatherSevereType::DebrisFlow,
            20 => FitFieldWeatherSevereType::FlashFreeze,
            21 => FitFieldWeatherSevereType::DustStorm,
            22 => FitFieldWeatherSevereType::HighWind,
            23 => FitFieldWeatherSevereType::WinterStorm,
            24 => FitFieldWeatherSevereType::HeavyFreezingSpray,
            25 => FitFieldWeatherSevereType::ExtremeCold,
            26 => FitFieldWeatherSevereType::WindChill,
            27 => FitFieldWeatherSevereType::ColdWave,
            28 => FitFieldWeatherSevereType::HeavySnowAlert,
            29 => FitFieldWeatherSevereType::LakeEffectBlowingSnow,
            30 => FitFieldWeatherSevereType::SnowSquall,
            31 => FitFieldWeatherSevereType::LakeEffectSnow,
            32 => FitFieldWeatherSevereType::WinterWeather,
            33 => FitFieldWeatherSevereType::Sleet,
            34 => FitFieldWeatherSevereType::Snowfall,
            35 => FitFieldWeatherSevereType::SnowAndBlowingSnow,
            36 => FitFieldWeatherSevereType::BlowingSnow,
            37 => FitFieldWeatherSevereType::SnowAlert,
            38 => FitFieldWeatherSevereType::ArcticOutflow,
            39 => FitFieldWeatherSevereType::FreezingDrizzle,
            40 => FitFieldWeatherSevereType::Storm,
            41 => FitFieldWeatherSevereType::StormSurge,
            42 => FitFieldWeatherSevereType::Rainfall,
            43 => FitFieldWeatherSevereType::ArealFlood,
            44 => FitFieldWeatherSevereType::CoastalFlood,
            45 => FitFieldWeatherSevereType::LakeshoreFlood,
            46 => FitFieldWeatherSevereType::ExcessiveHeat,
            47 => FitFieldWeatherSevereType::Heat,
            48 => FitFieldWeatherSevereType::Weather,
            49 => FitFieldWeatherSevereType::HighHeatAndHumidity,
            50 => FitFieldWeatherSevereType::HumidexAndHealth,
            51 => FitFieldWeatherSevereType::Humidex,
            52 => FitFieldWeatherSevereType::Gale,
            53 => FitFieldWeatherSevereType::FreezingSpray,
            54 => FitFieldWeatherSevereType::SpecialMarine,
            55 => FitFieldWeatherSevereType::Squall,
            56 => FitFieldWeatherSevereType::StrongWind,
            57 => FitFieldWeatherSevereType::LakeWind,
            58 => FitFieldWeatherSevereType::MarineWeather,
            59 => FitFieldWeatherSevereType::Wind,
            60 => FitFieldWeatherSevereType::SmallCraftHazardousSeas,
            61 => FitFieldWeatherSevereType::HazardousSeas,
            62 => FitFieldWeatherSevereType::SmallCraft,
            63 => FitFieldWeatherSevereType::SmallCraftWinds,
            64 => FitFieldWeatherSevereType::SmallCraftRoughBar,
            65 => FitFieldWeatherSevereType::HighWaterLevel,
            66 => FitFieldWeatherSevereType::Ashfall,
            67 => FitFieldWeatherSevereType::FreezingFog,
            68 => FitFieldWeatherSevereType::DenseFog,
            69 => FitFieldWeatherSevereType::DenseSmoke,
            70 => FitFieldWeatherSevereType::BlowingDust,
            71 => FitFieldWeatherSevereType::HardFreeze,
            72 => FitFieldWeatherSevereType::Freeze,
            73 => FitFieldWeatherSevereType::Frost,
            74 => FitFieldWeatherSevereType::FireWeather,
            75 => FitFieldWeatherSevereType::Flood,
            76 => FitFieldWeatherSevereType::RipTide,
            77 => FitFieldWeatherSevereType::HighSurf,
            78 => FitFieldWeatherSevereType::Smog,
            79 => FitFieldWeatherSevereType::AirQuality,
            80 => FitFieldWeatherSevereType::BriskWind,
            81 => FitFieldWeatherSevereType::AirStagnation,
            82 => FitFieldWeatherSevereType::LowWater,
            83 => FitFieldWeatherSevereType::Hydrological,
            84 => FitFieldWeatherSevereType::SpecialWeather,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWeatherSevereType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldHrZoneCalc { // fit base type: enum
    Custom = 0,
    PercentMaxHr = 1,
    PercentHrr = 2,
}

impl FitFieldHrZoneCalc {
    pub fn parse(input: &[u8]) -> Result<(FitFieldHrZoneCalc, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldHrZoneCalc::from(val), o))
    }
}

impl From<u8> for FitFieldHrZoneCalc {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldHrZoneCalc::Custom,
            1 => FitFieldHrZoneCalc::PercentMaxHr,
            2 => FitFieldHrZoneCalc::PercentHrr,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldHrZoneCalc", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldHrType { // fit base type: enum
    Normal = 0,
    Irregular = 1,
}

impl FitFieldHrType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldHrType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldHrType::from(val), o))
    }
}

impl From<u8> for FitFieldHrType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldHrType::Normal,
            1 => FitFieldHrType::Irregular,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldHrType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSegmentLeaderboardType { // fit base type: enum
    Overall = 0,
    PersonalBest = 1,
    Connections = 2,
    Group = 3,
    Challenger = 4,
    Kom = 5,
    Qom = 6,
    Pr = 7,
    Goal = 8,
    Rival = 9,
    ClubLeader = 10,
}

impl FitFieldSegmentLeaderboardType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSegmentLeaderboardType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSegmentLeaderboardType::from(val), o))
    }
}

impl From<u8> for FitFieldSegmentLeaderboardType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSegmentLeaderboardType::Overall,
            1 => FitFieldSegmentLeaderboardType::PersonalBest,
            2 => FitFieldSegmentLeaderboardType::Connections,
            3 => FitFieldSegmentLeaderboardType::Group,
            4 => FitFieldSegmentLeaderboardType::Challenger,
            5 => FitFieldSegmentLeaderboardType::Kom,
            6 => FitFieldSegmentLeaderboardType::Qom,
            7 => FitFieldSegmentLeaderboardType::Pr,
            8 => FitFieldSegmentLeaderboardType::Goal,
            9 => FitFieldSegmentLeaderboardType::Rival,
            10 => FitFieldSegmentLeaderboardType::ClubLeader,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSegmentLeaderboardType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAttitudeStage { // fit base type: enum
    Failed = 0,
    Aligning = 1,
    Degraded = 2,
    Valid = 3,
}

impl FitFieldAttitudeStage {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAttitudeStage, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldAttitudeStage::from(val), o))
    }
}

impl From<u8> for FitFieldAttitudeStage {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldAttitudeStage::Failed,
            1 => FitFieldAttitudeStage::Aligning,
            2 => FitFieldAttitudeStage::Degraded,
            3 => FitFieldAttitudeStage::Valid,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAttitudeStage", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDigitalWatchfaceLayout { // fit base type: enum
    Traditional = 0,
    Modern = 1,
    Bold = 2,
}

impl FitFieldDigitalWatchfaceLayout {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDigitalWatchfaceLayout, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDigitalWatchfaceLayout::from(val), o))
    }
}

impl From<u8> for FitFieldDigitalWatchfaceLayout {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDigitalWatchfaceLayout::Traditional,
            1 => FitFieldDigitalWatchfaceLayout::Modern,
            2 => FitFieldDigitalWatchfaceLayout::Bold,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDigitalWatchfaceLayout", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldPwrZoneCalc { // fit base type: enum
    Custom = 0,
    PercentFtp = 1,
}

impl FitFieldPwrZoneCalc {
    pub fn parse(input: &[u8]) -> Result<(FitFieldPwrZoneCalc, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldPwrZoneCalc::from(val), o))
    }
}

impl From<u8> for FitFieldPwrZoneCalc {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldPwrZoneCalc::Custom,
            1 => FitFieldPwrZoneCalc::PercentFtp,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldPwrZoneCalc", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWktStepTarget { // fit base type: enum
    Speed = 0,
    HeartRate = 1,
    Open = 2,
    Cadence = 3,
    Power = 4,
    Grade = 5,
    Resistance = 6,
}

impl FitFieldWktStepTarget {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWktStepTarget, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWktStepTarget::from(val), o))
    }
}

impl From<u8> for FitFieldWktStepTarget {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWktStepTarget::Speed,
            1 => FitFieldWktStepTarget::HeartRate,
            2 => FitFieldWktStepTarget::Open,
            3 => FitFieldWktStepTarget::Cadence,
            4 => FitFieldWktStepTarget::Power,
            5 => FitFieldWktStepTarget::Grade,
            6 => FitFieldWktStepTarget::Resistance,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWktStepTarget", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldActivity { // fit base type: enum
    Manual = 0,
    AutoMultiSport = 1,
}

impl FitFieldActivity {
    pub fn parse(input: &[u8]) -> Result<(FitFieldActivity, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldActivity::from(val), o))
    }
}

impl From<u8> for FitFieldActivity {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldActivity::Manual,
            1 => FitFieldActivity::AutoMultiSport,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldActivity", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWatchfaceMode { // fit base type: enum
    Digital = 0,
    Analog = 1,
    ConnectIq = 2,
}

impl FitFieldWatchfaceMode {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWatchfaceMode, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWatchfaceMode::from(val), o))
    }
}

impl From<u8> for FitFieldWatchfaceMode {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWatchfaceMode::Digital,
            1 => FitFieldWatchfaceMode::Analog,
            2 => FitFieldWatchfaceMode::ConnectIq,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWatchfaceMode", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldCoursePoint { // fit base type: enum
    Generic = 0,
    Summit = 1,
    Valley = 2,
    Water = 3,
    Food = 4,
    Danger = 5,
    Left = 6,
    Right = 7,
    Straight = 8,
    FirstAid = 9,
    FourthCategory = 10,
    ThirdCategory = 11,
    SecondCategory = 12,
    FirstCategory = 13,
    HorsCategory = 14,
    Sprint = 15,
    LeftFork = 16,
    RightFork = 17,
    MiddleFork = 18,
    SlightLeft = 19,
    SharpLeft = 20,
    SlightRight = 21,
    SharpRight = 22,
    UTurn = 23,
    SegmentStart = 24,
    SegmentEnd = 25,
}

impl FitFieldCoursePoint {
    pub fn parse(input: &[u8]) -> Result<(FitFieldCoursePoint, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldCoursePoint::from(val), o))
    }
}

impl From<u8> for FitFieldCoursePoint {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldCoursePoint::Generic,
            1 => FitFieldCoursePoint::Summit,
            2 => FitFieldCoursePoint::Valley,
            3 => FitFieldCoursePoint::Water,
            4 => FitFieldCoursePoint::Food,
            5 => FitFieldCoursePoint::Danger,
            6 => FitFieldCoursePoint::Left,
            7 => FitFieldCoursePoint::Right,
            8 => FitFieldCoursePoint::Straight,
            9 => FitFieldCoursePoint::FirstAid,
            10 => FitFieldCoursePoint::FourthCategory,
            11 => FitFieldCoursePoint::ThirdCategory,
            12 => FitFieldCoursePoint::SecondCategory,
            13 => FitFieldCoursePoint::FirstCategory,
            14 => FitFieldCoursePoint::HorsCategory,
            15 => FitFieldCoursePoint::Sprint,
            16 => FitFieldCoursePoint::LeftFork,
            17 => FitFieldCoursePoint::RightFork,
            18 => FitFieldCoursePoint::MiddleFork,
            19 => FitFieldCoursePoint::SlightLeft,
            20 => FitFieldCoursePoint::SharpLeft,
            21 => FitFieldCoursePoint::SlightRight,
            22 => FitFieldCoursePoint::SharpRight,
            23 => FitFieldCoursePoint::UTurn,
            24 => FitFieldCoursePoint::SegmentStart,
            25 => FitFieldCoursePoint::SegmentEnd,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldCoursePoint", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSide { // fit base type: enum
    Right = 0,
    Left = 1,
}

impl FitFieldSide {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSide, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSide::from(val), o))
    }
}

impl From<u8> for FitFieldSide {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSide::Right,
            1 => FitFieldSide::Left,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSide", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAnalogWatchfaceLayout { // fit base type: enum
    Minimal = 0,
    Traditional = 1,
    Modern = 2,
}

impl FitFieldAnalogWatchfaceLayout {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAnalogWatchfaceLayout, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldAnalogWatchfaceLayout::from(val), o))
    }
}

impl From<u8> for FitFieldAnalogWatchfaceLayout {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldAnalogWatchfaceLayout::Minimal,
            1 => FitFieldAnalogWatchfaceLayout::Traditional,
            2 => FitFieldAnalogWatchfaceLayout::Modern,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAnalogWatchfaceLayout", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWeight { // fit base type: uint16
    Calculating = 65534,
}

impl FitFieldWeight {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldWeight, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldWeight::from(val), o))
    }
}

impl From<u16> for FitFieldWeight {
    fn from(code: u16) -> Self {
        match code {
            65534 => FitFieldWeight::Calculating,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWeight", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldIntensity { // fit base type: enum
    Active = 0,
    Rest = 1,
    Warmup = 2,
    Cooldown = 3,
}

impl FitFieldIntensity {
    pub fn parse(input: &[u8]) -> Result<(FitFieldIntensity, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldIntensity::from(val), o))
    }
}

impl From<u8> for FitFieldIntensity {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldIntensity::Active,
            1 => FitFieldIntensity::Rest,
            2 => FitFieldIntensity::Warmup,
            3 => FitFieldIntensity::Cooldown,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldIntensity", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldConnectivityCapabilities { // fit base type: uint32z
    Bluetooth = 1,
    BluetoothLe = 2,
    Ant = 4,
    ActivityUpload = 8,
    CourseDownload = 16,
    WorkoutDownload = 32,
    LiveTrack = 64,
    WeatherConditions = 128,
    WeatherAlerts = 256,
    GpsEphemerisDownload = 512,
    ExplicitArchive = 1024,
    SetupIncomplete = 2048,
    ContinueSyncAfterSoftwareUpdate = 4096,
    ConnectIqAppDownload = 8192,
    GolfCourseDownload = 16384,
    DeviceInitiatesSync = 32768,  // Indicates device is in control of initiating all syncs
    ConnectIqWatchAppDownload = 65536,
    ConnectIqWidgetDownload = 131072,
    ConnectIqWatchFaceDownload = 262144,
    ConnectIqDataFieldDownload = 524288,
    ConnectIqAppManagment = 1048576,  // Device supports delete and reorder of apps via GCM
    SwingSensor = 2097152,
    SwingSensorRemote = 4194304,
    IncidentDetection = 8388608,  // Device supports incident detection
    AudioPrompts = 16777216,
    WifiVerification = 33554432,  // Device supports reporting wifi verification via GCM
    TrueUp = 67108864,  // Device supports True Up
    FindMyWatch = 134217728,  // Device supports Find My Watch
    RemoteManualSync = 268435456,
    LiveTrackAutoStart = 536870912,  // Device supports LiveTrack auto start
    LiveTrackMessaging = 1073741824,  // Device supports LiveTrack Messaging
}

impl FitFieldConnectivityCapabilities {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldConnectivityCapabilities, &[u8])> {
        let (val, o) = parse_uint32z(input, endianness)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldConnectivityCapabilities::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u32> for FitFieldConnectivityCapabilities {
    fn from(code: u32) -> Self {
        match code {
            1 => FitFieldConnectivityCapabilities::Bluetooth,
            2 => FitFieldConnectivityCapabilities::BluetoothLe,
            4 => FitFieldConnectivityCapabilities::Ant,
            8 => FitFieldConnectivityCapabilities::ActivityUpload,
            16 => FitFieldConnectivityCapabilities::CourseDownload,
            32 => FitFieldConnectivityCapabilities::WorkoutDownload,
            64 => FitFieldConnectivityCapabilities::LiveTrack,
            128 => FitFieldConnectivityCapabilities::WeatherConditions,
            256 => FitFieldConnectivityCapabilities::WeatherAlerts,
            512 => FitFieldConnectivityCapabilities::GpsEphemerisDownload,
            1024 => FitFieldConnectivityCapabilities::ExplicitArchive,
            2048 => FitFieldConnectivityCapabilities::SetupIncomplete,
            4096 => FitFieldConnectivityCapabilities::ContinueSyncAfterSoftwareUpdate,
            8192 => FitFieldConnectivityCapabilities::ConnectIqAppDownload,
            16384 => FitFieldConnectivityCapabilities::GolfCourseDownload,
            32768 => FitFieldConnectivityCapabilities::DeviceInitiatesSync,
            65536 => FitFieldConnectivityCapabilities::ConnectIqWatchAppDownload,
            131072 => FitFieldConnectivityCapabilities::ConnectIqWidgetDownload,
            262144 => FitFieldConnectivityCapabilities::ConnectIqWatchFaceDownload,
            524288 => FitFieldConnectivityCapabilities::ConnectIqDataFieldDownload,
            1048576 => FitFieldConnectivityCapabilities::ConnectIqAppManagment,
            2097152 => FitFieldConnectivityCapabilities::SwingSensor,
            4194304 => FitFieldConnectivityCapabilities::SwingSensorRemote,
            8388608 => FitFieldConnectivityCapabilities::IncidentDetection,
            16777216 => FitFieldConnectivityCapabilities::AudioPrompts,
            33554432 => FitFieldConnectivityCapabilities::WifiVerification,
            67108864 => FitFieldConnectivityCapabilities::TrueUp,
            134217728 => FitFieldConnectivityCapabilities::FindMyWatch,
            268435456 => FitFieldConnectivityCapabilities::RemoteManualSync,
            536870912 => FitFieldConnectivityCapabilities::LiveTrackAutoStart,
            1073741824 => FitFieldConnectivityCapabilities::LiveTrackMessaging,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldConnectivityCapabilities", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSport { // fit base type: enum
    Generic = 0,
    Running = 1,
    Cycling = 2,
    Transition = 3,  // Mulitsport transition
    FitnessEquipment = 4,
    Swimming = 5,
    Basketball = 6,
    Soccer = 7,
    Tennis = 8,
    AmericanFootball = 9,
    Training = 10,
    Walking = 11,
    CrossCountrySkiing = 12,
    AlpineSkiing = 13,
    Snowboarding = 14,
    Rowing = 15,
    Mountaineering = 16,
    Hiking = 17,
    Multisport = 18,
    Paddling = 19,
    Flying = 20,
    EBiking = 21,
    Motorcycling = 22,
    Boating = 23,
    Driving = 24,
    Golf = 25,
    HangGliding = 26,
    HorsebackRiding = 27,
    Hunting = 28,
    Fishing = 29,
    InlineSkating = 30,
    RockClimbing = 31,
    Sailing = 32,
    IceSkating = 33,
    SkyDiving = 34,
    Snowshoeing = 35,
    Snowmobiling = 36,
    StandUpPaddleboarding = 37,
    Surfing = 38,
    Wakeboarding = 39,
    WaterSkiing = 40,
    Kayaking = 41,
    Rafting = 42,
    Windsurfing = 43,
    Kitesurfing = 44,
    Tactical = 45,
    Jumpmaster = 46,
    Boxing = 47,
    FloorClimbing = 48,
    All = 254,  // All is for goals only to include all sports.
}

impl FitFieldSport {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSport, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSport::from(val), o))
    }
}

impl From<u8> for FitFieldSport {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSport::Generic,
            1 => FitFieldSport::Running,
            2 => FitFieldSport::Cycling,
            3 => FitFieldSport::Transition,
            4 => FitFieldSport::FitnessEquipment,
            5 => FitFieldSport::Swimming,
            6 => FitFieldSport::Basketball,
            7 => FitFieldSport::Soccer,
            8 => FitFieldSport::Tennis,
            9 => FitFieldSport::AmericanFootball,
            10 => FitFieldSport::Training,
            11 => FitFieldSport::Walking,
            12 => FitFieldSport::CrossCountrySkiing,
            13 => FitFieldSport::AlpineSkiing,
            14 => FitFieldSport::Snowboarding,
            15 => FitFieldSport::Rowing,
            16 => FitFieldSport::Mountaineering,
            17 => FitFieldSport::Hiking,
            18 => FitFieldSport::Multisport,
            19 => FitFieldSport::Paddling,
            20 => FitFieldSport::Flying,
            21 => FitFieldSport::EBiking,
            22 => FitFieldSport::Motorcycling,
            23 => FitFieldSport::Boating,
            24 => FitFieldSport::Driving,
            25 => FitFieldSport::Golf,
            26 => FitFieldSport::HangGliding,
            27 => FitFieldSport::HorsebackRiding,
            28 => FitFieldSport::Hunting,
            29 => FitFieldSport::Fishing,
            30 => FitFieldSport::InlineSkating,
            31 => FitFieldSport::RockClimbing,
            32 => FitFieldSport::Sailing,
            33 => FitFieldSport::IceSkating,
            34 => FitFieldSport::SkyDiving,
            35 => FitFieldSport::Snowshoeing,
            36 => FitFieldSport::Snowmobiling,
            37 => FitFieldSport::StandUpPaddleboarding,
            38 => FitFieldSport::Surfing,
            39 => FitFieldSport::Wakeboarding,
            40 => FitFieldSport::WaterSkiing,
            41 => FitFieldSport::Kayaking,
            42 => FitFieldSport::Rafting,
            43 => FitFieldSport::Windsurfing,
            44 => FitFieldSport::Kitesurfing,
            45 => FitFieldSport::Tactical,
            46 => FitFieldSport::Jumpmaster,
            47 => FitFieldSport::Boxing,
            48 => FitFieldSport::FloorClimbing,
            254 => FitFieldSport::All,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSport", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldEvent { // fit base type: enum
    Timer = 0,  // Group 0.  Start / stop_all
    Workout = 3,  // start / stop
    WorkoutStep = 4,  // Start at beginning of workout.  Stop at end of each step.
    PowerDown = 5,  // stop_all group 0
    PowerUp = 6,  // stop_all group 0
    OffCourse = 7,  // start / stop group 0
    Session = 8,  // Stop at end of each session.
    Lap = 9,  // Stop at end of each lap.
    CoursePoint = 10,  // marker
    Battery = 11,  // marker
    VirtualPartnerPace = 12,  // Group 1. Start at beginning of activity if VP enabled, when VP pace is changed during activity or VP enabled mid activity.  stop_disable when VP disabled.
    HrHighAlert = 13,  // Group 0.  Start / stop when in alert condition.
    HrLowAlert = 14,  // Group 0.  Start / stop when in alert condition.
    SpeedHighAlert = 15,  // Group 0.  Start / stop when in alert condition.
    SpeedLowAlert = 16,  // Group 0.  Start / stop when in alert condition.
    CadHighAlert = 17,  // Group 0.  Start / stop when in alert condition.
    CadLowAlert = 18,  // Group 0.  Start / stop when in alert condition.
    PowerHighAlert = 19,  // Group 0.  Start / stop when in alert condition.
    PowerLowAlert = 20,  // Group 0.  Start / stop when in alert condition.
    RecoveryHr = 21,  // marker
    BatteryLow = 22,  // marker
    TimeDurationAlert = 23,  // Group 1.  Start if enabled mid activity (not required at start of activity). Stop when duration is reached.  stop_disable if disabled.
    DistanceDurationAlert = 24,  // Group 1.  Start if enabled mid activity (not required at start of activity). Stop when duration is reached.  stop_disable if disabled.
    CalorieDurationAlert = 25,  // Group 1.  Start if enabled mid activity (not required at start of activity). Stop when duration is reached.  stop_disable if disabled.
    Activity = 26,  // Group 1..  Stop at end of activity.
    FitnessEquipment = 27,  // marker
    Length = 28,  // Stop at end of each length.
    UserMarker = 32,  // marker
    SportPoint = 33,  // marker
    Calibration = 36,  // start/stop/marker
    FrontGearChange = 42,  // marker
    RearGearChange = 43,  // marker
    RiderPositionChange = 44,  // marker
    ElevHighAlert = 45,  // Group 0.  Start / stop when in alert condition.
    ElevLowAlert = 46,  // Group 0.  Start / stop when in alert condition.
    CommTimeout = 47,  // marker
}

impl FitFieldEvent {
    pub fn parse(input: &[u8]) -> Result<(FitFieldEvent, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldEvent::from(val), o))
    }
}

impl From<u8> for FitFieldEvent {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldEvent::Timer,
            3 => FitFieldEvent::Workout,
            4 => FitFieldEvent::WorkoutStep,
            5 => FitFieldEvent::PowerDown,
            6 => FitFieldEvent::PowerUp,
            7 => FitFieldEvent::OffCourse,
            8 => FitFieldEvent::Session,
            9 => FitFieldEvent::Lap,
            10 => FitFieldEvent::CoursePoint,
            11 => FitFieldEvent::Battery,
            12 => FitFieldEvent::VirtualPartnerPace,
            13 => FitFieldEvent::HrHighAlert,
            14 => FitFieldEvent::HrLowAlert,
            15 => FitFieldEvent::SpeedHighAlert,
            16 => FitFieldEvent::SpeedLowAlert,
            17 => FitFieldEvent::CadHighAlert,
            18 => FitFieldEvent::CadLowAlert,
            19 => FitFieldEvent::PowerHighAlert,
            20 => FitFieldEvent::PowerLowAlert,
            21 => FitFieldEvent::RecoveryHr,
            22 => FitFieldEvent::BatteryLow,
            23 => FitFieldEvent::TimeDurationAlert,
            24 => FitFieldEvent::DistanceDurationAlert,
            25 => FitFieldEvent::CalorieDurationAlert,
            26 => FitFieldEvent::Activity,
            27 => FitFieldEvent::FitnessEquipment,
            28 => FitFieldEvent::Length,
            32 => FitFieldEvent::UserMarker,
            33 => FitFieldEvent::SportPoint,
            36 => FitFieldEvent::Calibration,
            42 => FitFieldEvent::FrontGearChange,
            43 => FitFieldEvent::RearGearChange,
            44 => FitFieldEvent::RiderPositionChange,
            45 => FitFieldEvent::ElevHighAlert,
            46 => FitFieldEvent::ElevLowAlert,
            47 => FitFieldEvent::CommTimeout,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldEvent", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDisplayPower { // fit base type: enum
    Watts = 0,
    PercentFtp = 1,
}

impl FitFieldDisplayPower {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDisplayPower, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDisplayPower::from(val), o))
    }
}

impl From<u8> for FitFieldDisplayPower {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDisplayPower::Watts,
            1 => FitFieldDisplayPower::PercentFtp,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDisplayPower", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldBikeLightNetworkConfigType { // fit base type: enum
    Auto = 0,
    Individual = 4,
    HighVisibility = 5,
    Trail = 6,
}

impl FitFieldBikeLightNetworkConfigType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldBikeLightNetworkConfigType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldBikeLightNetworkConfigType::from(val), o))
    }
}

impl From<u8> for FitFieldBikeLightNetworkConfigType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldBikeLightNetworkConfigType::Auto,
            4 => FitFieldBikeLightNetworkConfigType::Individual,
            5 => FitFieldBikeLightNetworkConfigType::HighVisibility,
            6 => FitFieldBikeLightNetworkConfigType::Trail,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldBikeLightNetworkConfigType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldBpStatus { // fit base type: enum
    NoError = 0,
    ErrorIncompleteData = 1,
    ErrorNoMeasurement = 2,
    ErrorDataOutOfRange = 3,
    ErrorIrregularHeartRate = 4,
}

impl FitFieldBpStatus {
    pub fn parse(input: &[u8]) -> Result<(FitFieldBpStatus, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldBpStatus::from(val), o))
    }
}

impl From<u8> for FitFieldBpStatus {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldBpStatus::NoError,
            1 => FitFieldBpStatus::ErrorIncompleteData,
            2 => FitFieldBpStatus::ErrorNoMeasurement,
            3 => FitFieldBpStatus::ErrorDataOutOfRange,
            4 => FitFieldBpStatus::ErrorIrregularHeartRate,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldBpStatus", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldBikeLightBeamAngleMode { // fit base type: uint8
    Manual = 0,
    Auto = 1,
}

impl FitFieldBikeLightBeamAngleMode {
    pub fn parse(input: &[u8]) -> Result<(FitFieldBikeLightBeamAngleMode, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldBikeLightBeamAngleMode::from(val), o))
    }
}

impl From<u8> for FitFieldBikeLightBeamAngleMode {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldBikeLightBeamAngleMode::Manual,
            1 => FitFieldBikeLightBeamAngleMode::Auto,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldBikeLightBeamAngleMode", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLocaltimeIntoDay { // fit base type: uint32
}

impl FitFieldLocaltimeIntoDay {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldLocaltimeIntoDay, &[u8])> {
        let (val, o) = parse_uint32(input, endianness)?;
        Ok((FitFieldLocaltimeIntoDay::from(val), o))
    }
}

impl From<u32> for FitFieldLocaltimeIntoDay {
    fn from(code: u32) -> Self {
        match code {
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLocaltimeIntoDay", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDisplayPosition { // fit base type: enum
    Degree = 0,  // dd.dddddd
    DegreeMinute = 1,  // dddmm.mmm
    DegreeMinuteSecond = 2,  // dddmmss
    AustrianGrid = 3,  // Austrian Grid (BMN)
    BritishGrid = 4,  // British National Grid
    DutchGrid = 5,  // Dutch grid system
    HungarianGrid = 6,  // Hungarian grid system
    FinnishGrid = 7,  // Finnish grid system Zone3 KKJ27
    GermanGrid = 8,  // Gausss Krueger (German)
    IcelandicGrid = 9,  // Icelandic Grid
    IndonesianEquatorial = 10,  // Indonesian Equatorial LCO
    IndonesianIrian = 11,  // Indonesian Irian LCO
    IndonesianSouthern = 12,  // Indonesian Southern LCO
    IndiaZone0 = 13,  // India zone 0
    IndiaZoneIA = 14,  // India zone IA
    IndiaZoneIB = 15,  // India zone IB
    IndiaZoneIIA = 16,  // India zone IIA
    IndiaZoneIIB = 17,  // India zone IIB
    IndiaZoneIIIA = 18,  // India zone IIIA
    IndiaZoneIIIB = 19,  // India zone IIIB
    IndiaZoneIVA = 20,  // India zone IVA
    IndiaZoneIVB = 21,  // India zone IVB
    IrishTransverse = 22,  // Irish Transverse Mercator
    IrishGrid = 23,  // Irish Grid
    Loran = 24,  // Loran TD
    MaidenheadGrid = 25,  // Maidenhead grid system
    MgrsGrid = 26,  // MGRS grid system
    NewZealandGrid = 27,  // New Zealand grid system
    NewZealandTransverse = 28,  // New Zealand Transverse Mercator
    QatarGrid = 29,  // Qatar National Grid
    ModifiedSwedishGrid = 30,  // Modified RT-90 (Sweden)
    SwedishGrid = 31,  // RT-90 (Sweden)
    SouthAfricanGrid = 32,  // South African Grid
    SwissGrid = 33,  // Swiss CH-1903 grid
    TaiwanGrid = 34,  // Taiwan Grid
    UnitedStatesGrid = 35,  // United States National Grid
    UtmUpsGrid = 36,  // UTM/UPS grid system
    WestMalayan = 37,  // West Malayan RSO
    BorneoRso = 38,  // Borneo RSO
    EstonianGrid = 39,  // Estonian grid system
    LatvianGrid = 40,  // Latvian Transverse Mercator
    SwedishRef99Grid = 41,  // Reference Grid 99 TM (Swedish)
}

impl FitFieldDisplayPosition {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDisplayPosition, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDisplayPosition::from(val), o))
    }
}

impl From<u8> for FitFieldDisplayPosition {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDisplayPosition::Degree,
            1 => FitFieldDisplayPosition::DegreeMinute,
            2 => FitFieldDisplayPosition::DegreeMinuteSecond,
            3 => FitFieldDisplayPosition::AustrianGrid,
            4 => FitFieldDisplayPosition::BritishGrid,
            5 => FitFieldDisplayPosition::DutchGrid,
            6 => FitFieldDisplayPosition::HungarianGrid,
            7 => FitFieldDisplayPosition::FinnishGrid,
            8 => FitFieldDisplayPosition::GermanGrid,
            9 => FitFieldDisplayPosition::IcelandicGrid,
            10 => FitFieldDisplayPosition::IndonesianEquatorial,
            11 => FitFieldDisplayPosition::IndonesianIrian,
            12 => FitFieldDisplayPosition::IndonesianSouthern,
            13 => FitFieldDisplayPosition::IndiaZone0,
            14 => FitFieldDisplayPosition::IndiaZoneIA,
            15 => FitFieldDisplayPosition::IndiaZoneIB,
            16 => FitFieldDisplayPosition::IndiaZoneIIA,
            17 => FitFieldDisplayPosition::IndiaZoneIIB,
            18 => FitFieldDisplayPosition::IndiaZoneIIIA,
            19 => FitFieldDisplayPosition::IndiaZoneIIIB,
            20 => FitFieldDisplayPosition::IndiaZoneIVA,
            21 => FitFieldDisplayPosition::IndiaZoneIVB,
            22 => FitFieldDisplayPosition::IrishTransverse,
            23 => FitFieldDisplayPosition::IrishGrid,
            24 => FitFieldDisplayPosition::Loran,
            25 => FitFieldDisplayPosition::MaidenheadGrid,
            26 => FitFieldDisplayPosition::MgrsGrid,
            27 => FitFieldDisplayPosition::NewZealandGrid,
            28 => FitFieldDisplayPosition::NewZealandTransverse,
            29 => FitFieldDisplayPosition::QatarGrid,
            30 => FitFieldDisplayPosition::ModifiedSwedishGrid,
            31 => FitFieldDisplayPosition::SwedishGrid,
            32 => FitFieldDisplayPosition::SouthAfricanGrid,
            33 => FitFieldDisplayPosition::SwissGrid,
            34 => FitFieldDisplayPosition::TaiwanGrid,
            35 => FitFieldDisplayPosition::UnitedStatesGrid,
            36 => FitFieldDisplayPosition::UtmUpsGrid,
            37 => FitFieldDisplayPosition::WestMalayan,
            38 => FitFieldDisplayPosition::BorneoRso,
            39 => FitFieldDisplayPosition::EstonianGrid,
            40 => FitFieldDisplayPosition::LatvianGrid,
            41 => FitFieldDisplayPosition::SwedishRef99Grid,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDisplayPosition", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldGoalRecurrence { // fit base type: enum
    Off = 0,
    Daily = 1,
    Weekly = 2,
    Monthly = 3,
    Yearly = 4,
    Custom = 5,
}

impl FitFieldGoalRecurrence {
    pub fn parse(input: &[u8]) -> Result<(FitFieldGoalRecurrence, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldGoalRecurrence::from(val), o))
    }
}

impl From<u8> for FitFieldGoalRecurrence {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldGoalRecurrence::Off,
            1 => FitFieldGoalRecurrence::Daily,
            2 => FitFieldGoalRecurrence::Weekly,
            3 => FitFieldGoalRecurrence::Monthly,
            4 => FitFieldGoalRecurrence::Yearly,
            5 => FitFieldGoalRecurrence::Custom,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldGoalRecurrence", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSchedule { // fit base type: enum
    Workout = 0,
    Course = 1,
}

impl FitFieldSchedule {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSchedule, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSchedule::from(val), o))
    }
}

impl From<u8> for FitFieldSchedule {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSchedule::Workout,
            1 => FitFieldSchedule::Course,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSchedule", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLeftRightBalance100 { // fit base type: uint16
    Mask = 16383,  // % contribution scaled by 100
    Right = 32768,  // data corresponds to right if set, otherwise unknown
}

impl FitFieldLeftRightBalance100 {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldLeftRightBalance100, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldLeftRightBalance100::from(val), o))
    }
}

impl From<u16> for FitFieldLeftRightBalance100 {
    fn from(code: u16) -> Self {
        match code {
            16383 => FitFieldLeftRightBalance100::Mask,
            32768 => FitFieldLeftRightBalance100::Right,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLeftRightBalance100", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldCommTimeoutType { // fit base type: uint16
    WildcardPairingTimeout = 0,  // Timeout pairing to any device
    PairingTimeout = 1,  // Timeout pairing to previously paired device
    ConnectionLost = 2,  // Temporary loss of communications
    ConnectionTimeout = 3,  // Connection closed due to extended bad communications
}

impl FitFieldCommTimeoutType {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldCommTimeoutType, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldCommTimeoutType::from(val), o))
    }
}

impl From<u16> for FitFieldCommTimeoutType {
    fn from(code: u16) -> Self {
        match code {
            0 => FitFieldCommTimeoutType::WildcardPairingTimeout,
            1 => FitFieldCommTimeoutType::PairingTimeout,
            2 => FitFieldCommTimeoutType::ConnectionLost,
            3 => FitFieldCommTimeoutType::ConnectionTimeout,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldCommTimeoutType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSegmentLapStatus { // fit base type: enum
    End = 0,
    Fail = 1,
}

impl FitFieldSegmentLapStatus {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSegmentLapStatus, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSegmentLapStatus::from(val), o))
    }
}

impl From<u8> for FitFieldSegmentLapStatus {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSegmentLapStatus::End,
            1 => FitFieldSegmentLapStatus::Fail,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSegmentLapStatus", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldManufacturer { // fit base type: uint16
    Garmin = 1,
    GarminFr405Antfs = 2,  // Do not use.  Used by FR405 for ANTFS man id.
    Zephyr = 3,
    Dayton = 4,
    Idt = 5,
    Srm = 6,
    Quarq = 7,
    Ibike = 8,
    Saris = 9,
    SparkHk = 10,
    Tanita = 11,
    Echowell = 12,
    DynastreamOem = 13,
    Nautilus = 14,
    Dynastream = 15,
    Timex = 16,
    Metrigear = 17,
    Xelic = 18,
    Beurer = 19,
    Cardiosport = 20,
    AAndD = 21,
    Hmm = 22,
    Suunto = 23,
    ThitaElektronik = 24,
    Gpulse = 25,
    CleanMobile = 26,
    PedalBrain = 27,
    Peaksware = 28,
    Saxonar = 29,
    LemondFitness = 30,
    Dexcom = 31,
    WahooFitness = 32,
    OctaneFitness = 33,
    Archinoetics = 34,
    TheHurtBox = 35,
    CitizenSystems = 36,
    Magellan = 37,
    Osynce = 38,
    Holux = 39,
    Concept2 = 40,
    OneGiantLeap = 42,
    AceSensor = 43,
    BrimBrothers = 44,
    Xplova = 45,
    PerceptionDigital = 46,
    Bf1systems = 47,
    Pioneer = 48,
    Spantec = 49,
    Metalogics = 50,
    SeikoEpson = 52,
    SeikoEpsonOem = 53,
    IforPowell = 54,
    MaxwellGuider = 55,
    StarTrac = 56,
    Breakaway = 57,
    AlatechTechnologyLtd = 58,
    MioTechnologyEurope = 59,
    Rotor = 60,
    Geonaute = 61,
    IdBike = 62,
    Specialized = 63,
    Wtek = 64,
    PhysicalEnterprises = 65,
    NorthPoleEngineering = 66,
    Bkool = 67,
    Cateye = 68,
    StagesCycling = 69,
    Sigmasport = 70,
    Tomtom = 71,
    Peripedal = 72,
    Wattbike = 73,
    Moxy = 76,
    Ciclosport = 77,
    Powerbahn = 78,
    AcornProjectsAps = 79,
    Lifebeam = 80,
    Bontrager = 81,
    Wellgo = 82,
    Scosche = 83,
    Magura = 84,
    Woodway = 85,
    Elite = 86,
    NielsenKellerman = 87,
    DkCity = 88,
    Tacx = 89,
    DirectionTechnology = 90,
    Magtonic = 91,
    InsideRideTechnologies = 93,
    SoundOfMotion = 94,
    Stryd = 95,
    Icg = 96,  // Indoorcycling Group
    MiPulse = 97,
    BsxAthletics = 98,
    Look = 99,
    CampagnoloSrl = 100,
    BodyBikeSmart = 101,
    Praxisworks = 102,
    LimitsTechnology = 103,  // Limits Technology Ltd.
    TopactionTechnology = 104,  // TopAction Technology Inc.
    Cosinuss = 105,
    Fitcare = 106,
    Magene = 107,
    GiantManufacturingCo = 108,
    Tigrasport = 109,  // Tigrasport
    Development = 255,
    Healthandlife = 257,
    Lezyne = 258,
    ScribeLabs = 259,
    Zwift = 260,
    Watteam = 261,
    Recon = 262,
    FaveroElectronics = 263,
    Dynovelo = 264,
    Strava = 265,
    Precor = 266,  // Amer Sports
    Bryton = 267,
    Sram = 268,
    Navman = 269,  // MiTAC Global Corporation (Mio Technology)
    Cobi = 270,  // COBI GmbH
    Spivi = 271,
    MioMagellan = 272,
    Evesports = 273,
    SensitivusGauge = 274,
    Podoon = 275,
    Actigraphcorp = 5759,
}

impl FitFieldManufacturer {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldManufacturer, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldManufacturer::from(val), o))
    }
}

impl From<u16> for FitFieldManufacturer {
    fn from(code: u16) -> Self {
        match code {
            1 => FitFieldManufacturer::Garmin,
            2 => FitFieldManufacturer::GarminFr405Antfs,
            3 => FitFieldManufacturer::Zephyr,
            4 => FitFieldManufacturer::Dayton,
            5 => FitFieldManufacturer::Idt,
            6 => FitFieldManufacturer::Srm,
            7 => FitFieldManufacturer::Quarq,
            8 => FitFieldManufacturer::Ibike,
            9 => FitFieldManufacturer::Saris,
            10 => FitFieldManufacturer::SparkHk,
            11 => FitFieldManufacturer::Tanita,
            12 => FitFieldManufacturer::Echowell,
            13 => FitFieldManufacturer::DynastreamOem,
            14 => FitFieldManufacturer::Nautilus,
            15 => FitFieldManufacturer::Dynastream,
            16 => FitFieldManufacturer::Timex,
            17 => FitFieldManufacturer::Metrigear,
            18 => FitFieldManufacturer::Xelic,
            19 => FitFieldManufacturer::Beurer,
            20 => FitFieldManufacturer::Cardiosport,
            21 => FitFieldManufacturer::AAndD,
            22 => FitFieldManufacturer::Hmm,
            23 => FitFieldManufacturer::Suunto,
            24 => FitFieldManufacturer::ThitaElektronik,
            25 => FitFieldManufacturer::Gpulse,
            26 => FitFieldManufacturer::CleanMobile,
            27 => FitFieldManufacturer::PedalBrain,
            28 => FitFieldManufacturer::Peaksware,
            29 => FitFieldManufacturer::Saxonar,
            30 => FitFieldManufacturer::LemondFitness,
            31 => FitFieldManufacturer::Dexcom,
            32 => FitFieldManufacturer::WahooFitness,
            33 => FitFieldManufacturer::OctaneFitness,
            34 => FitFieldManufacturer::Archinoetics,
            35 => FitFieldManufacturer::TheHurtBox,
            36 => FitFieldManufacturer::CitizenSystems,
            37 => FitFieldManufacturer::Magellan,
            38 => FitFieldManufacturer::Osynce,
            39 => FitFieldManufacturer::Holux,
            40 => FitFieldManufacturer::Concept2,
            42 => FitFieldManufacturer::OneGiantLeap,
            43 => FitFieldManufacturer::AceSensor,
            44 => FitFieldManufacturer::BrimBrothers,
            45 => FitFieldManufacturer::Xplova,
            46 => FitFieldManufacturer::PerceptionDigital,
            47 => FitFieldManufacturer::Bf1systems,
            48 => FitFieldManufacturer::Pioneer,
            49 => FitFieldManufacturer::Spantec,
            50 => FitFieldManufacturer::Metalogics,
            52 => FitFieldManufacturer::SeikoEpson,
            53 => FitFieldManufacturer::SeikoEpsonOem,
            54 => FitFieldManufacturer::IforPowell,
            55 => FitFieldManufacturer::MaxwellGuider,
            56 => FitFieldManufacturer::StarTrac,
            57 => FitFieldManufacturer::Breakaway,
            58 => FitFieldManufacturer::AlatechTechnologyLtd,
            59 => FitFieldManufacturer::MioTechnologyEurope,
            60 => FitFieldManufacturer::Rotor,
            61 => FitFieldManufacturer::Geonaute,
            62 => FitFieldManufacturer::IdBike,
            63 => FitFieldManufacturer::Specialized,
            64 => FitFieldManufacturer::Wtek,
            65 => FitFieldManufacturer::PhysicalEnterprises,
            66 => FitFieldManufacturer::NorthPoleEngineering,
            67 => FitFieldManufacturer::Bkool,
            68 => FitFieldManufacturer::Cateye,
            69 => FitFieldManufacturer::StagesCycling,
            70 => FitFieldManufacturer::Sigmasport,
            71 => FitFieldManufacturer::Tomtom,
            72 => FitFieldManufacturer::Peripedal,
            73 => FitFieldManufacturer::Wattbike,
            76 => FitFieldManufacturer::Moxy,
            77 => FitFieldManufacturer::Ciclosport,
            78 => FitFieldManufacturer::Powerbahn,
            79 => FitFieldManufacturer::AcornProjectsAps,
            80 => FitFieldManufacturer::Lifebeam,
            81 => FitFieldManufacturer::Bontrager,
            82 => FitFieldManufacturer::Wellgo,
            83 => FitFieldManufacturer::Scosche,
            84 => FitFieldManufacturer::Magura,
            85 => FitFieldManufacturer::Woodway,
            86 => FitFieldManufacturer::Elite,
            87 => FitFieldManufacturer::NielsenKellerman,
            88 => FitFieldManufacturer::DkCity,
            89 => FitFieldManufacturer::Tacx,
            90 => FitFieldManufacturer::DirectionTechnology,
            91 => FitFieldManufacturer::Magtonic,
            93 => FitFieldManufacturer::InsideRideTechnologies,
            94 => FitFieldManufacturer::SoundOfMotion,
            95 => FitFieldManufacturer::Stryd,
            96 => FitFieldManufacturer::Icg,
            97 => FitFieldManufacturer::MiPulse,
            98 => FitFieldManufacturer::BsxAthletics,
            99 => FitFieldManufacturer::Look,
            100 => FitFieldManufacturer::CampagnoloSrl,
            101 => FitFieldManufacturer::BodyBikeSmart,
            102 => FitFieldManufacturer::Praxisworks,
            103 => FitFieldManufacturer::LimitsTechnology,
            104 => FitFieldManufacturer::TopactionTechnology,
            105 => FitFieldManufacturer::Cosinuss,
            106 => FitFieldManufacturer::Fitcare,
            107 => FitFieldManufacturer::Magene,
            108 => FitFieldManufacturer::GiantManufacturingCo,
            109 => FitFieldManufacturer::Tigrasport,
            255 => FitFieldManufacturer::Development,
            257 => FitFieldManufacturer::Healthandlife,
            258 => FitFieldManufacturer::Lezyne,
            259 => FitFieldManufacturer::ScribeLabs,
            260 => FitFieldManufacturer::Zwift,
            261 => FitFieldManufacturer::Watteam,
            262 => FitFieldManufacturer::Recon,
            263 => FitFieldManufacturer::FaveroElectronics,
            264 => FitFieldManufacturer::Dynovelo,
            265 => FitFieldManufacturer::Strava,
            266 => FitFieldManufacturer::Precor,
            267 => FitFieldManufacturer::Bryton,
            268 => FitFieldManufacturer::Sram,
            269 => FitFieldManufacturer::Navman,
            270 => FitFieldManufacturer::Cobi,
            271 => FitFieldManufacturer::Spivi,
            272 => FitFieldManufacturer::MioMagellan,
            273 => FitFieldManufacturer::Evesports,
            274 => FitFieldManufacturer::SensitivusGauge,
            275 => FitFieldManufacturer::Podoon,
            5759 => FitFieldManufacturer::Actigraphcorp,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldManufacturer", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLengthType { // fit base type: enum
    Idle = 0,  // Rest period. Length with no strokes
    Active = 1,  // Length with strokes.
}

impl FitFieldLengthType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLengthType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldLengthType::from(val), o))
    }
}

impl From<u8> for FitFieldLengthType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldLengthType::Idle,
            1 => FitFieldLengthType::Active,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLengthType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLanguage { // fit base type: enum
    English = 0,
    French = 1,
    Italian = 2,
    German = 3,
    Spanish = 4,
    Croatian = 5,
    Czech = 6,
    Danish = 7,
    Dutch = 8,
    Finnish = 9,
    Greek = 10,
    Hungarian = 11,
    Norwegian = 12,
    Polish = 13,
    Portuguese = 14,
    Slovakian = 15,
    Slovenian = 16,
    Swedish = 17,
    Russian = 18,
    Turkish = 19,
    Latvian = 20,
    Ukrainian = 21,
    Arabic = 22,
    Farsi = 23,
    Bulgarian = 24,
    Romanian = 25,
    Chinese = 26,
    Japanese = 27,
    Korean = 28,
    Taiwanese = 29,
    Thai = 30,
    Hebrew = 31,
    BrazilianPortuguese = 32,
    Indonesian = 33,
    Custom = 254,
}

impl FitFieldLanguage {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLanguage, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldLanguage::from(val), o))
    }
}

impl From<u8> for FitFieldLanguage {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldLanguage::English,
            1 => FitFieldLanguage::French,
            2 => FitFieldLanguage::Italian,
            3 => FitFieldLanguage::German,
            4 => FitFieldLanguage::Spanish,
            5 => FitFieldLanguage::Croatian,
            6 => FitFieldLanguage::Czech,
            7 => FitFieldLanguage::Danish,
            8 => FitFieldLanguage::Dutch,
            9 => FitFieldLanguage::Finnish,
            10 => FitFieldLanguage::Greek,
            11 => FitFieldLanguage::Hungarian,
            12 => FitFieldLanguage::Norwegian,
            13 => FitFieldLanguage::Polish,
            14 => FitFieldLanguage::Portuguese,
            15 => FitFieldLanguage::Slovakian,
            16 => FitFieldLanguage::Slovenian,
            17 => FitFieldLanguage::Swedish,
            18 => FitFieldLanguage::Russian,
            19 => FitFieldLanguage::Turkish,
            20 => FitFieldLanguage::Latvian,
            21 => FitFieldLanguage::Ukrainian,
            22 => FitFieldLanguage::Arabic,
            23 => FitFieldLanguage::Farsi,
            24 => FitFieldLanguage::Bulgarian,
            25 => FitFieldLanguage::Romanian,
            26 => FitFieldLanguage::Chinese,
            27 => FitFieldLanguage::Japanese,
            28 => FitFieldLanguage::Korean,
            29 => FitFieldLanguage::Taiwanese,
            30 => FitFieldLanguage::Thai,
            31 => FitFieldLanguage::Hebrew,
            32 => FitFieldLanguage::BrazilianPortuguese,
            33 => FitFieldLanguage::Indonesian,
            254 => FitFieldLanguage::Custom,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLanguage", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldActivityClass { // fit base type: enum
    Level = 127,  // 0 to 100
    LevelMax = 100,
    Athlete = 128,
}

impl FitFieldActivityClass {
    pub fn parse(input: &[u8]) -> Result<(FitFieldActivityClass, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldActivityClass::from(val), o))
    }
}

impl From<u8> for FitFieldActivityClass {
    fn from(code: u8) -> Self {
        match code {
            127 => FitFieldActivityClass::Level,
            100 => FitFieldActivityClass::LevelMax,
            128 => FitFieldActivityClass::Athlete,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldActivityClass", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAttitudeValidity { // fit base type: uint16
    TrackAngleHeadingValid = 1,
    PitchValid = 2,
    RollValid = 4,
    LateralBodyAccelValid = 8,
    NormalBodyAccelValid = 16,
    TurnRateValid = 32,
    HwFail = 64,
    MagInvalid = 128,
    NoGps = 256,
    GpsInvalid = 512,
    SolutionCoasting = 1024,
    TrueTrackAngle = 2048,
    MagneticHeading = 4096,
}

impl FitFieldAttitudeValidity {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldAttitudeValidity, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldAttitudeValidity::from(val), o))
    }
}

impl From<u16> for FitFieldAttitudeValidity {
    fn from(code: u16) -> Self {
        match code {
            1 => FitFieldAttitudeValidity::TrackAngleHeadingValid,
            2 => FitFieldAttitudeValidity::PitchValid,
            4 => FitFieldAttitudeValidity::RollValid,
            8 => FitFieldAttitudeValidity::LateralBodyAccelValid,
            16 => FitFieldAttitudeValidity::NormalBodyAccelValid,
            32 => FitFieldAttitudeValidity::TurnRateValid,
            64 => FitFieldAttitudeValidity::HwFail,
            128 => FitFieldAttitudeValidity::MagInvalid,
            256 => FitFieldAttitudeValidity::NoGps,
            512 => FitFieldAttitudeValidity::GpsInvalid,
            1024 => FitFieldAttitudeValidity::SolutionCoasting,
            2048 => FitFieldAttitudeValidity::TrueTrackAngle,
            4096 => FitFieldAttitudeValidity::MagneticHeading,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAttitudeValidity", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldTimeZone { // fit base type: enum
    Almaty = 0,
    Bangkok = 1,
    Bombay = 2,
    Brasilia = 3,
    Cairo = 4,
    CapeVerdeIs = 5,
    Darwin = 6,
    Eniwetok = 7,
    Fiji = 8,
    HongKong = 9,
    Islamabad = 10,
    Kabul = 11,
    Magadan = 12,
    MidAtlantic = 13,
    Moscow = 14,
    Muscat = 15,
    Newfoundland = 16,
    Samoa = 17,
    Sydney = 18,
    Tehran = 19,
    Tokyo = 20,
    UsAlaska = 21,
    UsAtlantic = 22,
    UsCentral = 23,
    UsEastern = 24,
    UsHawaii = 25,
    UsMountain = 26,
    UsPacific = 27,
    Other = 28,
    Auckland = 29,
    Kathmandu = 30,
    EuropeWesternWet = 31,
    EuropeCentralCet = 32,
    EuropeEasternEet = 33,
    Jakarta = 34,
    Perth = 35,
    Adelaide = 36,
    Brisbane = 37,
    Tasmania = 38,
    Iceland = 39,
    Amsterdam = 40,
    Athens = 41,
    Barcelona = 42,
    Berlin = 43,
    Brussels = 44,
    Budapest = 45,
    Copenhagen = 46,
    Dublin = 47,
    Helsinki = 48,
    Lisbon = 49,
    London = 50,
    Madrid = 51,
    Munich = 52,
    Oslo = 53,
    Paris = 54,
    Prague = 55,
    Reykjavik = 56,
    Rome = 57,
    Stockholm = 58,
    Vienna = 59,
    Warsaw = 60,
    Zurich = 61,
    Quebec = 62,
    Ontario = 63,
    Manitoba = 64,
    Saskatchewan = 65,
    Alberta = 66,
    BritishColumbia = 67,
    Boise = 68,
    Boston = 69,
    Chicago = 70,
    Dallas = 71,
    Denver = 72,
    KansasCity = 73,
    LasVegas = 74,
    LosAngeles = 75,
    Miami = 76,
    Minneapolis = 77,
    NewYork = 78,
    NewOrleans = 79,
    Phoenix = 80,
    SantaFe = 81,
    Seattle = 82,
    WashingtonDc = 83,
    UsArizona = 84,
    Chita = 85,
    Ekaterinburg = 86,
    Irkutsk = 87,
    Kaliningrad = 88,
    Krasnoyarsk = 89,
    Novosibirsk = 90,
    PetropavlovskKamchatskiy = 91,
    Samara = 92,
    Vladivostok = 93,
    MexicoCentral = 94,
    MexicoMountain = 95,
    MexicoPacific = 96,
    CapeTown = 97,
    Winkhoek = 98,
    Lagos = 99,
    Riyahd = 100,
    Venezuela = 101,
    AustraliaLh = 102,
    Santiago = 103,
    Manual = 253,
    Automatic = 254,
}

impl FitFieldTimeZone {
    pub fn parse(input: &[u8]) -> Result<(FitFieldTimeZone, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldTimeZone::from(val), o))
    }
}

impl From<u8> for FitFieldTimeZone {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldTimeZone::Almaty,
            1 => FitFieldTimeZone::Bangkok,
            2 => FitFieldTimeZone::Bombay,
            3 => FitFieldTimeZone::Brasilia,
            4 => FitFieldTimeZone::Cairo,
            5 => FitFieldTimeZone::CapeVerdeIs,
            6 => FitFieldTimeZone::Darwin,
            7 => FitFieldTimeZone::Eniwetok,
            8 => FitFieldTimeZone::Fiji,
            9 => FitFieldTimeZone::HongKong,
            10 => FitFieldTimeZone::Islamabad,
            11 => FitFieldTimeZone::Kabul,
            12 => FitFieldTimeZone::Magadan,
            13 => FitFieldTimeZone::MidAtlantic,
            14 => FitFieldTimeZone::Moscow,
            15 => FitFieldTimeZone::Muscat,
            16 => FitFieldTimeZone::Newfoundland,
            17 => FitFieldTimeZone::Samoa,
            18 => FitFieldTimeZone::Sydney,
            19 => FitFieldTimeZone::Tehran,
            20 => FitFieldTimeZone::Tokyo,
            21 => FitFieldTimeZone::UsAlaska,
            22 => FitFieldTimeZone::UsAtlantic,
            23 => FitFieldTimeZone::UsCentral,
            24 => FitFieldTimeZone::UsEastern,
            25 => FitFieldTimeZone::UsHawaii,
            26 => FitFieldTimeZone::UsMountain,
            27 => FitFieldTimeZone::UsPacific,
            28 => FitFieldTimeZone::Other,
            29 => FitFieldTimeZone::Auckland,
            30 => FitFieldTimeZone::Kathmandu,
            31 => FitFieldTimeZone::EuropeWesternWet,
            32 => FitFieldTimeZone::EuropeCentralCet,
            33 => FitFieldTimeZone::EuropeEasternEet,
            34 => FitFieldTimeZone::Jakarta,
            35 => FitFieldTimeZone::Perth,
            36 => FitFieldTimeZone::Adelaide,
            37 => FitFieldTimeZone::Brisbane,
            38 => FitFieldTimeZone::Tasmania,
            39 => FitFieldTimeZone::Iceland,
            40 => FitFieldTimeZone::Amsterdam,
            41 => FitFieldTimeZone::Athens,
            42 => FitFieldTimeZone::Barcelona,
            43 => FitFieldTimeZone::Berlin,
            44 => FitFieldTimeZone::Brussels,
            45 => FitFieldTimeZone::Budapest,
            46 => FitFieldTimeZone::Copenhagen,
            47 => FitFieldTimeZone::Dublin,
            48 => FitFieldTimeZone::Helsinki,
            49 => FitFieldTimeZone::Lisbon,
            50 => FitFieldTimeZone::London,
            51 => FitFieldTimeZone::Madrid,
            52 => FitFieldTimeZone::Munich,
            53 => FitFieldTimeZone::Oslo,
            54 => FitFieldTimeZone::Paris,
            55 => FitFieldTimeZone::Prague,
            56 => FitFieldTimeZone::Reykjavik,
            57 => FitFieldTimeZone::Rome,
            58 => FitFieldTimeZone::Stockholm,
            59 => FitFieldTimeZone::Vienna,
            60 => FitFieldTimeZone::Warsaw,
            61 => FitFieldTimeZone::Zurich,
            62 => FitFieldTimeZone::Quebec,
            63 => FitFieldTimeZone::Ontario,
            64 => FitFieldTimeZone::Manitoba,
            65 => FitFieldTimeZone::Saskatchewan,
            66 => FitFieldTimeZone::Alberta,
            67 => FitFieldTimeZone::BritishColumbia,
            68 => FitFieldTimeZone::Boise,
            69 => FitFieldTimeZone::Boston,
            70 => FitFieldTimeZone::Chicago,
            71 => FitFieldTimeZone::Dallas,
            72 => FitFieldTimeZone::Denver,
            73 => FitFieldTimeZone::KansasCity,
            74 => FitFieldTimeZone::LasVegas,
            75 => FitFieldTimeZone::LosAngeles,
            76 => FitFieldTimeZone::Miami,
            77 => FitFieldTimeZone::Minneapolis,
            78 => FitFieldTimeZone::NewYork,
            79 => FitFieldTimeZone::NewOrleans,
            80 => FitFieldTimeZone::Phoenix,
            81 => FitFieldTimeZone::SantaFe,
            82 => FitFieldTimeZone::Seattle,
            83 => FitFieldTimeZone::WashingtonDc,
            84 => FitFieldTimeZone::UsArizona,
            85 => FitFieldTimeZone::Chita,
            86 => FitFieldTimeZone::Ekaterinburg,
            87 => FitFieldTimeZone::Irkutsk,
            88 => FitFieldTimeZone::Kaliningrad,
            89 => FitFieldTimeZone::Krasnoyarsk,
            90 => FitFieldTimeZone::Novosibirsk,
            91 => FitFieldTimeZone::PetropavlovskKamchatskiy,
            92 => FitFieldTimeZone::Samara,
            93 => FitFieldTimeZone::Vladivostok,
            94 => FitFieldTimeZone::MexicoCentral,
            95 => FitFieldTimeZone::MexicoMountain,
            96 => FitFieldTimeZone::MexicoPacific,
            97 => FitFieldTimeZone::CapeTown,
            98 => FitFieldTimeZone::Winkhoek,
            99 => FitFieldTimeZone::Lagos,
            100 => FitFieldTimeZone::Riyahd,
            101 => FitFieldTimeZone::Venezuela,
            102 => FitFieldTimeZone::AustraliaLh,
            103 => FitFieldTimeZone::Santiago,
            253 => FitFieldTimeZone::Manual,
            254 => FitFieldTimeZone::Automatic,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldTimeZone", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSegmentSelectionType { // fit base type: enum
    Starred = 0,
    Suggested = 1,
}

impl FitFieldSegmentSelectionType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSegmentSelectionType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSegmentSelectionType::from(val), o))
    }
}

impl From<u8> for FitFieldSegmentSelectionType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSegmentSelectionType::Starred,
            1 => FitFieldSegmentSelectionType::Suggested,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSegmentSelectionType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldUserLocalId { // fit base type: uint16
    LocalMin = 0,
    LocalMax = 15,
    StationaryMin = 16,
    StationaryMax = 255,
    PortableMin = 256,
    PortableMax = 65534,
}

impl FitFieldUserLocalId {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldUserLocalId, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldUserLocalId::from(val), o))
    }
}

impl From<u16> for FitFieldUserLocalId {
    fn from(code: u16) -> Self {
        match code {
            0 => FitFieldUserLocalId::LocalMin,
            15 => FitFieldUserLocalId::LocalMax,
            16 => FitFieldUserLocalId::StationaryMin,
            255 => FitFieldUserLocalId::StationaryMax,
            256 => FitFieldUserLocalId::PortableMin,
            65534 => FitFieldUserLocalId::PortableMax,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldUserLocalId", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldExdDescriptors { // fit base type: enum
    BikeLightBatteryStatus = 0,
    BeamAngleStatus = 1,
    BateryLevel = 2,
    LightNetworkMode = 3,
    NumberLightsConnected = 4,
    Cadence = 5,
    Distance = 6,
    EstimatedTimeOfArrival = 7,
    Heading = 8,
    Time = 9,
    BatteryLevel = 10,
    TrainerResistance = 11,
    TrainerTargetPower = 12,
    TimeSeated = 13,
    TimeStanding = 14,
    Elevation = 15,
    Grade = 16,
    Ascent = 17,
    Descent = 18,
    VerticalSpeed = 19,
    Di2BatteryLevel = 20,
    FrontGear = 21,
    RearGear = 22,
    GearRatio = 23,
    HeartRate = 24,
    HeartRateZone = 25,
    TimeInHeartRateZone = 26,
    HeartRateReserve = 27,
    Calories = 28,
    GpsAccuracy = 29,
    GpsSignalStrength = 30,
    Temperature = 31,
    TimeOfDay = 32,
    Balance = 33,
    PedalSmoothness = 34,
    Power = 35,
    FunctionalThresholdPower = 36,
    IntensityFactor = 37,
    Work = 38,
    PowerRatio = 39,
    NormalizedPower = 40,
    TrainingStressScore = 41,
    TimeOnZone = 42,
    Speed = 43,
    Laps = 44,
    Reps = 45,
    WorkoutStep = 46,
    CourseDistance = 47,
    NavigationDistance = 48,
    CourseEstimatedTimeOfArrival = 49,
    NavigationEstimatedTimeOfArrival = 50,
    CourseTime = 51,
    NavigationTime = 52,
    CourseHeading = 53,
    NavigationHeading = 54,
    PowerZone = 55,
    TorqueEffectiveness = 56,
    TimerTime = 57,
    PowerWeightRatio = 58,
    LeftPlatformCenterOffset = 59,
    RightPlatformCenterOffset = 60,
    LeftPowerPhaseStartAngle = 61,
    RightPowerPhaseStartAngle = 62,
    LeftPowerPhaseFinishAngle = 63,
    RightPowerPhaseFinishAngle = 64,
    Gears = 65,  // Combined gear information
    Pace = 66,
    TrainingEffect = 67,
    VerticalOscillation = 68,
    VerticalRatio = 69,
    GroundContactTime = 70,
    LeftGroundContactTimeBalance = 71,
    RightGroundContactTimeBalance = 72,
    StrideLength = 73,
    RunningCadence = 74,
    PerformanceCondition = 75,
    CourseType = 76,
    TimeInPowerZone = 77,
    NavigationTurn = 78,
    CourseLocation = 79,
    NavigationLocation = 80,
    Compass = 81,
    GearCombo = 82,
    MuscleOxygen = 83,
    Icon = 84,
    CompassHeading = 85,
    GpsHeading = 86,
    GpsElevation = 87,
    AnaerobicTrainingEffect = 88,
    Course = 89,
    OffCourse = 90,
    GlideRatio = 91,
    VerticalDistance = 92,
    Vmg = 93,
    AmbientPressure = 94,
    Pressure = 95,
}

impl FitFieldExdDescriptors {
    pub fn parse(input: &[u8]) -> Result<(FitFieldExdDescriptors, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldExdDescriptors::from(val), o))
    }
}

impl From<u8> for FitFieldExdDescriptors {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldExdDescriptors::BikeLightBatteryStatus,
            1 => FitFieldExdDescriptors::BeamAngleStatus,
            2 => FitFieldExdDescriptors::BateryLevel,
            3 => FitFieldExdDescriptors::LightNetworkMode,
            4 => FitFieldExdDescriptors::NumberLightsConnected,
            5 => FitFieldExdDescriptors::Cadence,
            6 => FitFieldExdDescriptors::Distance,
            7 => FitFieldExdDescriptors::EstimatedTimeOfArrival,
            8 => FitFieldExdDescriptors::Heading,
            9 => FitFieldExdDescriptors::Time,
            10 => FitFieldExdDescriptors::BatteryLevel,
            11 => FitFieldExdDescriptors::TrainerResistance,
            12 => FitFieldExdDescriptors::TrainerTargetPower,
            13 => FitFieldExdDescriptors::TimeSeated,
            14 => FitFieldExdDescriptors::TimeStanding,
            15 => FitFieldExdDescriptors::Elevation,
            16 => FitFieldExdDescriptors::Grade,
            17 => FitFieldExdDescriptors::Ascent,
            18 => FitFieldExdDescriptors::Descent,
            19 => FitFieldExdDescriptors::VerticalSpeed,
            20 => FitFieldExdDescriptors::Di2BatteryLevel,
            21 => FitFieldExdDescriptors::FrontGear,
            22 => FitFieldExdDescriptors::RearGear,
            23 => FitFieldExdDescriptors::GearRatio,
            24 => FitFieldExdDescriptors::HeartRate,
            25 => FitFieldExdDescriptors::HeartRateZone,
            26 => FitFieldExdDescriptors::TimeInHeartRateZone,
            27 => FitFieldExdDescriptors::HeartRateReserve,
            28 => FitFieldExdDescriptors::Calories,
            29 => FitFieldExdDescriptors::GpsAccuracy,
            30 => FitFieldExdDescriptors::GpsSignalStrength,
            31 => FitFieldExdDescriptors::Temperature,
            32 => FitFieldExdDescriptors::TimeOfDay,
            33 => FitFieldExdDescriptors::Balance,
            34 => FitFieldExdDescriptors::PedalSmoothness,
            35 => FitFieldExdDescriptors::Power,
            36 => FitFieldExdDescriptors::FunctionalThresholdPower,
            37 => FitFieldExdDescriptors::IntensityFactor,
            38 => FitFieldExdDescriptors::Work,
            39 => FitFieldExdDescriptors::PowerRatio,
            40 => FitFieldExdDescriptors::NormalizedPower,
            41 => FitFieldExdDescriptors::TrainingStressScore,
            42 => FitFieldExdDescriptors::TimeOnZone,
            43 => FitFieldExdDescriptors::Speed,
            44 => FitFieldExdDescriptors::Laps,
            45 => FitFieldExdDescriptors::Reps,
            46 => FitFieldExdDescriptors::WorkoutStep,
            47 => FitFieldExdDescriptors::CourseDistance,
            48 => FitFieldExdDescriptors::NavigationDistance,
            49 => FitFieldExdDescriptors::CourseEstimatedTimeOfArrival,
            50 => FitFieldExdDescriptors::NavigationEstimatedTimeOfArrival,
            51 => FitFieldExdDescriptors::CourseTime,
            52 => FitFieldExdDescriptors::NavigationTime,
            53 => FitFieldExdDescriptors::CourseHeading,
            54 => FitFieldExdDescriptors::NavigationHeading,
            55 => FitFieldExdDescriptors::PowerZone,
            56 => FitFieldExdDescriptors::TorqueEffectiveness,
            57 => FitFieldExdDescriptors::TimerTime,
            58 => FitFieldExdDescriptors::PowerWeightRatio,
            59 => FitFieldExdDescriptors::LeftPlatformCenterOffset,
            60 => FitFieldExdDescriptors::RightPlatformCenterOffset,
            61 => FitFieldExdDescriptors::LeftPowerPhaseStartAngle,
            62 => FitFieldExdDescriptors::RightPowerPhaseStartAngle,
            63 => FitFieldExdDescriptors::LeftPowerPhaseFinishAngle,
            64 => FitFieldExdDescriptors::RightPowerPhaseFinishAngle,
            65 => FitFieldExdDescriptors::Gears,
            66 => FitFieldExdDescriptors::Pace,
            67 => FitFieldExdDescriptors::TrainingEffect,
            68 => FitFieldExdDescriptors::VerticalOscillation,
            69 => FitFieldExdDescriptors::VerticalRatio,
            70 => FitFieldExdDescriptors::GroundContactTime,
            71 => FitFieldExdDescriptors::LeftGroundContactTimeBalance,
            72 => FitFieldExdDescriptors::RightGroundContactTimeBalance,
            73 => FitFieldExdDescriptors::StrideLength,
            74 => FitFieldExdDescriptors::RunningCadence,
            75 => FitFieldExdDescriptors::PerformanceCondition,
            76 => FitFieldExdDescriptors::CourseType,
            77 => FitFieldExdDescriptors::TimeInPowerZone,
            78 => FitFieldExdDescriptors::NavigationTurn,
            79 => FitFieldExdDescriptors::CourseLocation,
            80 => FitFieldExdDescriptors::NavigationLocation,
            81 => FitFieldExdDescriptors::Compass,
            82 => FitFieldExdDescriptors::GearCombo,
            83 => FitFieldExdDescriptors::MuscleOxygen,
            84 => FitFieldExdDescriptors::Icon,
            85 => FitFieldExdDescriptors::CompassHeading,
            86 => FitFieldExdDescriptors::GpsHeading,
            87 => FitFieldExdDescriptors::GpsElevation,
            88 => FitFieldExdDescriptors::AnaerobicTrainingEffect,
            89 => FitFieldExdDescriptors::Course,
            90 => FitFieldExdDescriptors::OffCourse,
            91 => FitFieldExdDescriptors::GlideRatio,
            92 => FitFieldExdDescriptors::VerticalDistance,
            93 => FitFieldExdDescriptors::Vmg,
            94 => FitFieldExdDescriptors::AmbientPressure,
            95 => FitFieldExdDescriptors::Pressure,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldExdDescriptors", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldActivityType { // fit base type: enum
    Generic = 0,
    Running = 1,
    Cycling = 2,
    Transition = 3,  // Mulitsport transition
    FitnessEquipment = 4,
    Swimming = 5,
    Walking = 6,
    Sedentary = 8,
    All = 254,  // All is for goals only to include all sports.
}

impl FitFieldActivityType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldActivityType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldActivityType::from(val), o))
    }
}

impl From<u8> for FitFieldActivityType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldActivityType::Generic,
            1 => FitFieldActivityType::Running,
            2 => FitFieldActivityType::Cycling,
            3 => FitFieldActivityType::Transition,
            4 => FitFieldActivityType::FitnessEquipment,
            5 => FitFieldActivityType::Swimming,
            6 => FitFieldActivityType::Walking,
            8 => FitFieldActivityType::Sedentary,
            254 => FitFieldActivityType::All,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldActivityType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAutolapTrigger { // fit base type: enum
    Time = 0,
    Distance = 1,
    PositionStart = 2,
    PositionLap = 3,
    PositionWaypoint = 4,
    PositionMarked = 5,
    Off = 6,
}

impl FitFieldAutolapTrigger {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAutolapTrigger, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldAutolapTrigger::from(val), o))
    }
}

impl From<u8> for FitFieldAutolapTrigger {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldAutolapTrigger::Time,
            1 => FitFieldAutolapTrigger::Distance,
            2 => FitFieldAutolapTrigger::PositionStart,
            3 => FitFieldAutolapTrigger::PositionLap,
            4 => FitFieldAutolapTrigger::PositionWaypoint,
            5 => FitFieldAutolapTrigger::PositionMarked,
            6 => FitFieldAutolapTrigger::Off,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAutolapTrigger", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldBacklightMode { // fit base type: enum
    Off = 0,
    Manual = 1,
    KeyAndMessages = 2,
    AutoBrightness = 3,
    SmartNotifications = 4,
    KeyAndMessagesNight = 5,
    KeyAndMessagesAndSmartNotifications = 6,
}

impl FitFieldBacklightMode {
    pub fn parse(input: &[u8]) -> Result<(FitFieldBacklightMode, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldBacklightMode::from(val), o))
    }
}

impl From<u8> for FitFieldBacklightMode {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldBacklightMode::Off,
            1 => FitFieldBacklightMode::Manual,
            2 => FitFieldBacklightMode::KeyAndMessages,
            3 => FitFieldBacklightMode::AutoBrightness,
            4 => FitFieldBacklightMode::SmartNotifications,
            5 => FitFieldBacklightMode::KeyAndMessagesNight,
            6 => FitFieldBacklightMode::KeyAndMessagesAndSmartNotifications,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldBacklightMode", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDisplayOrientation { // fit base type: enum
    Auto = 0,  // automatic if the device supports it
    Portrait = 1,
    Landscape = 2,
    PortraitFlipped = 3,  // portrait mode but rotated 180 degrees
    LandscapeFlipped = 4,  // landscape mode but rotated 180 degrees
}

impl FitFieldDisplayOrientation {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDisplayOrientation, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDisplayOrientation::from(val), o))
    }
}

impl From<u8> for FitFieldDisplayOrientation {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDisplayOrientation::Auto,
            1 => FitFieldDisplayOrientation::Portrait,
            2 => FitFieldDisplayOrientation::Landscape,
            3 => FitFieldDisplayOrientation::PortraitFlipped,
            4 => FitFieldDisplayOrientation::LandscapeFlipped,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDisplayOrientation", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLapTrigger { // fit base type: enum
    Manual = 0,
    Time = 1,
    Distance = 2,
    PositionStart = 3,
    PositionLap = 4,
    PositionWaypoint = 5,
    PositionMarked = 6,
    SessionEnd = 7,
    FitnessEquipment = 8,
}

impl FitFieldLapTrigger {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLapTrigger, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldLapTrigger::from(val), o))
    }
}

impl From<u8> for FitFieldLapTrigger {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldLapTrigger::Manual,
            1 => FitFieldLapTrigger::Time,
            2 => FitFieldLapTrigger::Distance,
            3 => FitFieldLapTrigger::PositionStart,
            4 => FitFieldLapTrigger::PositionLap,
            5 => FitFieldLapTrigger::PositionWaypoint,
            6 => FitFieldLapTrigger::PositionMarked,
            7 => FitFieldLapTrigger::SessionEnd,
            8 => FitFieldLapTrigger::FitnessEquipment,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLapTrigger", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldCourseCapabilities { // fit base type: uint32z
    Processed = 1,
    Valid = 2,
    Time = 4,
    Distance = 8,
    Position = 16,
    HeartRate = 32,
    Power = 64,
    Cadence = 128,
    Training = 256,
    Navigation = 512,
    Bikeway = 1024,
}

impl FitFieldCourseCapabilities {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldCourseCapabilities, &[u8])> {
        let (val, o) = parse_uint32z(input, endianness)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldCourseCapabilities::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u32> for FitFieldCourseCapabilities {
    fn from(code: u32) -> Self {
        match code {
            1 => FitFieldCourseCapabilities::Processed,
            2 => FitFieldCourseCapabilities::Valid,
            4 => FitFieldCourseCapabilities::Time,
            8 => FitFieldCourseCapabilities::Distance,
            16 => FitFieldCourseCapabilities::Position,
            32 => FitFieldCourseCapabilities::HeartRate,
            64 => FitFieldCourseCapabilities::Power,
            128 => FitFieldCourseCapabilities::Cadence,
            256 => FitFieldCourseCapabilities::Training,
            512 => FitFieldCourseCapabilities::Navigation,
            1024 => FitFieldCourseCapabilities::Bikeway,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldCourseCapabilities", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldTimeMode { // fit base type: enum
    Hour12 = 0,
    Hour24 = 1,  // Does not use a leading zero and has a colon
    Military = 2,  // Uses a leading zero and does not have a colon
    Hour12WithSeconds = 3,
    Hour24WithSeconds = 4,
    Utc = 5,
}

impl FitFieldTimeMode {
    pub fn parse(input: &[u8]) -> Result<(FitFieldTimeMode, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldTimeMode::from(val), o))
    }
}

impl From<u8> for FitFieldTimeMode {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldTimeMode::Hour12,
            1 => FitFieldTimeMode::Hour24,
            2 => FitFieldTimeMode::Military,
            3 => FitFieldTimeMode::Hour12WithSeconds,
            4 => FitFieldTimeMode::Hour24WithSeconds,
            5 => FitFieldTimeMode::Utc,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldTimeMode", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldExdQualifiers { // fit base type: enum
    NoQualifier = 0,
    Instantaneous = 1,
    Average = 2,
    Lap = 3,
    Maximum = 4,
    MaximumAverage = 5,
    MaximumLap = 6,
    LastLap = 7,
    AverageLap = 8,
    ToDestination = 9,
    ToGo = 10,
    ToNext = 11,
    NextCoursePoint = 12,
    Total = 13,
    ThreeSecondAverage = 14,
    TenSecondAverage = 15,
    ThirtySecondAverage = 16,
    PercentMaximum = 17,
    PercentMaximumAverage = 18,
    LapPercentMaximum = 19,
    Elapsed = 20,
    Sunrise = 21,
    Sunset = 22,
    ComparedToVirtualPartner = 23,
    Maximum24h = 24,
    Minimum24h = 25,
    Minimum = 26,
    First = 27,
    Second = 28,
    Third = 29,
    Shifter = 30,
    LastSport = 31,
    Moving = 32,
    Stopped = 33,
    Zone9 = 242,
    Zone8 = 243,
    Zone7 = 244,
    Zone6 = 245,
    Zone5 = 246,
    Zone4 = 247,
    Zone3 = 248,
    Zone2 = 249,
    Zone1 = 250,
}

impl FitFieldExdQualifiers {
    pub fn parse(input: &[u8]) -> Result<(FitFieldExdQualifiers, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldExdQualifiers::from(val), o))
    }
}

impl From<u8> for FitFieldExdQualifiers {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldExdQualifiers::NoQualifier,
            1 => FitFieldExdQualifiers::Instantaneous,
            2 => FitFieldExdQualifiers::Average,
            3 => FitFieldExdQualifiers::Lap,
            4 => FitFieldExdQualifiers::Maximum,
            5 => FitFieldExdQualifiers::MaximumAverage,
            6 => FitFieldExdQualifiers::MaximumLap,
            7 => FitFieldExdQualifiers::LastLap,
            8 => FitFieldExdQualifiers::AverageLap,
            9 => FitFieldExdQualifiers::ToDestination,
            10 => FitFieldExdQualifiers::ToGo,
            11 => FitFieldExdQualifiers::ToNext,
            12 => FitFieldExdQualifiers::NextCoursePoint,
            13 => FitFieldExdQualifiers::Total,
            14 => FitFieldExdQualifiers::ThreeSecondAverage,
            15 => FitFieldExdQualifiers::TenSecondAverage,
            16 => FitFieldExdQualifiers::ThirtySecondAverage,
            17 => FitFieldExdQualifiers::PercentMaximum,
            18 => FitFieldExdQualifiers::PercentMaximumAverage,
            19 => FitFieldExdQualifiers::LapPercentMaximum,
            20 => FitFieldExdQualifiers::Elapsed,
            21 => FitFieldExdQualifiers::Sunrise,
            22 => FitFieldExdQualifiers::Sunset,
            23 => FitFieldExdQualifiers::ComparedToVirtualPartner,
            24 => FitFieldExdQualifiers::Maximum24h,
            25 => FitFieldExdQualifiers::Minimum24h,
            26 => FitFieldExdQualifiers::Minimum,
            27 => FitFieldExdQualifiers::First,
            28 => FitFieldExdQualifiers::Second,
            29 => FitFieldExdQualifiers::Third,
            30 => FitFieldExdQualifiers::Shifter,
            31 => FitFieldExdQualifiers::LastSport,
            32 => FitFieldExdQualifiers::Moving,
            33 => FitFieldExdQualifiers::Stopped,
            242 => FitFieldExdQualifiers::Zone9,
            243 => FitFieldExdQualifiers::Zone8,
            244 => FitFieldExdQualifiers::Zone7,
            245 => FitFieldExdQualifiers::Zone6,
            246 => FitFieldExdQualifiers::Zone5,
            247 => FitFieldExdQualifiers::Zone4,
            248 => FitFieldExdQualifiers::Zone3,
            249 => FitFieldExdQualifiers::Zone2,
            250 => FitFieldExdQualifiers::Zone1,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldExdQualifiers", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldGoalSource { // fit base type: enum
    Auto = 0,  // Device generated
    Community = 1,  // Social network sourced goal
    User = 2,  // Manually generated
}

impl FitFieldGoalSource {
    pub fn parse(input: &[u8]) -> Result<(FitFieldGoalSource, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldGoalSource::from(val), o))
    }
}

impl From<u8> for FitFieldGoalSource {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldGoalSource::Auto,
            1 => FitFieldGoalSource::Community,
            2 => FitFieldGoalSource::User,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldGoalSource", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDateMode { // fit base type: enum
    DayMonth = 0,
    MonthDay = 1,
}

impl FitFieldDateMode {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDateMode, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDateMode::from(val), o))
    }
}

impl From<u8> for FitFieldDateMode {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDateMode::DayMonth,
            1 => FitFieldDateMode::MonthDay,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDateMode", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSubSport { // fit base type: enum
    Generic = 0,
    Treadmill = 1,  // Run/Fitness Equipment
    Street = 2,  // Run
    Trail = 3,  // Run
    Track = 4,  // Run
    Spin = 5,  // Cycling
    IndoorCycling = 6,  // Cycling/Fitness Equipment
    Road = 7,  // Cycling
    Mountain = 8,  // Cycling
    Downhill = 9,  // Cycling
    Recumbent = 10,  // Cycling
    Cyclocross = 11,  // Cycling
    HandCycling = 12,  // Cycling
    TrackCycling = 13,  // Cycling
    IndoorRowing = 14,  // Fitness Equipment
    Elliptical = 15,  // Fitness Equipment
    StairClimbing = 16,  // Fitness Equipment
    LapSwimming = 17,  // Swimming
    OpenWater = 18,  // Swimming
    FlexibilityTraining = 19,  // Training
    StrengthTraining = 20,  // Training
    WarmUp = 21,  // Tennis
    Match = 22,  // Tennis
    Exercise = 23,  // Tennis
    Challenge = 24,  // Tennis
    IndoorSkiing = 25,  // Fitness Equipment
    CardioTraining = 26,  // Training
    IndoorWalking = 27,  // Walking/Fitness Equipment
    EBikeFitness = 28,  // E-Biking
    Bmx = 29,  // Cycling
    CasualWalking = 30,  // Walking
    SpeedWalking = 31,  // Walking
    BikeToRunTransition = 32,  // Transition
    RunToBikeTransition = 33,  // Transition
    SwimToBikeTransition = 34,  // Transition
    Atv = 35,  // Motorcycling
    Motocross = 36,  // Motorcycling
    Backcountry = 37,  // Alpine Skiing/Snowboarding
    Resort = 38,  // Alpine Skiing/Snowboarding
    RcDrone = 39,  // Flying
    Wingsuit = 40,  // Flying
    Whitewater = 41,  // Kayaking/Rafting
    SkateSkiing = 42,  // Cross Country Skiing
    Yoga = 43,  // Training
    Pilates = 44,  // Training
    IndoorRunning = 45,  // Run
    GravelCycling = 46,  // Cycling
    EBikeMountain = 47,  // Cycling
    Commuting = 48,  // Cycling
    MixedSurface = 49,  // Cycling
    Navigate = 50,
    TrackMe = 51,
    Map = 52,
    All = 254,
}

impl FitFieldSubSport {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSubSport, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSubSport::from(val), o))
    }
}

impl From<u8> for FitFieldSubSport {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSubSport::Generic,
            1 => FitFieldSubSport::Treadmill,
            2 => FitFieldSubSport::Street,
            3 => FitFieldSubSport::Trail,
            4 => FitFieldSubSport::Track,
            5 => FitFieldSubSport::Spin,
            6 => FitFieldSubSport::IndoorCycling,
            7 => FitFieldSubSport::Road,
            8 => FitFieldSubSport::Mountain,
            9 => FitFieldSubSport::Downhill,
            10 => FitFieldSubSport::Recumbent,
            11 => FitFieldSubSport::Cyclocross,
            12 => FitFieldSubSport::HandCycling,
            13 => FitFieldSubSport::TrackCycling,
            14 => FitFieldSubSport::IndoorRowing,
            15 => FitFieldSubSport::Elliptical,
            16 => FitFieldSubSport::StairClimbing,
            17 => FitFieldSubSport::LapSwimming,
            18 => FitFieldSubSport::OpenWater,
            19 => FitFieldSubSport::FlexibilityTraining,
            20 => FitFieldSubSport::StrengthTraining,
            21 => FitFieldSubSport::WarmUp,
            22 => FitFieldSubSport::Match,
            23 => FitFieldSubSport::Exercise,
            24 => FitFieldSubSport::Challenge,
            25 => FitFieldSubSport::IndoorSkiing,
            26 => FitFieldSubSport::CardioTraining,
            27 => FitFieldSubSport::IndoorWalking,
            28 => FitFieldSubSport::EBikeFitness,
            29 => FitFieldSubSport::Bmx,
            30 => FitFieldSubSport::CasualWalking,
            31 => FitFieldSubSport::SpeedWalking,
            32 => FitFieldSubSport::BikeToRunTransition,
            33 => FitFieldSubSport::RunToBikeTransition,
            34 => FitFieldSubSport::SwimToBikeTransition,
            35 => FitFieldSubSport::Atv,
            36 => FitFieldSubSport::Motocross,
            37 => FitFieldSubSport::Backcountry,
            38 => FitFieldSubSport::Resort,
            39 => FitFieldSubSport::RcDrone,
            40 => FitFieldSubSport::Wingsuit,
            41 => FitFieldSubSport::Whitewater,
            42 => FitFieldSubSport::SkateSkiing,
            43 => FitFieldSubSport::Yoga,
            44 => FitFieldSubSport::Pilates,
            45 => FitFieldSubSport::IndoorRunning,
            46 => FitFieldSubSport::GravelCycling,
            47 => FitFieldSubSport::EBikeMountain,
            48 => FitFieldSubSport::Commuting,
            49 => FitFieldSubSport::MixedSurface,
            50 => FitFieldSubSport::Navigate,
            51 => FitFieldSubSport::TrackMe,
            52 => FitFieldSubSport::Map,
            254 => FitFieldSubSport::All,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSubSport", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDeviceIndex { // fit base type: uint8
    Creator = 0,  // Creator of the file is always device index 0.
}

impl FitFieldDeviceIndex {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDeviceIndex, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldDeviceIndex::from(val), o))
    }
}

impl From<u8> for FitFieldDeviceIndex {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDeviceIndex::Creator,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDeviceIndex", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits0 { // fit base type: uint8z
    Generic = 1,
    Running = 2,
    Cycling = 4,
    Transition = 8,  // Mulitsport transition
    FitnessEquipment = 16,
    Swimming = 32,
    Basketball = 64,
    Soccer = 128,
}

impl FitFieldSportBits0 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits0, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits0::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits0 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits0::Generic,
            2 => FitFieldSportBits0::Running,
            4 => FitFieldSportBits0::Cycling,
            8 => FitFieldSportBits0::Transition,
            16 => FitFieldSportBits0::FitnessEquipment,
            32 => FitFieldSportBits0::Swimming,
            64 => FitFieldSportBits0::Basketball,
            128 => FitFieldSportBits0::Soccer,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits0", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits1 { // fit base type: uint8z
    Tennis = 1,
    AmericanFootball = 2,
    Training = 4,
    Walking = 8,
    CrossCountrySkiing = 16,
    AlpineSkiing = 32,
    Snowboarding = 64,
    Rowing = 128,
}

impl FitFieldSportBits1 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits1, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits1::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits1 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits1::Tennis,
            2 => FitFieldSportBits1::AmericanFootball,
            4 => FitFieldSportBits1::Training,
            8 => FitFieldSportBits1::Walking,
            16 => FitFieldSportBits1::CrossCountrySkiing,
            32 => FitFieldSportBits1::AlpineSkiing,
            64 => FitFieldSportBits1::Snowboarding,
            128 => FitFieldSportBits1::Rowing,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits1", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits2 { // fit base type: uint8z
    Mountaineering = 1,
    Hiking = 2,
    Multisport = 4,
    Paddling = 8,
    Flying = 16,
    EBiking = 32,
    Motorcycling = 64,
    Boating = 128,
}

impl FitFieldSportBits2 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits2, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits2::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits2 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits2::Mountaineering,
            2 => FitFieldSportBits2::Hiking,
            4 => FitFieldSportBits2::Multisport,
            8 => FitFieldSportBits2::Paddling,
            16 => FitFieldSportBits2::Flying,
            32 => FitFieldSportBits2::EBiking,
            64 => FitFieldSportBits2::Motorcycling,
            128 => FitFieldSportBits2::Boating,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits2", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits3 { // fit base type: uint8z
    Driving = 1,
    Golf = 2,
    HangGliding = 4,
    HorsebackRiding = 8,
    Hunting = 16,
    Fishing = 32,
    InlineSkating = 64,
    RockClimbing = 128,
}

impl FitFieldSportBits3 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits3, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits3::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits3 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits3::Driving,
            2 => FitFieldSportBits3::Golf,
            4 => FitFieldSportBits3::HangGliding,
            8 => FitFieldSportBits3::HorsebackRiding,
            16 => FitFieldSportBits3::Hunting,
            32 => FitFieldSportBits3::Fishing,
            64 => FitFieldSportBits3::InlineSkating,
            128 => FitFieldSportBits3::RockClimbing,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits3", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits4 { // fit base type: uint8z
    Sailing = 1,
    IceSkating = 2,
    SkyDiving = 4,
    Snowshoeing = 8,
    Snowmobiling = 16,
    StandUpPaddleboarding = 32,
    Surfing = 64,
    Wakeboarding = 128,
}

impl FitFieldSportBits4 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits4, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits4::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits4 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits4::Sailing,
            2 => FitFieldSportBits4::IceSkating,
            4 => FitFieldSportBits4::SkyDiving,
            8 => FitFieldSportBits4::Snowshoeing,
            16 => FitFieldSportBits4::Snowmobiling,
            32 => FitFieldSportBits4::StandUpPaddleboarding,
            64 => FitFieldSportBits4::Surfing,
            128 => FitFieldSportBits4::Wakeboarding,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits4", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits5 { // fit base type: uint8z
    WaterSkiing = 1,
    Kayaking = 2,
    Rafting = 4,
    Windsurfing = 8,
    Kitesurfing = 16,
    Tactical = 32,
    Jumpmaster = 64,
    Boxing = 128,
}

impl FitFieldSportBits5 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits5, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits5::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits5 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits5::WaterSkiing,
            2 => FitFieldSportBits5::Kayaking,
            4 => FitFieldSportBits5::Rafting,
            8 => FitFieldSportBits5::Windsurfing,
            16 => FitFieldSportBits5::Kitesurfing,
            32 => FitFieldSportBits5::Tactical,
            64 => FitFieldSportBits5::Jumpmaster,
            128 => FitFieldSportBits5::Boxing,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits5", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSportBits6 { // fit base type: uint8z
    FloorClimbing = 1,
}

impl FitFieldSportBits6 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSportBits6, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSportBits6::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldSportBits6 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldSportBits6::FloorClimbing,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSportBits6", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSegmentDeleteStatus { // fit base type: enum
    DoNotDelete = 0,
    DeleteOne = 1,
    DeleteAll = 2,
}

impl FitFieldSegmentDeleteStatus {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSegmentDeleteStatus, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSegmentDeleteStatus::from(val), o))
    }
}

impl From<u8> for FitFieldSegmentDeleteStatus {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSegmentDeleteStatus::DoNotDelete,
            1 => FitFieldSegmentDeleteStatus::DeleteOne,
            2 => FitFieldSegmentDeleteStatus::DeleteAll,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSegmentDeleteStatus", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldFitnessEquipmentState { // fit base type: enum
    Ready = 0,
    InUse = 1,
    Paused = 2,
    Unknown = 3,  // lost connection to fitness equipment
}

impl FitFieldFitnessEquipmentState {
    pub fn parse(input: &[u8]) -> Result<(FitFieldFitnessEquipmentState, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldFitnessEquipmentState::from(val), o))
    }
}

impl From<u8> for FitFieldFitnessEquipmentState {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldFitnessEquipmentState::Ready,
            1 => FitFieldFitnessEquipmentState::InUse,
            2 => FitFieldFitnessEquipmentState::Paused,
            3 => FitFieldFitnessEquipmentState::Unknown,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldFitnessEquipmentState", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAutoActivityDetect { // fit base type: uint32
    None = 0,
    Running = 1,
    Cycling = 2,
    Swimming = 4,
    Walking = 8,
    Elliptical = 32,
    Sedentary = 1024,
}

impl FitFieldAutoActivityDetect {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldAutoActivityDetect, &[u8])> {
        let (val, o) = parse_uint32(input, endianness)?;
        Ok((FitFieldAutoActivityDetect::from(val), o))
    }
}

impl From<u32> for FitFieldAutoActivityDetect {
    fn from(code: u32) -> Self {
        match code {
            0 => FitFieldAutoActivityDetect::None,
            1 => FitFieldAutoActivityDetect::Running,
            2 => FitFieldAutoActivityDetect::Cycling,
            4 => FitFieldAutoActivityDetect::Swimming,
            8 => FitFieldAutoActivityDetect::Walking,
            32 => FitFieldAutoActivityDetect::Elliptical,
            1024 => FitFieldAutoActivityDetect::Sedentary,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAutoActivityDetect", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldRiderPositionType { // fit base type: enum
    Seated = 0,
    Standing = 1,
}

impl FitFieldRiderPositionType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldRiderPositionType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldRiderPositionType::from(val), o))
    }
}

impl From<u8> for FitFieldRiderPositionType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldRiderPositionType::Seated,
            1 => FitFieldRiderPositionType::Standing,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldRiderPositionType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldGarminProduct { // fit base type: uint16
    Hrm1 = 1,
    Axh01 = 2,  // AXH01 HRM chipset
    Axb01 = 3,
    Axb02 = 4,
    Hrm2ss = 5,
    DsiAlf02 = 6,
    Hrm3ss = 7,
    HrmRunSingleByteProductId = 8,  // hrm_run model for HRM ANT+ messaging
    Bsm = 9,  // BSM model for ANT+ messaging
    Bcm = 10,  // BCM model for ANT+ messaging
    Axs01 = 11,  // AXS01 HRM Bike Chipset model for ANT+ messaging
    HrmTriSingleByteProductId = 12,  // hrm_tri model for HRM ANT+ messaging
    Fr225SingleByteProductId = 14,  // fr225 model for HRM ANT+ messaging
    Fr301China = 473,
    Fr301Japan = 474,
    Fr301Korea = 475,
    Fr301Taiwan = 494,
    Fr405 = 717,  // Forerunner 405
    Fr50 = 782,  // Forerunner 50
    Fr405Japan = 987,
    Fr60 = 988,  // Forerunner 60
    DsiAlf01 = 1011,
    Fr310xt = 1018,  // Forerunner 310
    Edge500 = 1036,
    Fr110 = 1124,  // Forerunner 110
    Edge800 = 1169,
    Edge500Taiwan = 1199,
    Edge500Japan = 1213,
    Chirp = 1253,
    Fr110Japan = 1274,
    Edge200 = 1325,
    Fr910xt = 1328,
    Edge800Taiwan = 1333,
    Edge800Japan = 1334,
    Alf04 = 1341,
    Fr610 = 1345,
    Fr210Japan = 1360,
    VectorSs = 1380,
    VectorCp = 1381,
    Edge800China = 1386,
    Edge500China = 1387,
    Fr610Japan = 1410,
    Edge500Korea = 1422,
    Fr70 = 1436,
    Fr310xt4t = 1446,
    Amx = 1461,
    Fr10 = 1482,
    Edge800Korea = 1497,
    Swim = 1499,
    Fr910xtChina = 1537,
    Fenix = 1551,
    Edge200Taiwan = 1555,
    Edge510 = 1561,
    Edge810 = 1567,
    Tempe = 1570,
    Fr910xtJapan = 1600,
    Fr620 = 1623,
    Fr220 = 1632,
    Fr910xtKorea = 1664,
    Fr10Japan = 1688,
    Edge810Japan = 1721,
    VirbElite = 1735,
    EdgeTouring = 1736,  // Also Edge Touring Plus
    Edge510Japan = 1742,
    HrmTri = 1743,
    HrmRun = 1752,
    Fr920xt = 1765,
    Edge510Asia = 1821,
    Edge810China = 1822,
    Edge810Taiwan = 1823,
    Edge1000 = 1836,
    VivoFit = 1837,
    VirbRemote = 1853,
    VivoKi = 1885,
    Fr15 = 1903,
    VivoActive = 1907,
    Edge510Korea = 1918,
    Fr620Japan = 1928,
    Fr620China = 1929,
    Fr220Japan = 1930,
    Fr220China = 1931,
    ApproachS6 = 1936,
    VivoSmart = 1956,
    Fenix2 = 1967,
    Epix = 1988,
    Fenix3 = 2050,
    Edge1000Taiwan = 2052,
    Edge1000Japan = 2053,
    Fr15Japan = 2061,
    Edge520 = 2067,
    Edge1000China = 2070,
    Fr620Russia = 2072,
    Fr220Russia = 2073,
    VectorS = 2079,
    Edge1000Korea = 2100,
    Fr920xtTaiwan = 2130,
    Fr920xtChina = 2131,
    Fr920xtJapan = 2132,
    Virbx = 2134,
    VivoSmartApac = 2135,
    EtrexTouch = 2140,
    Edge25 = 2147,
    Fr25 = 2148,
    VivoFit2 = 2150,
    Fr225 = 2153,
    Fr630 = 2156,
    Fr230 = 2157,
    VivoActiveApac = 2160,
    Vector2 = 2161,
    Vector2s = 2162,
    Virbxe = 2172,
    Fr620Taiwan = 2173,
    Fr220Taiwan = 2174,
    Truswing = 2175,
    Fenix3China = 2188,
    Fenix3Twn = 2189,
    VariaHeadlight = 2192,
    VariaTaillightOld = 2193,
    EdgeExplore1000 = 2204,
    Fr225Asia = 2219,
    VariaRadarTaillight = 2225,
    VariaRadarDisplay = 2226,
    Edge20 = 2238,
    D2Bravo = 2262,
    ApproachS20 = 2266,
    VariaRemote = 2276,
    Hrm4Run = 2327,
    VivoActiveHr = 2337,
    VivoSmartGpsHr = 2347,
    VivoSmartHr = 2348,
    VivoMove = 2368,
    VariaVision = 2398,
    VivoFit3 = 2406,
    Fenix3Hr = 2413,
    IndexSmartScale = 2429,
    Fr235 = 2431,
    Oregon7xx = 2441,
    Rino7xx = 2444,
    Nautix = 2496,
    Edge820 = 2530,
    EdgeExplore820 = 2531,
    Sdm4 = 10007,  // SDM4 footpod
    EdgeRemote = 10014,
    TrainingCenter = 20119,
    ConnectiqSimulator = 65531,
    AndroidAntplusPlugin = 65532,
    Connect = 65534,  // Garmin Connect website
}

impl FitFieldGarminProduct {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldGarminProduct, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldGarminProduct::from(val), o))
    }
}

impl From<u16> for FitFieldGarminProduct {
    fn from(code: u16) -> Self {
        match code {
            1 => FitFieldGarminProduct::Hrm1,
            2 => FitFieldGarminProduct::Axh01,
            3 => FitFieldGarminProduct::Axb01,
            4 => FitFieldGarminProduct::Axb02,
            5 => FitFieldGarminProduct::Hrm2ss,
            6 => FitFieldGarminProduct::DsiAlf02,
            7 => FitFieldGarminProduct::Hrm3ss,
            8 => FitFieldGarminProduct::HrmRunSingleByteProductId,
            9 => FitFieldGarminProduct::Bsm,
            10 => FitFieldGarminProduct::Bcm,
            11 => FitFieldGarminProduct::Axs01,
            12 => FitFieldGarminProduct::HrmTriSingleByteProductId,
            14 => FitFieldGarminProduct::Fr225SingleByteProductId,
            473 => FitFieldGarminProduct::Fr301China,
            474 => FitFieldGarminProduct::Fr301Japan,
            475 => FitFieldGarminProduct::Fr301Korea,
            494 => FitFieldGarminProduct::Fr301Taiwan,
            717 => FitFieldGarminProduct::Fr405,
            782 => FitFieldGarminProduct::Fr50,
            987 => FitFieldGarminProduct::Fr405Japan,
            988 => FitFieldGarminProduct::Fr60,
            1011 => FitFieldGarminProduct::DsiAlf01,
            1018 => FitFieldGarminProduct::Fr310xt,
            1036 => FitFieldGarminProduct::Edge500,
            1124 => FitFieldGarminProduct::Fr110,
            1169 => FitFieldGarminProduct::Edge800,
            1199 => FitFieldGarminProduct::Edge500Taiwan,
            1213 => FitFieldGarminProduct::Edge500Japan,
            1253 => FitFieldGarminProduct::Chirp,
            1274 => FitFieldGarminProduct::Fr110Japan,
            1325 => FitFieldGarminProduct::Edge200,
            1328 => FitFieldGarminProduct::Fr910xt,
            1333 => FitFieldGarminProduct::Edge800Taiwan,
            1334 => FitFieldGarminProduct::Edge800Japan,
            1341 => FitFieldGarminProduct::Alf04,
            1345 => FitFieldGarminProduct::Fr610,
            1360 => FitFieldGarminProduct::Fr210Japan,
            1380 => FitFieldGarminProduct::VectorSs,
            1381 => FitFieldGarminProduct::VectorCp,
            1386 => FitFieldGarminProduct::Edge800China,
            1387 => FitFieldGarminProduct::Edge500China,
            1410 => FitFieldGarminProduct::Fr610Japan,
            1422 => FitFieldGarminProduct::Edge500Korea,
            1436 => FitFieldGarminProduct::Fr70,
            1446 => FitFieldGarminProduct::Fr310xt4t,
            1461 => FitFieldGarminProduct::Amx,
            1482 => FitFieldGarminProduct::Fr10,
            1497 => FitFieldGarminProduct::Edge800Korea,
            1499 => FitFieldGarminProduct::Swim,
            1537 => FitFieldGarminProduct::Fr910xtChina,
            1551 => FitFieldGarminProduct::Fenix,
            1555 => FitFieldGarminProduct::Edge200Taiwan,
            1561 => FitFieldGarminProduct::Edge510,
            1567 => FitFieldGarminProduct::Edge810,
            1570 => FitFieldGarminProduct::Tempe,
            1600 => FitFieldGarminProduct::Fr910xtJapan,
            1623 => FitFieldGarminProduct::Fr620,
            1632 => FitFieldGarminProduct::Fr220,
            1664 => FitFieldGarminProduct::Fr910xtKorea,
            1688 => FitFieldGarminProduct::Fr10Japan,
            1721 => FitFieldGarminProduct::Edge810Japan,
            1735 => FitFieldGarminProduct::VirbElite,
            1736 => FitFieldGarminProduct::EdgeTouring,
            1742 => FitFieldGarminProduct::Edge510Japan,
            1743 => FitFieldGarminProduct::HrmTri,
            1752 => FitFieldGarminProduct::HrmRun,
            1765 => FitFieldGarminProduct::Fr920xt,
            1821 => FitFieldGarminProduct::Edge510Asia,
            1822 => FitFieldGarminProduct::Edge810China,
            1823 => FitFieldGarminProduct::Edge810Taiwan,
            1836 => FitFieldGarminProduct::Edge1000,
            1837 => FitFieldGarminProduct::VivoFit,
            1853 => FitFieldGarminProduct::VirbRemote,
            1885 => FitFieldGarminProduct::VivoKi,
            1903 => FitFieldGarminProduct::Fr15,
            1907 => FitFieldGarminProduct::VivoActive,
            1918 => FitFieldGarminProduct::Edge510Korea,
            1928 => FitFieldGarminProduct::Fr620Japan,
            1929 => FitFieldGarminProduct::Fr620China,
            1930 => FitFieldGarminProduct::Fr220Japan,
            1931 => FitFieldGarminProduct::Fr220China,
            1936 => FitFieldGarminProduct::ApproachS6,
            1956 => FitFieldGarminProduct::VivoSmart,
            1967 => FitFieldGarminProduct::Fenix2,
            1988 => FitFieldGarminProduct::Epix,
            2050 => FitFieldGarminProduct::Fenix3,
            2052 => FitFieldGarminProduct::Edge1000Taiwan,
            2053 => FitFieldGarminProduct::Edge1000Japan,
            2061 => FitFieldGarminProduct::Fr15Japan,
            2067 => FitFieldGarminProduct::Edge520,
            2070 => FitFieldGarminProduct::Edge1000China,
            2072 => FitFieldGarminProduct::Fr620Russia,
            2073 => FitFieldGarminProduct::Fr220Russia,
            2079 => FitFieldGarminProduct::VectorS,
            2100 => FitFieldGarminProduct::Edge1000Korea,
            2130 => FitFieldGarminProduct::Fr920xtTaiwan,
            2131 => FitFieldGarminProduct::Fr920xtChina,
            2132 => FitFieldGarminProduct::Fr920xtJapan,
            2134 => FitFieldGarminProduct::Virbx,
            2135 => FitFieldGarminProduct::VivoSmartApac,
            2140 => FitFieldGarminProduct::EtrexTouch,
            2147 => FitFieldGarminProduct::Edge25,
            2148 => FitFieldGarminProduct::Fr25,
            2150 => FitFieldGarminProduct::VivoFit2,
            2153 => FitFieldGarminProduct::Fr225,
            2156 => FitFieldGarminProduct::Fr630,
            2157 => FitFieldGarminProduct::Fr230,
            2160 => FitFieldGarminProduct::VivoActiveApac,
            2161 => FitFieldGarminProduct::Vector2,
            2162 => FitFieldGarminProduct::Vector2s,
            2172 => FitFieldGarminProduct::Virbxe,
            2173 => FitFieldGarminProduct::Fr620Taiwan,
            2174 => FitFieldGarminProduct::Fr220Taiwan,
            2175 => FitFieldGarminProduct::Truswing,
            2188 => FitFieldGarminProduct::Fenix3China,
            2189 => FitFieldGarminProduct::Fenix3Twn,
            2192 => FitFieldGarminProduct::VariaHeadlight,
            2193 => FitFieldGarminProduct::VariaTaillightOld,
            2204 => FitFieldGarminProduct::EdgeExplore1000,
            2219 => FitFieldGarminProduct::Fr225Asia,
            2225 => FitFieldGarminProduct::VariaRadarTaillight,
            2226 => FitFieldGarminProduct::VariaRadarDisplay,
            2238 => FitFieldGarminProduct::Edge20,
            2262 => FitFieldGarminProduct::D2Bravo,
            2266 => FitFieldGarminProduct::ApproachS20,
            2276 => FitFieldGarminProduct::VariaRemote,
            2327 => FitFieldGarminProduct::Hrm4Run,
            2337 => FitFieldGarminProduct::VivoActiveHr,
            2347 => FitFieldGarminProduct::VivoSmartGpsHr,
            2348 => FitFieldGarminProduct::VivoSmartHr,
            2368 => FitFieldGarminProduct::VivoMove,
            2398 => FitFieldGarminProduct::VariaVision,
            2406 => FitFieldGarminProduct::VivoFit3,
            2413 => FitFieldGarminProduct::Fenix3Hr,
            2429 => FitFieldGarminProduct::IndexSmartScale,
            2431 => FitFieldGarminProduct::Fr235,
            2441 => FitFieldGarminProduct::Oregon7xx,
            2444 => FitFieldGarminProduct::Rino7xx,
            2496 => FitFieldGarminProduct::Nautix,
            2530 => FitFieldGarminProduct::Edge820,
            2531 => FitFieldGarminProduct::EdgeExplore820,
            10007 => FitFieldGarminProduct::Sdm4,
            10014 => FitFieldGarminProduct::EdgeRemote,
            20119 => FitFieldGarminProduct::TrainingCenter,
            65531 => FitFieldGarminProduct::ConnectiqSimulator,
            65532 => FitFieldGarminProduct::AndroidAntplusPlugin,
            65534 => FitFieldGarminProduct::Connect,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldGarminProduct", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldExdLayout { // fit base type: enum
    FullScreen = 0,
    HalfVertical = 1,
    HalfHorizontal = 2,
    HalfVerticalRightSplit = 3,
    HalfHorizontalBottomSplit = 4,
    FullQuarterSplit = 5,
    HalfVerticalLeftSplit = 6,
    HalfHorizontalTopSplit = 7,
}

impl FitFieldExdLayout {
    pub fn parse(input: &[u8]) -> Result<(FitFieldExdLayout, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldExdLayout::from(val), o))
    }
}

impl From<u8> for FitFieldExdLayout {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldExdLayout::FullScreen,
            1 => FitFieldExdLayout::HalfVertical,
            2 => FitFieldExdLayout::HalfHorizontal,
            3 => FitFieldExdLayout::HalfVerticalRightSplit,
            4 => FitFieldExdLayout::HalfHorizontalBottomSplit,
            5 => FitFieldExdLayout::FullQuarterSplit,
            6 => FitFieldExdLayout::HalfVerticalLeftSplit,
            7 => FitFieldExdLayout::HalfHorizontalTopSplit,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldExdLayout", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWorkoutPower { // fit base type: uint32
    WattsOffset = 1000,
}

impl FitFieldWorkoutPower {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldWorkoutPower, &[u8])> {
        let (val, o) = parse_uint32(input, endianness)?;
        Ok((FitFieldWorkoutPower::from(val), o))
    }
}

impl From<u32> for FitFieldWorkoutPower {
    fn from(code: u32) -> Self {
        match code {
            1000 => FitFieldWorkoutPower::WattsOffset,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWorkoutPower", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWeatherReport { // fit base type: enum
    Current = 0,
    HourlyForecast = 1,
    DailyForecast = 2,
}

impl FitFieldWeatherReport {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWeatherReport, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWeatherReport::from(val), o))
    }
}

impl From<u8> for FitFieldWeatherReport {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWeatherReport::Current,
            1 => FitFieldWeatherReport::HourlyForecast,
            2 => FitFieldWeatherReport::DailyForecast,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWeatherReport", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldActivitySubtype { // fit base type: enum
    Generic = 0,
    Treadmill = 1,  // Run
    Street = 2,  // Run
    Trail = 3,  // Run
    Track = 4,  // Run
    Spin = 5,  // Cycling
    IndoorCycling = 6,  // Cycling
    Road = 7,  // Cycling
    Mountain = 8,  // Cycling
    Downhill = 9,  // Cycling
    Recumbent = 10,  // Cycling
    Cyclocross = 11,  // Cycling
    HandCycling = 12,  // Cycling
    TrackCycling = 13,  // Cycling
    IndoorRowing = 14,  // Fitness Equipment
    Elliptical = 15,  // Fitness Equipment
    StairClimbing = 16,  // Fitness Equipment
    LapSwimming = 17,  // Swimming
    OpenWater = 18,  // Swimming
    All = 254,
}

impl FitFieldActivitySubtype {
    pub fn parse(input: &[u8]) -> Result<(FitFieldActivitySubtype, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldActivitySubtype::from(val), o))
    }
}

impl From<u8> for FitFieldActivitySubtype {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldActivitySubtype::Generic,
            1 => FitFieldActivitySubtype::Treadmill,
            2 => FitFieldActivitySubtype::Street,
            3 => FitFieldActivitySubtype::Trail,
            4 => FitFieldActivitySubtype::Track,
            5 => FitFieldActivitySubtype::Spin,
            6 => FitFieldActivitySubtype::IndoorCycling,
            7 => FitFieldActivitySubtype::Road,
            8 => FitFieldActivitySubtype::Mountain,
            9 => FitFieldActivitySubtype::Downhill,
            10 => FitFieldActivitySubtype::Recumbent,
            11 => FitFieldActivitySubtype::Cyclocross,
            12 => FitFieldActivitySubtype::HandCycling,
            13 => FitFieldActivitySubtype::TrackCycling,
            14 => FitFieldActivitySubtype::IndoorRowing,
            15 => FitFieldActivitySubtype::Elliptical,
            16 => FitFieldActivitySubtype::StairClimbing,
            17 => FitFieldActivitySubtype::LapSwimming,
            18 => FitFieldActivitySubtype::OpenWater,
            254 => FitFieldActivitySubtype::All,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldActivitySubtype", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDayOfWeek { // fit base type: enum
    Sunday = 0,
    Monday = 1,
    Tuesday = 2,
    Wednesday = 3,
    Thursday = 4,
    Friday = 5,
    Saturday = 6,
}

impl FitFieldDayOfWeek {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDayOfWeek, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDayOfWeek::from(val), o))
    }
}

impl From<u8> for FitFieldDayOfWeek {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDayOfWeek::Sunday,
            1 => FitFieldDayOfWeek::Monday,
            2 => FitFieldDayOfWeek::Tuesday,
            3 => FitFieldDayOfWeek::Wednesday,
            4 => FitFieldDayOfWeek::Thursday,
            5 => FitFieldDayOfWeek::Friday,
            6 => FitFieldDayOfWeek::Saturday,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDayOfWeek", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAntplusDeviceType { // fit base type: uint8
    Antfs = 1,
    BikePower = 11,
    EnvironmentSensorLegacy = 12,
    MultiSportSpeedDistance = 15,
    Control = 16,
    FitnessEquipment = 17,
    BloodPressure = 18,
    GeocacheNode = 19,
    LightElectricVehicle = 20,
    EnvSensor = 25,
    Racquet = 26,
    ControlHub = 27,
    MuscleOxygen = 31,
    BikeLightMain = 35,
    BikeLightShared = 36,
    Exd = 38,
    BikeRadar = 40,
    WeightScale = 119,
    HeartRate = 120,
    BikeSpeedCadence = 121,
    BikeCadence = 122,
    BikeSpeed = 123,
    StrideSpeedDistance = 124,
}

impl FitFieldAntplusDeviceType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAntplusDeviceType, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldAntplusDeviceType::from(val), o))
    }
}

impl From<u8> for FitFieldAntplusDeviceType {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldAntplusDeviceType::Antfs,
            11 => FitFieldAntplusDeviceType::BikePower,
            12 => FitFieldAntplusDeviceType::EnvironmentSensorLegacy,
            15 => FitFieldAntplusDeviceType::MultiSportSpeedDistance,
            16 => FitFieldAntplusDeviceType::Control,
            17 => FitFieldAntplusDeviceType::FitnessEquipment,
            18 => FitFieldAntplusDeviceType::BloodPressure,
            19 => FitFieldAntplusDeviceType::GeocacheNode,
            20 => FitFieldAntplusDeviceType::LightElectricVehicle,
            25 => FitFieldAntplusDeviceType::EnvSensor,
            26 => FitFieldAntplusDeviceType::Racquet,
            27 => FitFieldAntplusDeviceType::ControlHub,
            31 => FitFieldAntplusDeviceType::MuscleOxygen,
            35 => FitFieldAntplusDeviceType::BikeLightMain,
            36 => FitFieldAntplusDeviceType::BikeLightShared,
            38 => FitFieldAntplusDeviceType::Exd,
            40 => FitFieldAntplusDeviceType::BikeRadar,
            119 => FitFieldAntplusDeviceType::WeightScale,
            120 => FitFieldAntplusDeviceType::HeartRate,
            121 => FitFieldAntplusDeviceType::BikeSpeedCadence,
            122 => FitFieldAntplusDeviceType::BikeCadence,
            123 => FitFieldAntplusDeviceType::BikeSpeed,
            124 => FitFieldAntplusDeviceType::StrideSpeedDistance,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAntplusDeviceType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldTurnType { // fit base type: enum
    ArrivingIdx = 0,
    ArrivingLeftIdx = 1,
    ArrivingRightIdx = 2,
    ArrivingViaIdx = 3,
    ArrivingViaLeftIdx = 4,
    ArrivingViaRightIdx = 5,
    BearKeepLeftIdx = 6,
    BearKeepRightIdx = 7,
    ContinueIdx = 8,
    ExitLeftIdx = 9,
    ExitRightIdx = 10,
    FerryIdx = 11,
    Roundabout45Idx = 12,
    Roundabout90Idx = 13,
    Roundabout135Idx = 14,
    Roundabout180Idx = 15,
    Roundabout225Idx = 16,
    Roundabout270Idx = 17,
    Roundabout315Idx = 18,
    Roundabout360Idx = 19,
    RoundaboutNeg45Idx = 20,
    RoundaboutNeg90Idx = 21,
    RoundaboutNeg135Idx = 22,
    RoundaboutNeg180Idx = 23,
    RoundaboutNeg225Idx = 24,
    RoundaboutNeg270Idx = 25,
    RoundaboutNeg315Idx = 26,
    RoundaboutNeg360Idx = 27,
    RoundaboutGenericIdx = 28,
    RoundaboutNegGenericIdx = 29,
    SharpTurnLeftIdx = 30,
    SharpTurnRightIdx = 31,
    TurnLeftIdx = 32,
    TurnRightIdx = 33,
    UturnLeftIdx = 34,
    UturnRightIdx = 35,
    IconInvIdx = 36,
    IconIdxCnt = 37,
}

impl FitFieldTurnType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldTurnType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldTurnType::from(val), o))
    }
}

impl From<u8> for FitFieldTurnType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldTurnType::ArrivingIdx,
            1 => FitFieldTurnType::ArrivingLeftIdx,
            2 => FitFieldTurnType::ArrivingRightIdx,
            3 => FitFieldTurnType::ArrivingViaIdx,
            4 => FitFieldTurnType::ArrivingViaLeftIdx,
            5 => FitFieldTurnType::ArrivingViaRightIdx,
            6 => FitFieldTurnType::BearKeepLeftIdx,
            7 => FitFieldTurnType::BearKeepRightIdx,
            8 => FitFieldTurnType::ContinueIdx,
            9 => FitFieldTurnType::ExitLeftIdx,
            10 => FitFieldTurnType::ExitRightIdx,
            11 => FitFieldTurnType::FerryIdx,
            12 => FitFieldTurnType::Roundabout45Idx,
            13 => FitFieldTurnType::Roundabout90Idx,
            14 => FitFieldTurnType::Roundabout135Idx,
            15 => FitFieldTurnType::Roundabout180Idx,
            16 => FitFieldTurnType::Roundabout225Idx,
            17 => FitFieldTurnType::Roundabout270Idx,
            18 => FitFieldTurnType::Roundabout315Idx,
            19 => FitFieldTurnType::Roundabout360Idx,
            20 => FitFieldTurnType::RoundaboutNeg45Idx,
            21 => FitFieldTurnType::RoundaboutNeg90Idx,
            22 => FitFieldTurnType::RoundaboutNeg135Idx,
            23 => FitFieldTurnType::RoundaboutNeg180Idx,
            24 => FitFieldTurnType::RoundaboutNeg225Idx,
            25 => FitFieldTurnType::RoundaboutNeg270Idx,
            26 => FitFieldTurnType::RoundaboutNeg315Idx,
            27 => FitFieldTurnType::RoundaboutNeg360Idx,
            28 => FitFieldTurnType::RoundaboutGenericIdx,
            29 => FitFieldTurnType::RoundaboutNegGenericIdx,
            30 => FitFieldTurnType::SharpTurnLeftIdx,
            31 => FitFieldTurnType::SharpTurnRightIdx,
            32 => FitFieldTurnType::TurnLeftIdx,
            33 => FitFieldTurnType::TurnRightIdx,
            34 => FitFieldTurnType::UturnLeftIdx,
            35 => FitFieldTurnType::UturnRightIdx,
            36 => FitFieldTurnType::IconInvIdx,
            37 => FitFieldTurnType::IconIdxCnt,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldTurnType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldFitBaseUnit { // fit base type: uint16
    Other = 0,
}

impl FitFieldFitBaseUnit {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldFitBaseUnit, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldFitBaseUnit::from(val), o))
    }
}

impl From<u16> for FitFieldFitBaseUnit {
    fn from(code: u16) -> Self {
        match code {
            0 => FitFieldFitBaseUnit::Other,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldFitBaseUnit", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldBatteryStatus { // fit base type: uint8
    New = 1,
    Good = 2,
    Ok = 3,
    Low = 4,
    Critical = 5,
    Charging = 6,
    Unknown = 7,
}

impl FitFieldBatteryStatus {
    pub fn parse(input: &[u8]) -> Result<(FitFieldBatteryStatus, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldBatteryStatus::from(val), o))
    }
}

impl From<u8> for FitFieldBatteryStatus {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldBatteryStatus::New,
            2 => FitFieldBatteryStatus::Good,
            3 => FitFieldBatteryStatus::Ok,
            4 => FitFieldBatteryStatus::Low,
            5 => FitFieldBatteryStatus::Critical,
            6 => FitFieldBatteryStatus::Charging,
            7 => FitFieldBatteryStatus::Unknown,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldBatteryStatus", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldChecksum { // fit base type: uint8
    Clear = 0,  // Allows clear of checksum for flash memory where can only write 1 to 0 without erasing sector.
    Ok = 1,  // Set to mark checksum as valid if computes to invalid values 0 or 0xFF.  Checksum can also be set to ok to save encoding computation time.
}

impl FitFieldChecksum {
    pub fn parse(input: &[u8]) -> Result<(FitFieldChecksum, &[u8])> {
        let (val, o) = parse_uint8(input)?;
        Ok((FitFieldChecksum::from(val), o))
    }
}

impl From<u8> for FitFieldChecksum {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldChecksum::Clear,
            1 => FitFieldChecksum::Ok,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldChecksum", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWorkoutHr { // fit base type: uint32
    BpmOffset = 100,
}

impl FitFieldWorkoutHr {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldWorkoutHr, &[u8])> {
        let (val, o) = parse_uint32(input, endianness)?;
        Ok((FitFieldWorkoutHr::from(val), o))
    }
}

impl From<u32> for FitFieldWorkoutHr {
    fn from(code: u32) -> Self {
        match code {
            100 => FitFieldWorkoutHr::BpmOffset,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWorkoutHr", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLanguageBits3 { // fit base type: uint8z
    Bulgarian = 1,
    Romanian = 2,
    Chinese = 4,
    Japanese = 8,
    Korean = 16,
    Taiwanese = 32,
    Thai = 64,
    Hebrew = 128,
}

impl FitFieldLanguageBits3 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLanguageBits3, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldLanguageBits3::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldLanguageBits3 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldLanguageBits3::Bulgarian,
            2 => FitFieldLanguageBits3::Romanian,
            4 => FitFieldLanguageBits3::Chinese,
            8 => FitFieldLanguageBits3::Japanese,
            16 => FitFieldLanguageBits3::Korean,
            32 => FitFieldLanguageBits3::Taiwanese,
            64 => FitFieldLanguageBits3::Thai,
            128 => FitFieldLanguageBits3::Hebrew,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLanguageBits3", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLanguageBits2 { // fit base type: uint8z
    Slovenian = 1,
    Swedish = 2,
    Russian = 4,
    Turkish = 8,
    Latvian = 16,
    Ukrainian = 32,
    Arabic = 64,
    Farsi = 128,
}

impl FitFieldLanguageBits2 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLanguageBits2, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldLanguageBits2::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldLanguageBits2 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldLanguageBits2::Slovenian,
            2 => FitFieldLanguageBits2::Swedish,
            4 => FitFieldLanguageBits2::Russian,
            8 => FitFieldLanguageBits2::Turkish,
            16 => FitFieldLanguageBits2::Latvian,
            32 => FitFieldLanguageBits2::Ukrainian,
            64 => FitFieldLanguageBits2::Arabic,
            128 => FitFieldLanguageBits2::Farsi,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLanguageBits2", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLanguageBits1 { // fit base type: uint8z
    Dutch = 1,
    Finnish = 2,
    Greek = 4,
    Hungarian = 8,
    Norwegian = 16,
    Polish = 32,
    Portuguese = 64,
    Slovakian = 128,
}

impl FitFieldLanguageBits1 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLanguageBits1, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldLanguageBits1::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldLanguageBits1 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldLanguageBits1::Dutch,
            2 => FitFieldLanguageBits1::Finnish,
            4 => FitFieldLanguageBits1::Greek,
            8 => FitFieldLanguageBits1::Hungarian,
            16 => FitFieldLanguageBits1::Norwegian,
            32 => FitFieldLanguageBits1::Polish,
            64 => FitFieldLanguageBits1::Portuguese,
            128 => FitFieldLanguageBits1::Slovakian,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLanguageBits1", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLanguageBits0 { // fit base type: uint8z
    English = 1,
    French = 2,
    Italian = 4,
    German = 8,
    Spanish = 16,
    Croatian = 32,
    Czech = 64,
    Danish = 128,
}

impl FitFieldLanguageBits0 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLanguageBits0, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldLanguageBits0::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldLanguageBits0 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldLanguageBits0::English,
            2 => FitFieldLanguageBits0::French,
            4 => FitFieldLanguageBits0::Italian,
            8 => FitFieldLanguageBits0::German,
            16 => FitFieldLanguageBits0::Spanish,
            32 => FitFieldLanguageBits0::Croatian,
            64 => FitFieldLanguageBits0::Czech,
            128 => FitFieldLanguageBits0::Danish,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLanguageBits0", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldEventType { // fit base type: enum
    Start = 0,
    Stop = 1,
    ConsecutiveDepreciated = 2,
    Marker = 3,
    StopAll = 4,
    BeginDepreciated = 5,
    EndDepreciated = 6,
    EndAllDepreciated = 7,
    StopDisable = 8,
    StopDisableAll = 9,
}

impl FitFieldEventType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldEventType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldEventType::from(val), o))
    }
}

impl From<u8> for FitFieldEventType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldEventType::Start,
            1 => FitFieldEventType::Stop,
            2 => FitFieldEventType::ConsecutiveDepreciated,
            3 => FitFieldEventType::Marker,
            4 => FitFieldEventType::StopAll,
            5 => FitFieldEventType::BeginDepreciated,
            6 => FitFieldEventType::EndDepreciated,
            7 => FitFieldEventType::EndAllDepreciated,
            8 => FitFieldEventType::StopDisable,
            9 => FitFieldEventType::StopDisableAll,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldEventType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldLanguageBits4 { // fit base type: uint8z
    BrazilianPortuguese = 1,
    Indonesian = 2,
}

impl FitFieldLanguageBits4 {
    pub fn parse(input: &[u8]) -> Result<(FitFieldLanguageBits4, &[u8])> {
        let (val, o) = parse_uint8z(input)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldLanguageBits4::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u8> for FitFieldLanguageBits4 {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldLanguageBits4::BrazilianPortuguese,
            2 => FitFieldLanguageBits4::Indonesian,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldLanguageBits4", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldBodyLocation { // fit base type: enum
    LeftLeg = 0,
    LeftCalf = 1,
    LeftShin = 2,
    LeftHamstring = 3,
    LeftQuad = 4,
    LeftGlute = 5,
    RightLeg = 6,
    RightCalf = 7,
    RightShin = 8,
    RightHamstring = 9,
    RightQuad = 10,
    RightGlute = 11,
    TorsoBack = 12,
    LeftLowerBack = 13,
    LeftUpperBack = 14,
    RightLowerBack = 15,
    RightUpperBack = 16,
    TorsoFront = 17,
    LeftAbdomen = 18,
    LeftChest = 19,
    RightAbdomen = 20,
    RightChest = 21,
    LeftArm = 22,
    LeftShoulder = 23,
    LeftBicep = 24,
    LeftTricep = 25,
    LeftBrachioradialis = 26,  // Left anterior forearm
    LeftForearmExtensors = 27,  // Left posterior forearm
    RightArm = 28,
    RightShoulder = 29,
    RightBicep = 30,
    RightTricep = 31,
    RightBrachioradialis = 32,  // Right anterior forearm
    RightForearmExtensors = 33,  // Right posterior forearm
    Neck = 34,
    Throat = 35,
    WaistMidBack = 36,
    WaistFront = 37,
    WaistLeft = 38,
    WaistRight = 39,
}

impl FitFieldBodyLocation {
    pub fn parse(input: &[u8]) -> Result<(FitFieldBodyLocation, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldBodyLocation::from(val), o))
    }
}

impl From<u8> for FitFieldBodyLocation {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldBodyLocation::LeftLeg,
            1 => FitFieldBodyLocation::LeftCalf,
            2 => FitFieldBodyLocation::LeftShin,
            3 => FitFieldBodyLocation::LeftHamstring,
            4 => FitFieldBodyLocation::LeftQuad,
            5 => FitFieldBodyLocation::LeftGlute,
            6 => FitFieldBodyLocation::RightLeg,
            7 => FitFieldBodyLocation::RightCalf,
            8 => FitFieldBodyLocation::RightShin,
            9 => FitFieldBodyLocation::RightHamstring,
            10 => FitFieldBodyLocation::RightQuad,
            11 => FitFieldBodyLocation::RightGlute,
            12 => FitFieldBodyLocation::TorsoBack,
            13 => FitFieldBodyLocation::LeftLowerBack,
            14 => FitFieldBodyLocation::LeftUpperBack,
            15 => FitFieldBodyLocation::RightLowerBack,
            16 => FitFieldBodyLocation::RightUpperBack,
            17 => FitFieldBodyLocation::TorsoFront,
            18 => FitFieldBodyLocation::LeftAbdomen,
            19 => FitFieldBodyLocation::LeftChest,
            20 => FitFieldBodyLocation::RightAbdomen,
            21 => FitFieldBodyLocation::RightChest,
            22 => FitFieldBodyLocation::LeftArm,
            23 => FitFieldBodyLocation::LeftShoulder,
            24 => FitFieldBodyLocation::LeftBicep,
            25 => FitFieldBodyLocation::LeftTricep,
            26 => FitFieldBodyLocation::LeftBrachioradialis,
            27 => FitFieldBodyLocation::LeftForearmExtensors,
            28 => FitFieldBodyLocation::RightArm,
            29 => FitFieldBodyLocation::RightShoulder,
            30 => FitFieldBodyLocation::RightBicep,
            31 => FitFieldBodyLocation::RightTricep,
            32 => FitFieldBodyLocation::RightBrachioradialis,
            33 => FitFieldBodyLocation::RightForearmExtensors,
            34 => FitFieldBodyLocation::Neck,
            35 => FitFieldBodyLocation::Throat,
            36 => FitFieldBodyLocation::WaistMidBack,
            37 => FitFieldBodyLocation::WaistFront,
            38 => FitFieldBodyLocation::WaistLeft,
            39 => FitFieldBodyLocation::WaistRight,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldBodyLocation", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldFile { // fit base type: enum
    Device = 1,  // Read only, single file. Must be in root directory.
    Settings = 2,  // Read/write, single file. Directory=Settings
    Sport = 3,  // Read/write, multiple files, file number = sport type. Directory=Sports
    Activity = 4,  // Read/erase, multiple files. Directory=Activities
    Workout = 5,  // Read/write/erase, multiple files. Directory=Workouts
    Course = 6,  // Read/write/erase, multiple files. Directory=Courses
    Schedules = 7,  // Read/write, single file. Directory=Schedules
    Weight = 9,  // Read only, single file. Circular buffer. All message definitions at start of file. Directory=Weight
    Totals = 10,  // Read only, single file. Directory=Totals
    Goals = 11,  // Read/write, single file. Directory=Goals
    BloodPressure = 14,  // Read only. Directory=Blood Pressure
    MonitoringA = 15,  // Read only. Directory=Monitoring. File number=sub type.
    ActivitySummary = 20,  // Read/erase, multiple files. Directory=Activities
    MonitoringDaily = 28,
    MonitoringB = 32,  // Read only. Directory=Monitoring. File number=identifier
    Segment = 34,  // Read/write/erase. Multiple Files.  Directory=Segments
    SegmentList = 35,  // Read/write/erase. Single File.  Directory=Segments
    ExdConfiguration = 40,  // Read/write/erase. Single File. Directory=Settings
    MfgRangeMin = 247,  // 0xF7 - 0xFE reserved for manufacturer specific file types
    MfgRangeMax = 254,  // 0xF7 - 0xFE reserved for manufacturer specific file types
}

impl FitFieldFile {
    pub fn parse(input: &[u8]) -> Result<(FitFieldFile, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldFile::from(val), o))
    }
}

impl From<u8> for FitFieldFile {
    fn from(code: u8) -> Self {
        match code {
            1 => FitFieldFile::Device,
            2 => FitFieldFile::Settings,
            3 => FitFieldFile::Sport,
            4 => FitFieldFile::Activity,
            5 => FitFieldFile::Workout,
            6 => FitFieldFile::Course,
            7 => FitFieldFile::Schedules,
            9 => FitFieldFile::Weight,
            10 => FitFieldFile::Totals,
            11 => FitFieldFile::Goals,
            14 => FitFieldFile::BloodPressure,
            15 => FitFieldFile::MonitoringA,
            20 => FitFieldFile::ActivitySummary,
            28 => FitFieldFile::MonitoringDaily,
            32 => FitFieldFile::MonitoringB,
            34 => FitFieldFile::Segment,
            35 => FitFieldFile::SegmentList,
            40 => FitFieldFile::ExdConfiguration,
            247 => FitFieldFile::MfgRangeMin,
            254 => FitFieldFile::MfgRangeMax,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldFile", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDisplayMeasure { // fit base type: enum
    Metric = 0,
    Statute = 1,
    Nautical = 2,
}

impl FitFieldDisplayMeasure {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDisplayMeasure, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDisplayMeasure::from(val), o))
    }
}

impl From<u8> for FitFieldDisplayMeasure {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDisplayMeasure::Metric,
            1 => FitFieldDisplayMeasure::Statute,
            2 => FitFieldDisplayMeasure::Nautical,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDisplayMeasure", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldCameraEventType { // fit base type: enum
    VideoStart = 0,  // Start of video recording
    VideoSplit = 1,  // Mark of video file split (end of one file, beginning of the other)
    VideoEnd = 2,  // End of video recording
    PhotoTaken = 3,  // Still photo taken
    VideoSecondStreamStart = 4,
    VideoSecondStreamSplit = 5,
    VideoSecondStreamEnd = 6,
    VideoSplitStart = 7,  // Mark of video file split start
    VideoSecondStreamSplitStart = 8,
    VideoPause = 11,  // Mark when a video recording has been paused
    VideoSecondStreamPause = 12,
    VideoResume = 13,  // Mark when a video recording has been resumed
    VideoSecondStreamResume = 14,
}

impl FitFieldCameraEventType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldCameraEventType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldCameraEventType::from(val), o))
    }
}

impl From<u8> for FitFieldCameraEventType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldCameraEventType::VideoStart,
            1 => FitFieldCameraEventType::VideoSplit,
            2 => FitFieldCameraEventType::VideoEnd,
            3 => FitFieldCameraEventType::PhotoTaken,
            4 => FitFieldCameraEventType::VideoSecondStreamStart,
            5 => FitFieldCameraEventType::VideoSecondStreamSplit,
            6 => FitFieldCameraEventType::VideoSecondStreamEnd,
            7 => FitFieldCameraEventType::VideoSplitStart,
            8 => FitFieldCameraEventType::VideoSecondStreamSplitStart,
            11 => FitFieldCameraEventType::VideoPause,
            12 => FitFieldCameraEventType::VideoSecondStreamPause,
            13 => FitFieldCameraEventType::VideoResume,
            14 => FitFieldCameraEventType::VideoSecondStreamResume,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldCameraEventType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAutoSyncFrequency { // fit base type: enum
    Never = 0,
    Occasionally = 1,
    Frequent = 2,
    OnceADay = 3,
}

impl FitFieldAutoSyncFrequency {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAutoSyncFrequency, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldAutoSyncFrequency::from(val), o))
    }
}

impl From<u8> for FitFieldAutoSyncFrequency {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldAutoSyncFrequency::Never,
            1 => FitFieldAutoSyncFrequency::Occasionally,
            2 => FitFieldAutoSyncFrequency::Frequent,
            3 => FitFieldAutoSyncFrequency::OnceADay,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAutoSyncFrequency", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldMessageIndex { // fit base type: uint16
    Selected = 32768,  // message is selected if set
    Reserved = 28672,  // reserved (default 0)
    Mask = 4095,  // index
}

impl FitFieldMessageIndex {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldMessageIndex, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldMessageIndex::from(val), o))
    }
}

impl From<u16> for FitFieldMessageIndex {
    fn from(code: u16) -> Self {
        match code {
            32768 => FitFieldMessageIndex::Selected,
            28672 => FitFieldMessageIndex::Reserved,
            4095 => FitFieldMessageIndex::Mask,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldMessageIndex", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldGoal { // fit base type: enum
    Time = 0,
    Distance = 1,
    Calories = 2,
    Frequency = 3,
    Steps = 4,
    Ascent = 5,
    ActiveMinutes = 6,
}

impl FitFieldGoal {
    pub fn parse(input: &[u8]) -> Result<(FitFieldGoal, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldGoal::from(val), o))
    }
}

impl From<u8> for FitFieldGoal {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldGoal::Time,
            1 => FitFieldGoal::Distance,
            2 => FitFieldGoal::Calories,
            3 => FitFieldGoal::Frequency,
            4 => FitFieldGoal::Steps,
            5 => FitFieldGoal::Ascent,
            6 => FitFieldGoal::ActiveMinutes,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldGoal", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldActivityLevel { // fit base type: enum
    Low = 0,
    Medium = 1,
    High = 2,
}

impl FitFieldActivityLevel {
    pub fn parse(input: &[u8]) -> Result<(FitFieldActivityLevel, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldActivityLevel::from(val), o))
    }
}

impl From<u8> for FitFieldActivityLevel {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldActivityLevel::Low,
            1 => FitFieldActivityLevel::Medium,
            2 => FitFieldActivityLevel::High,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldActivityLevel", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWktStepDuration { // fit base type: enum
    Time = 0,
    Distance = 1,
    HrLessThan = 2,
    HrGreaterThan = 3,
    Calories = 4,
    Open = 5,
    RepeatUntilStepsCmplt = 6,
    RepeatUntilTime = 7,
    RepeatUntilDistance = 8,
    RepeatUntilCalories = 9,
    RepeatUntilHrLessThan = 10,
    RepeatUntilHrGreaterThan = 11,
    RepeatUntilPowerLessThan = 12,
    RepeatUntilPowerGreaterThan = 13,
    PowerLessThan = 14,
    PowerGreaterThan = 15,
    RepetitionTime = 28,
}

impl FitFieldWktStepDuration {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWktStepDuration, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWktStepDuration::from(val), o))
    }
}

impl From<u8> for FitFieldWktStepDuration {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWktStepDuration::Time,
            1 => FitFieldWktStepDuration::Distance,
            2 => FitFieldWktStepDuration::HrLessThan,
            3 => FitFieldWktStepDuration::HrGreaterThan,
            4 => FitFieldWktStepDuration::Calories,
            5 => FitFieldWktStepDuration::Open,
            6 => FitFieldWktStepDuration::RepeatUntilStepsCmplt,
            7 => FitFieldWktStepDuration::RepeatUntilTime,
            8 => FitFieldWktStepDuration::RepeatUntilDistance,
            9 => FitFieldWktStepDuration::RepeatUntilCalories,
            10 => FitFieldWktStepDuration::RepeatUntilHrLessThan,
            11 => FitFieldWktStepDuration::RepeatUntilHrGreaterThan,
            12 => FitFieldWktStepDuration::RepeatUntilPowerLessThan,
            13 => FitFieldWktStepDuration::RepeatUntilPowerGreaterThan,
            14 => FitFieldWktStepDuration::PowerLessThan,
            15 => FitFieldWktStepDuration::PowerGreaterThan,
            28 => FitFieldWktStepDuration::RepetitionTime,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWktStepDuration", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWeatherSeverity { // fit base type: enum
    Unknown = 0,
    Warning = 1,
    Watch = 2,
    Advisory = 3,
    Statement = 4,
}

impl FitFieldWeatherSeverity {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWeatherSeverity, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWeatherSeverity::from(val), o))
    }
}

impl From<u8> for FitFieldWeatherSeverity {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWeatherSeverity::Unknown,
            1 => FitFieldWeatherSeverity::Warning,
            2 => FitFieldWeatherSeverity::Watch,
            3 => FitFieldWeatherSeverity::Advisory,
            4 => FitFieldWeatherSeverity::Statement,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWeatherSeverity", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldCameraOrientationType { // fit base type: enum
    CameraOrientation0 = 0,
    CameraOrientation90 = 1,
    CameraOrientation180 = 2,
    CameraOrientation270 = 3,
}

impl FitFieldCameraOrientationType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldCameraOrientationType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldCameraOrientationType::from(val), o))
    }
}

impl From<u8> for FitFieldCameraOrientationType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldCameraOrientationType::CameraOrientation0,
            1 => FitFieldCameraOrientationType::CameraOrientation90,
            2 => FitFieldCameraOrientationType::CameraOrientation180,
            3 => FitFieldCameraOrientationType::CameraOrientation270,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldCameraOrientationType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldWeatherStatus { // fit base type: enum
    Clear = 0,
    PartlyCloudy = 1,
    MostlyCloudy = 2,
    Rain = 3,
    Snow = 4,
    Windy = 5,
    Thunderstorms = 6,
    WintryMix = 7,
    Fog = 8,
    Hazy = 11,
    Hail = 12,
    ScatteredShowers = 13,
    ScatteredThunderstorms = 14,
    UnknownPrecipitation = 15,
    LightRain = 16,
    HeavyRain = 17,
    LightSnow = 18,
    HeavySnow = 19,
    LightRainSnow = 20,
    HeavyRainSnow = 21,
    Cloudy = 22,
}

impl FitFieldWeatherStatus {
    pub fn parse(input: &[u8]) -> Result<(FitFieldWeatherStatus, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldWeatherStatus::from(val), o))
    }
}

impl From<u8> for FitFieldWeatherStatus {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldWeatherStatus::Clear,
            1 => FitFieldWeatherStatus::PartlyCloudy,
            2 => FitFieldWeatherStatus::MostlyCloudy,
            3 => FitFieldWeatherStatus::Rain,
            4 => FitFieldWeatherStatus::Snow,
            5 => FitFieldWeatherStatus::Windy,
            6 => FitFieldWeatherStatus::Thunderstorms,
            7 => FitFieldWeatherStatus::WintryMix,
            8 => FitFieldWeatherStatus::Fog,
            11 => FitFieldWeatherStatus::Hazy,
            12 => FitFieldWeatherStatus::Hail,
            13 => FitFieldWeatherStatus::ScatteredShowers,
            14 => FitFieldWeatherStatus::ScatteredThunderstorms,
            15 => FitFieldWeatherStatus::UnknownPrecipitation,
            16 => FitFieldWeatherStatus::LightRain,
            17 => FitFieldWeatherStatus::HeavyRain,
            18 => FitFieldWeatherStatus::LightSnow,
            19 => FitFieldWeatherStatus::HeavySnow,
            20 => FitFieldWeatherStatus::LightRainSnow,
            21 => FitFieldWeatherStatus::HeavyRainSnow,
            22 => FitFieldWeatherStatus::Cloudy,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldWeatherStatus", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldExdDisplayType { // fit base type: enum
    Numerical = 0,
    Simple = 1,
    Graph = 2,
    Bar = 3,
    CircleGraph = 4,
    VirtualPartner = 5,
    Balance = 6,
    StringList = 7,
    String = 8,
    SimpleDynamicIcon = 9,
    Gauge = 10,
}

impl FitFieldExdDisplayType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldExdDisplayType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldExdDisplayType::from(val), o))
    }
}

impl From<u8> for FitFieldExdDisplayType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldExdDisplayType::Numerical,
            1 => FitFieldExdDisplayType::Simple,
            2 => FitFieldExdDisplayType::Graph,
            3 => FitFieldExdDisplayType::Bar,
            4 => FitFieldExdDisplayType::CircleGraph,
            5 => FitFieldExdDisplayType::VirtualPartner,
            6 => FitFieldExdDisplayType::Balance,
            7 => FitFieldExdDisplayType::StringList,
            8 => FitFieldExdDisplayType::String,
            9 => FitFieldExdDisplayType::SimpleDynamicIcon,
            10 => FitFieldExdDisplayType::Gauge,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldExdDisplayType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSourceType { // fit base type: enum
    Ant = 0,  // External device connected with ANT
    Antplus = 1,  // External device connected with ANT+
    Bluetooth = 2,  // External device connected with BT
    BluetoothLowEnergy = 3,  // External device connected with BLE
    Wifi = 4,  // External device connected with Wifi
    Local = 5,  // Onboard device
}

impl FitFieldSourceType {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSourceType, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSourceType::from(val), o))
    }
}

impl From<u8> for FitFieldSourceType {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSourceType::Ant,
            1 => FitFieldSourceType::Antplus,
            2 => FitFieldSourceType::Bluetooth,
            3 => FitFieldSourceType::BluetoothLowEnergy,
            4 => FitFieldSourceType::Wifi,
            5 => FitFieldSourceType::Local,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSourceType", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldDisplayHeart { // fit base type: enum
    Bpm = 0,
    Max = 1,
    Reserve = 2,
}

impl FitFieldDisplayHeart {
    pub fn parse(input: &[u8]) -> Result<(FitFieldDisplayHeart, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldDisplayHeart::from(val), o))
    }
}

impl From<u8> for FitFieldDisplayHeart {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldDisplayHeart::Bpm,
            1 => FitFieldDisplayHeart::Max,
            2 => FitFieldDisplayHeart::Reserve,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldDisplayHeart", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldTimeIntoDay { // fit base type: uint32
}

impl FitFieldTimeIntoDay {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldTimeIntoDay, &[u8])> {
        let (val, o) = parse_uint32(input, endianness)?;
        Ok((FitFieldTimeIntoDay::from(val), o))
    }
}

impl From<u32> for FitFieldTimeIntoDay {
    fn from(code: u32) -> Self {
        match code {
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldTimeIntoDay", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSwimStroke { // fit base type: enum
    Freestyle = 0,
    Backstroke = 1,
    Breaststroke = 2,
    Butterfly = 3,
    Drill = 4,
    Mixed = 5,
    Im = 6,  // IM is a mixed interval containing the same number of lengths for each of: Butterfly, Backstroke, Breaststroke, Freestyle, swam in that order.
}

impl FitFieldSwimStroke {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSwimStroke, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSwimStroke::from(val), o))
    }
}

impl From<u8> for FitFieldSwimStroke {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSwimStroke::Freestyle,
            1 => FitFieldSwimStroke::Backstroke,
            2 => FitFieldSwimStroke::Breaststroke,
            3 => FitFieldSwimStroke::Butterfly,
            4 => FitFieldSwimStroke::Drill,
            5 => FitFieldSwimStroke::Mixed,
            6 => FitFieldSwimStroke::Im,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSwimStroke", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSupportedExdScreenLayouts { // fit base type: uint32z
    FullScreen = 1,
    HalfVertical = 2,
    HalfHorizontal = 4,
    HalfVerticalRightSplit = 8,
    HalfHorizontalBottomSplit = 16,
    FullQuarterSplit = 32,
    HalfVerticalLeftSplit = 64,
    HalfHorizontalTopSplit = 128,
}

impl FitFieldSupportedExdScreenLayouts {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldSupportedExdScreenLayouts, &[u8])> {
        let (val, o) = parse_uint32z(input, endianness)?;
        match val {
            Some(nonzero_val) => Ok((FitFieldSupportedExdScreenLayouts::from(nonzero_val), o)),
            None => Err(Error::parse_zero())
        }
        
    }
}

impl From<u32> for FitFieldSupportedExdScreenLayouts {
    fn from(code: u32) -> Self {
        match code {
            1 => FitFieldSupportedExdScreenLayouts::FullScreen,
            2 => FitFieldSupportedExdScreenLayouts::HalfVertical,
            4 => FitFieldSupportedExdScreenLayouts::HalfHorizontal,
            8 => FitFieldSupportedExdScreenLayouts::HalfVerticalRightSplit,
            16 => FitFieldSupportedExdScreenLayouts::HalfHorizontalBottomSplit,
            32 => FitFieldSupportedExdScreenLayouts::FullQuarterSplit,
            64 => FitFieldSupportedExdScreenLayouts::HalfVerticalLeftSplit,
            128 => FitFieldSupportedExdScreenLayouts::HalfHorizontalTopSplit,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSupportedExdScreenLayouts", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldSwitch { // fit base type: enum
    Off = 0,
    On = 1,
    Auto = 2,
}

impl FitFieldSwitch {
    pub fn parse(input: &[u8]) -> Result<(FitFieldSwitch, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldSwitch::from(val), o))
    }
}

impl From<u8> for FitFieldSwitch {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldSwitch::Off,
            1 => FitFieldSwitch::On,
            2 => FitFieldSwitch::Auto,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldSwitch", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldMesgNum { // fit base type: uint16
    FileId = 0,
    Capabilities = 1,
    DeviceSettings = 2,
    UserProfile = 3,
    HrmProfile = 4,
    SdmProfile = 5,
    BikeProfile = 6,
    ZonesTarget = 7,
    HrZone = 8,
    PowerZone = 9,
    MetZone = 10,
    Sport = 12,
    Goal = 15,
    Session = 18,
    Lap = 19,
    Record = 20,
    Event = 21,
    DeviceInfo = 23,
    Workout = 26,
    WorkoutStep = 27,
    Schedule = 28,
    WeightScale = 30,
    Course = 31,
    CoursePoint = 32,
    Totals = 33,
    Activity = 34,
    Software = 35,
    FileCapabilities = 37,
    MesgCapabilities = 38,
    FieldCapabilities = 39,
    FileCreator = 49,
    BloodPressure = 51,
    SpeedZone = 53,
    Monitoring = 55,
    TrainingFile = 72,
    Hrv = 78,
    AntRx = 80,
    AntTx = 81,
    AntChannelId = 82,
    Length = 101,
    MonitoringInfo = 103,
    SlaveDevice = 106,
    Connectivity = 127,
    WeatherConditions = 128,
    WeatherAlert = 129,
    CadenceZone = 131,
    Hr = 132,
    SegmentLap = 142,
    MemoGlob = 145,
    SegmentId = 148,
    SegmentLeaderboardEntry = 149,
    SegmentPoint = 150,
    SegmentFile = 151,
    WatchfaceSettings = 159,
    GpsMetadata = 160,
    CameraEvent = 161,
    TimestampCorrelation = 162,
    GyroscopeData = 164,
    AccelerometerData = 165,
    ThreeDSensorCalibration = 167,
    VideoFrame = 169,
    ObdiiData = 174,
    NmeaSentence = 177,
    AviationAttitude = 178,
    Video = 184,
    VideoTitle = 185,
    VideoDescription = 186,
    VideoClip = 187,
    OhrSettings = 188,
    ExdScreenConfiguration = 200,
    ExdDataFieldConfiguration = 201,
    ExdDataConceptConfiguration = 202,
    FieldDescription = 206,
    DeveloperDataId = 207,
    MagnetometerData = 208,
    MfgRangeMin = 65280,  // 0xFF00 - 0xFFFE reserved for manufacturer specific messages
    MfgRangeMax = 65534,  // 0xFF00 - 0xFFFE reserved for manufacturer specific messages
}

impl FitFieldMesgNum {
    pub fn parse(input: &[u8], endianness: Endianness) -> Result<(FitFieldMesgNum, &[u8])> {
        let (val, o) = parse_uint16(input, endianness)?;
        Ok((FitFieldMesgNum::from(val), o))
    }
}

impl From<u16> for FitFieldMesgNum {
    fn from(code: u16) -> Self {
        match code {
            0 => FitFieldMesgNum::FileId,
            1 => FitFieldMesgNum::Capabilities,
            2 => FitFieldMesgNum::DeviceSettings,
            3 => FitFieldMesgNum::UserProfile,
            4 => FitFieldMesgNum::HrmProfile,
            5 => FitFieldMesgNum::SdmProfile,
            6 => FitFieldMesgNum::BikeProfile,
            7 => FitFieldMesgNum::ZonesTarget,
            8 => FitFieldMesgNum::HrZone,
            9 => FitFieldMesgNum::PowerZone,
            10 => FitFieldMesgNum::MetZone,
            12 => FitFieldMesgNum::Sport,
            15 => FitFieldMesgNum::Goal,
            18 => FitFieldMesgNum::Session,
            19 => FitFieldMesgNum::Lap,
            20 => FitFieldMesgNum::Record,
            21 => FitFieldMesgNum::Event,
            23 => FitFieldMesgNum::DeviceInfo,
            26 => FitFieldMesgNum::Workout,
            27 => FitFieldMesgNum::WorkoutStep,
            28 => FitFieldMesgNum::Schedule,
            30 => FitFieldMesgNum::WeightScale,
            31 => FitFieldMesgNum::Course,
            32 => FitFieldMesgNum::CoursePoint,
            33 => FitFieldMesgNum::Totals,
            34 => FitFieldMesgNum::Activity,
            35 => FitFieldMesgNum::Software,
            37 => FitFieldMesgNum::FileCapabilities,
            38 => FitFieldMesgNum::MesgCapabilities,
            39 => FitFieldMesgNum::FieldCapabilities,
            49 => FitFieldMesgNum::FileCreator,
            51 => FitFieldMesgNum::BloodPressure,
            53 => FitFieldMesgNum::SpeedZone,
            55 => FitFieldMesgNum::Monitoring,
            72 => FitFieldMesgNum::TrainingFile,
            78 => FitFieldMesgNum::Hrv,
            80 => FitFieldMesgNum::AntRx,
            81 => FitFieldMesgNum::AntTx,
            82 => FitFieldMesgNum::AntChannelId,
            101 => FitFieldMesgNum::Length,
            103 => FitFieldMesgNum::MonitoringInfo,
            106 => FitFieldMesgNum::SlaveDevice,
            127 => FitFieldMesgNum::Connectivity,
            128 => FitFieldMesgNum::WeatherConditions,
            129 => FitFieldMesgNum::WeatherAlert,
            131 => FitFieldMesgNum::CadenceZone,
            132 => FitFieldMesgNum::Hr,
            142 => FitFieldMesgNum::SegmentLap,
            145 => FitFieldMesgNum::MemoGlob,
            148 => FitFieldMesgNum::SegmentId,
            149 => FitFieldMesgNum::SegmentLeaderboardEntry,
            150 => FitFieldMesgNum::SegmentPoint,
            151 => FitFieldMesgNum::SegmentFile,
            159 => FitFieldMesgNum::WatchfaceSettings,
            160 => FitFieldMesgNum::GpsMetadata,
            161 => FitFieldMesgNum::CameraEvent,
            162 => FitFieldMesgNum::TimestampCorrelation,
            164 => FitFieldMesgNum::GyroscopeData,
            165 => FitFieldMesgNum::AccelerometerData,
            167 => FitFieldMesgNum::ThreeDSensorCalibration,
            169 => FitFieldMesgNum::VideoFrame,
            174 => FitFieldMesgNum::ObdiiData,
            177 => FitFieldMesgNum::NmeaSentence,
            178 => FitFieldMesgNum::AviationAttitude,
            184 => FitFieldMesgNum::Video,
            185 => FitFieldMesgNum::VideoTitle,
            186 => FitFieldMesgNum::VideoDescription,
            187 => FitFieldMesgNum::VideoClip,
            188 => FitFieldMesgNum::OhrSettings,
            200 => FitFieldMesgNum::ExdScreenConfiguration,
            201 => FitFieldMesgNum::ExdDataFieldConfiguration,
            202 => FitFieldMesgNum::ExdDataConceptConfiguration,
            206 => FitFieldMesgNum::FieldDescription,
            207 => FitFieldMesgNum::DeveloperDataId,
            208 => FitFieldMesgNum::MagnetometerData,
            65280 => FitFieldMesgNum::MfgRangeMin,
            65534 => FitFieldMesgNum::MfgRangeMax,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldMesgNum", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldAntNetwork { // fit base type: enum
    Public = 0,
    Antplus = 1,
    Antfs = 2,
    Private = 3,
}

impl FitFieldAntNetwork {
    pub fn parse(input: &[u8]) -> Result<(FitFieldAntNetwork, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldAntNetwork::from(val), o))
    }
}

impl From<u8> for FitFieldAntNetwork {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldAntNetwork::Public,
            1 => FitFieldAntNetwork::Antplus,
            2 => FitFieldAntNetwork::Antfs,
            3 => FitFieldAntNetwork::Private,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldAntNetwork", invalid_field_num))
        }
    }
}






#[derive(Debug)]
pub enum FitFieldExdDataUnits { // fit base type: enum
    NoUnits = 0,
    Laps = 1,
    MilesPerHour = 2,
    KilometersPerHour = 3,
    FeetPerHour = 4,
    MetersPerHour = 5,
    DegreesCelsius = 6,
    DegreesFarenheit = 7,
    Zone = 8,
    Gear = 9,
    Rpm = 10,
    Bpm = 11,
    Degrees = 12,
    Millimeters = 13,
    Meters = 14,
    Kilometers = 15,
    Feet = 16,
    Yards = 17,
    Kilofeet = 18,
    Miles = 19,
    Time = 20,
    EnumTurnType = 21,
    Percent = 22,
    Watts = 23,
    WattsPerKilogram = 24,
    EnumBatteryStatus = 25,
    EnumBikeLightBeamAngleMode = 26,
    EnumBikeLightBatteryStatus = 27,
    EnumBikeLightNetworkConfigType = 28,
    Lights = 29,
    Seconds = 30,
    Minutes = 31,
    Hours = 32,
    Calories = 33,
    Kilojoules = 34,
    Milliseconds = 35,
    SecondPerMile = 36,
    SecondPerKilometer = 37,
    Centimeter = 38,
    EnumCoursePoint = 39,
    Bradians = 40,
    EnumSport = 41,
}

impl FitFieldExdDataUnits {
    pub fn parse(input: &[u8]) -> Result<(FitFieldExdDataUnits, &[u8])> {
        let (val, o) = parse_enum(input)?;
        Ok((FitFieldExdDataUnits::from(val), o))
    }
}

impl From<u8> for FitFieldExdDataUnits {
    fn from(code: u8) -> Self {
        match code {
            0 => FitFieldExdDataUnits::NoUnits,
            1 => FitFieldExdDataUnits::Laps,
            2 => FitFieldExdDataUnits::MilesPerHour,
            3 => FitFieldExdDataUnits::KilometersPerHour,
            4 => FitFieldExdDataUnits::FeetPerHour,
            5 => FitFieldExdDataUnits::MetersPerHour,
            6 => FitFieldExdDataUnits::DegreesCelsius,
            7 => FitFieldExdDataUnits::DegreesFarenheit,
            8 => FitFieldExdDataUnits::Zone,
            9 => FitFieldExdDataUnits::Gear,
            10 => FitFieldExdDataUnits::Rpm,
            11 => FitFieldExdDataUnits::Bpm,
            12 => FitFieldExdDataUnits::Degrees,
            13 => FitFieldExdDataUnits::Millimeters,
            14 => FitFieldExdDataUnits::Meters,
            15 => FitFieldExdDataUnits::Kilometers,
            16 => FitFieldExdDataUnits::Feet,
            17 => FitFieldExdDataUnits::Yards,
            18 => FitFieldExdDataUnits::Kilofeet,
            19 => FitFieldExdDataUnits::Miles,
            20 => FitFieldExdDataUnits::Time,
            21 => FitFieldExdDataUnits::EnumTurnType,
            22 => FitFieldExdDataUnits::Percent,
            23 => FitFieldExdDataUnits::Watts,
            24 => FitFieldExdDataUnits::WattsPerKilogram,
            25 => FitFieldExdDataUnits::EnumBatteryStatus,
            26 => FitFieldExdDataUnits::EnumBikeLightBeamAngleMode,
            27 => FitFieldExdDataUnits::EnumBikeLightBatteryStatus,
            28 => FitFieldExdDataUnits::EnumBikeLightNetworkConfigType,
            29 => FitFieldExdDataUnits::Lights,
            30 => FitFieldExdDataUnits::Seconds,
            31 => FitFieldExdDataUnits::Minutes,
            32 => FitFieldExdDataUnits::Hours,
            33 => FitFieldExdDataUnits::Calories,
            34 => FitFieldExdDataUnits::Kilojoules,
            35 => FitFieldExdDataUnits::Milliseconds,
            36 => FitFieldExdDataUnits::SecondPerMile,
            37 => FitFieldExdDataUnits::SecondPerKilometer,
            38 => FitFieldExdDataUnits::Centimeter,
            39 => FitFieldExdDataUnits::EnumCoursePoint,
            40 => FitFieldExdDataUnits::Bradians,
            41 => FitFieldExdDataUnits::EnumSport,
            invalid_field_num => panic!(format!("invalid field_num {} for FitFieldExdDataUnits", invalid_field_num))
        }
    }
}
#[derive(Debug)]
pub struct FitMessageAccelerometerData<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp
    pub timestamp_ms: Option<u16>,  // Millisecond part of the timestamp.
    pub sample_time_offset: Option<u16>,  // Each time in the array describes the time at which the accelerometer sample with the corrosponding index was taken. Limited to 30 samples in each message. The samples may span across seconds. Array size must match the number of samples in accel_x and accel_y and accel_z
    pub accel_x: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub accel_y: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub accel_z: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub calibrated_accel_x: Option<f32>,  // Calibrated accel reading
    pub calibrated_accel_y: Option<f32>,  // Calibrated accel reading
    pub calibrated_accel_z: Option<f32>,  // Calibrated accel reading
    
}
impl<'a> FitMessageAccelerometerData<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageAccelerometerData<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageAccelerometerData {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            sample_time_offset: None,
            accel_x: None,
            accel_y: None,
            accel_z: None,
            calibrated_accel_x: None,
            calibrated_accel_y: None,
            calibrated_accel_z: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageAccelerometerData::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageAccelerometerData:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageAccelerometerData<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // sample_time_offset
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sample_time_offset = Some(val);
                    Ok(())
                },
            
                2 => {  // accel_x
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.accel_x = Some(val);
                    Ok(())
                },
            
                3 => {  // accel_y
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.accel_y = Some(val);
                    Ok(())
                },
            
                4 => {  // accel_z
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.accel_z = Some(val);
                    Ok(())
                },
            
                5 => {  // calibrated_accel_x
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_accel_x = Some(val);
                    Ok(())
                },
            
                6 => {  // calibrated_accel_y
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_accel_y = Some(val);
                    Ok(())
                },
            
                7 => {  // calibrated_accel_z
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_accel_z = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageActivity<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub total_timer_time: Option<f64>,  // Exclude pauses
    pub num_sessions: Option<u16>,  
    pub ftype: Option<FitFieldActivity>,  
    pub event: Option<FitFieldEvent>,  
    pub event_type: Option<FitFieldEventType>,  
    pub local_timestamp: Option<FitFieldLocalDateTime>,  // timestamp epoch expressed in local time, used to convert activity timestamps to local time 
    pub event_group: Option<u8>,  
    
}
impl<'a> FitMessageActivity<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageActivity<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageActivity {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            total_timer_time: None,
            num_sessions: None,
            ftype: None,
            event: None,
            event_type: None,
            local_timestamp: None,
            event_group: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageActivity::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageActivity:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageActivity<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // total_timer_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_timer_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                1 => {  // num_sessions
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.num_sessions = Some(val);
                    Ok(())
                },
            
                2 => {  // ftype
                    let (val, outp) = FitFieldActivity::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                3 => {  // event
                    let (val, outp) = FitFieldEvent::parse(inp)?;
                    inp = outp;
                    message.event = Some(val);
                    Ok(())
                },
            
                4 => {  // event_type
                    let (val, outp) = FitFieldEventType::parse(inp)?;
                    inp = outp;
                    message.event_type = Some(val);
                    Ok(())
                },
            
                5 => {  // local_timestamp
                    let (val, outp) = FitFieldLocalDateTime::parse(inp, message.definition_message.endianness, tz_offset)?;
                    inp = outp;
                    message.local_timestamp = Some(val);
                    Ok(())
                },
            
                6 => {  // event_group
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.event_group = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageAntChannelId<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub channel_number: Option<u8>,  
    pub device_type: Option<u8>,  
    pub device_number: Option<u16>,  
    pub transmission_type: Option<u8>,  
    pub device_index: Option<FitFieldDeviceIndex>,  
    
}
impl<'a> FitMessageAntChannelId<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageAntChannelId<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageAntChannelId {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            channel_number: None,
            device_type: None,
            device_number: None,
            transmission_type: None,
            device_index: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageAntChannelId::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageAntChannelId:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageAntChannelId<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // channel_number
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.channel_number = Some(val);
                    Ok(())
                },
            
                1 => {  // device_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.device_type = val;
                    Ok(())
                },
            
                2 => {  // device_number
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.device_number = val;
                    Ok(())
                },
            
                3 => {  // transmission_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.transmission_type = val;
                    Ok(())
                },
            
                4 => {  // device_index
                    let (val, outp) = FitFieldDeviceIndex::parse(inp)?;
                    inp = outp;
                    message.device_index = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageAntRx<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub fractional_timestamp: Option<f64>,  
    pub mesg_id: Option<&'a [u8]>,  
    pub mesg_data: Option<&'a [u8]>,  
    pub channel_number: Option<u8>,  
    pub data: Option<&'a [u8]>,  
    
}
impl<'a> FitMessageAntRx<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageAntRx<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageAntRx {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            fractional_timestamp: None,
            mesg_id: None,
            mesg_data: None,
            channel_number: None,
            data: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageAntRx::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageAntRx:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageAntRx<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // fractional_timestamp
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.fractional_timestamp = Some(val as f64 / 32768 as f64);
                    Ok(())
                },
            
                1 => {  // mesg_id
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.mesg_id = Some(val);
                    Ok(())
                },
            
                2 => {  // mesg_data
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.mesg_data = Some(val);
                    Ok(())
                },
            
                3 => {  // channel_number
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.channel_number = Some(val);
                    Ok(())
                },
            
                4 => {  // data
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.data = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageAntTx<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub fractional_timestamp: Option<f64>,  
    pub mesg_id: Option<&'a [u8]>,  
    pub mesg_data: Option<&'a [u8]>,  
    pub channel_number: Option<u8>,  
    pub data: Option<&'a [u8]>,  
    
}
impl<'a> FitMessageAntTx<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageAntTx<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageAntTx {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            fractional_timestamp: None,
            mesg_id: None,
            mesg_data: None,
            channel_number: None,
            data: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageAntTx::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageAntTx:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageAntTx<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // fractional_timestamp
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.fractional_timestamp = Some(val as f64 / 32768 as f64);
                    Ok(())
                },
            
                1 => {  // mesg_id
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.mesg_id = Some(val);
                    Ok(())
                },
            
                2 => {  // mesg_data
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.mesg_data = Some(val);
                    Ok(())
                },
            
                3 => {  // channel_number
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.channel_number = Some(val);
                    Ok(())
                },
            
                4 => {  // data
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.data = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageAviationAttitude<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Timestamp message was output
    pub timestamp_ms: Option<u16>,  // Fractional part of timestamp, added to timestamp
    pub system_time: Option<u32>,  // System time associated with sample expressed in ms.
    pub pitch: Option<f64>,  // Range -PI/2 to +PI/2
    pub roll: Option<f64>,  // Range -PI to +PI
    pub accel_lateral: Option<f64>,  // Range -78.4 to +78.4 (-8 Gs to 8 Gs)
    pub accel_normal: Option<f64>,  // Range -78.4 to +78.4 (-8 Gs to 8 Gs)
    pub turn_rate: Option<f64>,  // Range -8.727 to +8.727 (-500 degs/sec to +500 degs/sec)
    pub stage: Option<FitFieldAttitudeStage>,  
    pub attitude_stage_complete: Option<u8>,  // The percent complete of the current attitude stage.  Set to 0 for attitude stages 0, 1 and 2 and to 100 for attitude stage 3 by AHRS modules that do not support it.  Range - 100
    pub track: Option<f64>,  // Track Angle/Heading Range 0 - 2pi
    pub validity: Option<FitFieldAttitudeValidity>,  
    
}
impl<'a> FitMessageAviationAttitude<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageAviationAttitude<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageAviationAttitude {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            system_time: None,
            pitch: None,
            roll: None,
            accel_lateral: None,
            accel_normal: None,
            turn_rate: None,
            stage: None,
            attitude_stage_complete: None,
            track: None,
            validity: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageAviationAttitude::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageAviationAttitude:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageAviationAttitude<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // system_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.system_time = Some(val);
                    Ok(())
                },
            
                2 => {  // pitch
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.pitch = Some(val as f64 / 10430.38 as f64);
                    Ok(())
                },
            
                3 => {  // roll
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.roll = Some(val as f64 / 10430.38 as f64);
                    Ok(())
                },
            
                4 => {  // accel_lateral
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.accel_lateral = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                5 => {  // accel_normal
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.accel_normal = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                6 => {  // turn_rate
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.turn_rate = Some(val as f64 / 1024 as f64);
                    Ok(())
                },
            
                7 => {  // stage
                    let (val, outp) = FitFieldAttitudeStage::parse(inp)?;
                    inp = outp;
                    message.stage = Some(val);
                    Ok(())
                },
            
                8 => {  // attitude_stage_complete
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.attitude_stage_complete = Some(val);
                    Ok(())
                },
            
                9 => {  // track
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.track = Some(val as f64 / 10430.38 as f64);
                    Ok(())
                },
            
                10 => {  // validity
                    let (val, outp) = FitFieldAttitudeValidity::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.validity = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageBikeProfile<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub name: Option<String>,  
    pub sport: Option<FitFieldSport>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    pub odometer: Option<f64>,  
    pub bike_spd_ant_id: Option<u16>,  
    pub bike_cad_ant_id: Option<u16>,  
    pub bike_spdcad_ant_id: Option<u16>,  
    pub bike_power_ant_id: Option<u16>,  
    pub custom_wheelsize: Option<f64>,  
    pub auto_wheelsize: Option<f64>,  
    pub bike_weight: Option<f64>,  
    pub power_cal_factor: Option<f64>,  
    pub auto_wheel_cal: Option<bool>,  
    pub auto_power_zero: Option<bool>,  
    pub id: Option<u8>,  
    pub spd_enabled: Option<bool>,  
    pub cad_enabled: Option<bool>,  
    pub spdcad_enabled: Option<bool>,  
    pub power_enabled: Option<bool>,  
    pub crank_length: Option<f64>,  
    pub enabled: Option<bool>,  
    pub bike_spd_ant_id_trans_type: Option<u8>,  
    pub bike_cad_ant_id_trans_type: Option<u8>,  
    pub bike_spdcad_ant_id_trans_type: Option<u8>,  
    pub bike_power_ant_id_trans_type: Option<u8>,  
    pub odometer_rollover: Option<u8>,  // Rollover counter that can be used to extend the odometer
    pub front_gear_num: Option<u8>,  // Number of front gears
    pub front_gear: Option<u8>,  // Number of teeth on each gear 0 is innermost
    pub rear_gear_num: Option<u8>,  // Number of rear gears
    pub rear_gear: Option<u8>,  // Number of teeth on each gear 0 is innermost
    pub shimano_di2_enabled: Option<bool>,  
    
}
impl<'a> FitMessageBikeProfile<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageBikeProfile<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageBikeProfile {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            name: None,
            sport: None,
            sub_sport: None,
            odometer: None,
            bike_spd_ant_id: None,
            bike_cad_ant_id: None,
            bike_spdcad_ant_id: None,
            bike_power_ant_id: None,
            custom_wheelsize: None,
            auto_wheelsize: None,
            bike_weight: None,
            power_cal_factor: None,
            auto_wheel_cal: None,
            auto_power_zero: None,
            id: None,
            spd_enabled: None,
            cad_enabled: None,
            spdcad_enabled: None,
            power_enabled: None,
            crank_length: None,
            enabled: None,
            bike_spd_ant_id_trans_type: None,
            bike_cad_ant_id_trans_type: None,
            bike_spdcad_ant_id_trans_type: None,
            bike_power_ant_id_trans_type: None,
            odometer_rollover: None,
            front_gear_num: None,
            front_gear: None,
            rear_gear_num: None,
            rear_gear: None,
            shimano_di2_enabled: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageBikeProfile::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageBikeProfile:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageBikeProfile<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                1 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                2 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                3 => {  // odometer
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.odometer = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                4 => {  // bike_spd_ant_id
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.bike_spd_ant_id = val;
                    Ok(())
                },
            
                5 => {  // bike_cad_ant_id
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.bike_cad_ant_id = val;
                    Ok(())
                },
            
                6 => {  // bike_spdcad_ant_id
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.bike_spdcad_ant_id = val;
                    Ok(())
                },
            
                7 => {  // bike_power_ant_id
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.bike_power_ant_id = val;
                    Ok(())
                },
            
                8 => {  // custom_wheelsize
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.custom_wheelsize = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                9 => {  // auto_wheelsize
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.auto_wheelsize = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                10 => {  // bike_weight
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.bike_weight = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                11 => {  // power_cal_factor
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.power_cal_factor = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                12 => {  // auto_wheel_cal
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.auto_wheel_cal = Some(val);
                    Ok(())
                },
            
                13 => {  // auto_power_zero
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.auto_power_zero = Some(val);
                    Ok(())
                },
            
                14 => {  // id
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.id = Some(val);
                    Ok(())
                },
            
                15 => {  // spd_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.spd_enabled = Some(val);
                    Ok(())
                },
            
                16 => {  // cad_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.cad_enabled = Some(val);
                    Ok(())
                },
            
                17 => {  // spdcad_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.spdcad_enabled = Some(val);
                    Ok(())
                },
            
                18 => {  // power_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.power_enabled = Some(val);
                    Ok(())
                },
            
                19 => {  // crank_length
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.crank_length = Some((val as f64 / 2 as f64) - (-110 as f64));
                    Ok(())
                },
            
                20 => {  // enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                21 => {  // bike_spd_ant_id_trans_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.bike_spd_ant_id_trans_type = val;
                    Ok(())
                },
            
                22 => {  // bike_cad_ant_id_trans_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.bike_cad_ant_id_trans_type = val;
                    Ok(())
                },
            
                23 => {  // bike_spdcad_ant_id_trans_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.bike_spdcad_ant_id_trans_type = val;
                    Ok(())
                },
            
                24 => {  // bike_power_ant_id_trans_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.bike_power_ant_id_trans_type = val;
                    Ok(())
                },
            
                37 => {  // odometer_rollover
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.odometer_rollover = Some(val);
                    Ok(())
                },
            
                38 => {  // front_gear_num
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.front_gear_num = val;
                    Ok(())
                },
            
                39 => {  // front_gear
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.front_gear = val;
                    Ok(())
                },
            
                40 => {  // rear_gear_num
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.rear_gear_num = val;
                    Ok(())
                },
            
                41 => {  // rear_gear
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.rear_gear = val;
                    Ok(())
                },
            
                44 => {  // shimano_di2_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.shimano_di2_enabled = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageBloodPressure<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub systolic_pressure: Option<u16>,  
    pub diastolic_pressure: Option<u16>,  
    pub mean_arterial_pressure: Option<u16>,  
    pub map_3_sample_mean: Option<u16>,  
    pub map_morning_values: Option<u16>,  
    pub map_evening_values: Option<u16>,  
    pub heart_rate: Option<u8>,  
    pub heart_rate_type: Option<FitFieldHrType>,  
    pub status: Option<FitFieldBpStatus>,  
    pub user_profile_index: Option<FitFieldMessageIndex>,  // Associates this blood pressure message to a user.  This corresponds to the index of the user profile message in the blood pressure file.
    
}
impl<'a> FitMessageBloodPressure<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageBloodPressure<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageBloodPressure {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            systolic_pressure: None,
            diastolic_pressure: None,
            mean_arterial_pressure: None,
            map_3_sample_mean: None,
            map_morning_values: None,
            map_evening_values: None,
            heart_rate: None,
            heart_rate_type: None,
            status: None,
            user_profile_index: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageBloodPressure::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageBloodPressure:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageBloodPressure<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // systolic_pressure
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.systolic_pressure = Some(val);
                    Ok(())
                },
            
                1 => {  // diastolic_pressure
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.diastolic_pressure = Some(val);
                    Ok(())
                },
            
                2 => {  // mean_arterial_pressure
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.mean_arterial_pressure = Some(val);
                    Ok(())
                },
            
                3 => {  // map_3_sample_mean
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.map_3_sample_mean = Some(val);
                    Ok(())
                },
            
                4 => {  // map_morning_values
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.map_morning_values = Some(val);
                    Ok(())
                },
            
                5 => {  // map_evening_values
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.map_evening_values = Some(val);
                    Ok(())
                },
            
                6 => {  // heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.heart_rate = Some(val);
                    Ok(())
                },
            
                7 => {  // heart_rate_type
                    let (val, outp) = FitFieldHrType::parse(inp)?;
                    inp = outp;
                    message.heart_rate_type = Some(val);
                    Ok(())
                },
            
                8 => {  // status
                    let (val, outp) = FitFieldBpStatus::parse(inp)?;
                    inp = outp;
                    message.status = Some(val);
                    Ok(())
                },
            
                9 => {  // user_profile_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.user_profile_index = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageCadenceZone<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub high_value: Option<u8>,  
    pub name: Option<String>,  
    
}
impl<'a> FitMessageCadenceZone<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageCadenceZone<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageCadenceZone {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            high_value: None,
            name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageCadenceZone::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageCadenceZone:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageCadenceZone<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // high_value
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.high_value = Some(val);
                    Ok(())
                },
            
                1 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageCameraEvent<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp.
    pub timestamp_ms: Option<u16>,  // Millisecond part of the timestamp.
    pub camera_event_type: Option<FitFieldCameraEventType>,  
    pub camera_file_uuid: Option<String>,  
    pub camera_orientation: Option<FitFieldCameraOrientationType>,  
    
}
impl<'a> FitMessageCameraEvent<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageCameraEvent<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageCameraEvent {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            camera_event_type: None,
            camera_file_uuid: None,
            camera_orientation: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageCameraEvent::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageCameraEvent:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageCameraEvent<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // camera_event_type
                    let (val, outp) = FitFieldCameraEventType::parse(inp)?;
                    inp = outp;
                    message.camera_event_type = Some(val);
                    Ok(())
                },
            
                2 => {  // camera_file_uuid
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.camera_file_uuid = Some(val);
                    Ok(())
                },
            
                3 => {  // camera_orientation
                    let (val, outp) = FitFieldCameraOrientationType::parse(inp)?;
                    inp = outp;
                    message.camera_orientation = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageCapabilities<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub languages: Option<u8>,  // Use language_bits_x types where x is index of array.
    pub sports: Option<FitFieldSportBits0>,  // Use sport_bits_x types where x is index of array.
    pub workouts_supported: Option<FitFieldWorkoutCapabilities>,  
    pub connectivity_supported: Option<FitFieldConnectivityCapabilities>,  
    
}
impl<'a> FitMessageCapabilities<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageCapabilities<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageCapabilities {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            languages: None,
            sports: None,
            workouts_supported: None,
            connectivity_supported: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageCapabilities::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageCapabilities:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageCapabilities<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // languages
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.languages = val;
                    Ok(())
                },
            
                1 => {  // sports
                    let (val, outp) = FitFieldSportBits0::parse(inp)?;
                    inp = outp;
                    message.sports = Some(val);
                    Ok(())
                },
            
                21 => {  // workouts_supported
                    let (val, outp) = FitFieldWorkoutCapabilities::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.workouts_supported = Some(val);
                    Ok(())
                },
            
                23 => {  // connectivity_supported
                    let (val, outp) = FitFieldConnectivityCapabilities::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.connectivity_supported = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageConnectivity<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub bluetooth_enabled: Option<bool>,  // Use Bluetooth for connectivity features
    pub bluetooth_le_enabled: Option<bool>,  // Use Bluetooth Low Energy for connectivity features
    pub ant_enabled: Option<bool>,  // Use ANT for connectivity features
    pub name: Option<String>,  
    pub live_tracking_enabled: Option<bool>,  
    pub weather_conditions_enabled: Option<bool>,  
    pub weather_alerts_enabled: Option<bool>,  
    pub auto_activity_upload_enabled: Option<bool>,  
    pub course_download_enabled: Option<bool>,  
    pub workout_download_enabled: Option<bool>,  
    pub gps_ephemeris_download_enabled: Option<bool>,  
    pub incident_detection_enabled: Option<bool>,  
    pub grouptrack_enabled: Option<bool>,  
    
}
impl<'a> FitMessageConnectivity<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageConnectivity<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageConnectivity {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            bluetooth_enabled: None,
            bluetooth_le_enabled: None,
            ant_enabled: None,
            name: None,
            live_tracking_enabled: None,
            weather_conditions_enabled: None,
            weather_alerts_enabled: None,
            auto_activity_upload_enabled: None,
            course_download_enabled: None,
            workout_download_enabled: None,
            gps_ephemeris_download_enabled: None,
            incident_detection_enabled: None,
            grouptrack_enabled: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageConnectivity::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageConnectivity:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageConnectivity<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // bluetooth_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.bluetooth_enabled = Some(val);
                    Ok(())
                },
            
                1 => {  // bluetooth_le_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.bluetooth_le_enabled = Some(val);
                    Ok(())
                },
            
                2 => {  // ant_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.ant_enabled = Some(val);
                    Ok(())
                },
            
                3 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                4 => {  // live_tracking_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.live_tracking_enabled = Some(val);
                    Ok(())
                },
            
                5 => {  // weather_conditions_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.weather_conditions_enabled = Some(val);
                    Ok(())
                },
            
                6 => {  // weather_alerts_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.weather_alerts_enabled = Some(val);
                    Ok(())
                },
            
                7 => {  // auto_activity_upload_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.auto_activity_upload_enabled = Some(val);
                    Ok(())
                },
            
                8 => {  // course_download_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.course_download_enabled = Some(val);
                    Ok(())
                },
            
                9 => {  // workout_download_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.workout_download_enabled = Some(val);
                    Ok(())
                },
            
                10 => {  // gps_ephemeris_download_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.gps_ephemeris_download_enabled = Some(val);
                    Ok(())
                },
            
                11 => {  // incident_detection_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.incident_detection_enabled = Some(val);
                    Ok(())
                },
            
                12 => {  // grouptrack_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.grouptrack_enabled = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageCourse<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub sport: Option<FitFieldSport>,  
    pub name: Option<String>,  
    pub capabilities: Option<FitFieldCourseCapabilities>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    
}
impl<'a> FitMessageCourse<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageCourse<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageCourse {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            sport: None,
            name: None,
            capabilities: None,
            sub_sport: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageCourse::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageCourse:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageCourse<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                4 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                5 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                6 => {  // capabilities
                    let (val, outp) = FitFieldCourseCapabilities::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.capabilities = Some(val);
                    Ok(())
                },
            
                7 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageCoursePoint<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub timestamp: Option<FitFieldDateTime>,  
    pub position_lat: Option<i32>,  
    pub position_long: Option<i32>,  
    pub distance: Option<f64>,  
    pub ftype: Option<FitFieldCoursePoint>,  
    pub name: Option<String>,  
    pub favorite: Option<bool>,  
    
}
impl<'a> FitMessageCoursePoint<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageCoursePoint<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageCoursePoint {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            timestamp: None,
            position_lat: None,
            position_long: None,
            distance: None,
            ftype: None,
            name: None,
            favorite: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageCoursePoint::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageCoursePoint:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageCoursePoint<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                1 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                2 => {  // position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_lat = Some(val);
                    Ok(())
                },
            
                3 => {  // position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_long = Some(val);
                    Ok(())
                },
            
                4 => {  // distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                5 => {  // ftype
                    let (val, outp) = FitFieldCoursePoint::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                6 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                8 => {  // favorite
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.favorite = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageDeveloperDataId<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub developer_id: Option<&'a [u8]>,  
    pub application_id: Option<&'a [u8]>,  
    pub manufacturer_id: Option<FitFieldManufacturer>,  
    pub developer_data_index: Option<u8>,  
    pub application_version: Option<u32>,  
    
}
impl<'a> FitMessageDeveloperDataId<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageDeveloperDataId<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageDeveloperDataId {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            developer_id: None,
            application_id: None,
            manufacturer_id: None,
            developer_data_index: None,
            application_version: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageDeveloperDataId::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageDeveloperDataId:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageDeveloperDataId<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // developer_id
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.developer_id = Some(val);
                    Ok(())
                },
            
                1 => {  // application_id
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.application_id = Some(val);
                    Ok(())
                },
            
                2 => {  // manufacturer_id
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer_id = Some(val);
                    Ok(())
                },
            
                3 => {  // developer_data_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.developer_data_index = Some(val);
                    Ok(())
                },
            
                4 => {  // application_version
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.application_version = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageDeviceInfoSubfieldDeviceType {
    Default(u8),
    AntDeviceType(u8),
    AntplusDeviceType(FitFieldAntplusDeviceType),
}

impl FitMessageDeviceInfoSubfieldDeviceType {
    fn parse<'a>(message: &FitMessageDeviceInfo<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageDeviceInfoSubfieldDeviceType,  &'a [u8])> {
        
        match message.source_type {
        
            Some(FitFieldSourceType::Antplus) => {
                let (val, o) = FitFieldAntplusDeviceType::parse(inp)?;
                return Ok((FitMessageDeviceInfoSubfieldDeviceType::AntplusDeviceType(val), o))
            },
        
            Some(FitFieldSourceType::Ant) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageDeviceInfoSubfieldDeviceType::AntDeviceType(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint8(inp)?;
        Ok((FitMessageDeviceInfoSubfieldDeviceType::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageDeviceInfoSubfieldProduct {
    Default(u16),
    GarminProduct(FitFieldGarminProduct),
}

impl FitMessageDeviceInfoSubfieldProduct {
    fn parse<'a>(message: &FitMessageDeviceInfo<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageDeviceInfoSubfieldProduct,  &'a [u8])> {
        
        match message.manufacturer {
        
            Some(FitFieldManufacturer::Garmin) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageDeviceInfoSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::Dynastream) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageDeviceInfoSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::DynastreamOem) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageDeviceInfoSubfieldProduct::GarminProduct(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
        Ok((FitMessageDeviceInfoSubfieldProduct::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageDeviceInfo<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub device_index: Option<FitFieldDeviceIndex>,  
    pub device_type: Option<FitMessageDeviceInfoSubfieldDeviceType>,  
    pub manufacturer: Option<FitFieldManufacturer>,  
    pub serial_number: Option<u32>,  
    pub product: Option<FitMessageDeviceInfoSubfieldProduct>,  
    pub software_version: Option<f64>,  
    pub hardware_version: Option<u8>,  
    pub cum_operating_time: Option<u32>,  // Reset by new battery or charge.
    pub battery_voltage: Option<f64>,  
    pub battery_status: Option<FitFieldBatteryStatus>,  
    pub sensor_position: Option<FitFieldBodyLocation>,  // Indicates the location of the sensor
    pub descriptor: Option<String>,  // Used to describe the sensor or location
    pub ant_transmission_type: Option<u8>,  
    pub ant_device_number: Option<u16>,  
    pub ant_network: Option<FitFieldAntNetwork>,  
    pub source_type: Option<FitFieldSourceType>,  
    pub product_name: Option<String>,  // Optional free form string to indicate the devices name or model
    
}
impl<'a> FitMessageDeviceInfo<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageDeviceInfo<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageDeviceInfo {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            device_index: None,
            device_type: None,
            manufacturer: None,
            serial_number: None,
            product: None,
            software_version: None,
            hardware_version: None,
            cum_operating_time: None,
            battery_voltage: None,
            battery_status: None,
            sensor_position: None,
            descriptor: None,
            ant_transmission_type: None,
            ant_device_number: None,
            ant_network: None,
            source_type: None,
            product_name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageDeviceInfo::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageDeviceInfo:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageDeviceInfo<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // device_index
                    let (val, outp) = FitFieldDeviceIndex::parse(inp)?;
                    inp = outp;
                    message.device_index = Some(val);
                    Ok(())
                },
            
                1 => {  // device_type
                    let (val, outp) = FitMessageDeviceInfoSubfieldDeviceType::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.device_type = Some(val);
                    Ok(())
                },
            
                2 => {  // manufacturer
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer = Some(val);
                    Ok(())
                },
            
                3 => {  // serial_number
                    let (val, outp) = parse_uint32z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.serial_number = val;
                    Ok(())
                },
            
                4 => {  // product
                    let (val, outp) = FitMessageDeviceInfoSubfieldProduct::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.product = Some(val);
                    Ok(())
                },
            
                5 => {  // software_version
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.software_version = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                6 => {  // hardware_version
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.hardware_version = Some(val);
                    Ok(())
                },
            
                7 => {  // cum_operating_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.cum_operating_time = Some(val);
                    Ok(())
                },
            
                10 => {  // battery_voltage
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.battery_voltage = Some(val as f64 / 256 as f64);
                    Ok(())
                },
            
                11 => {  // battery_status
                    let (val, outp) = FitFieldBatteryStatus::parse(inp)?;
                    inp = outp;
                    message.battery_status = Some(val);
                    Ok(())
                },
            
                18 => {  // sensor_position
                    let (val, outp) = FitFieldBodyLocation::parse(inp)?;
                    inp = outp;
                    message.sensor_position = Some(val);
                    Ok(())
                },
            
                19 => {  // descriptor
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.descriptor = Some(val);
                    Ok(())
                },
            
                20 => {  // ant_transmission_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.ant_transmission_type = val;
                    Ok(())
                },
            
                21 => {  // ant_device_number
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.ant_device_number = val;
                    Ok(())
                },
            
                22 => {  // ant_network
                    let (val, outp) = FitFieldAntNetwork::parse(inp)?;
                    inp = outp;
                    message.ant_network = Some(val);
                    Ok(())
                },
            
                25 => {  // source_type
                    let (val, outp) = FitFieldSourceType::parse(inp)?;
                    inp = outp;
                    message.source_type = Some(val);
                    Ok(())
                },
            
                27 => {  // product_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.product_name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageDeviceSettings<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub active_time_zone: Option<u8>,  // Index into time zone arrays.
    pub utc_offset: Option<u32>,  // Offset from system time. Required to convert timestamp from system time to UTC.
    pub time_offset: Option<u32>,  // Offset from system time.
    pub time_mode: Option<FitFieldTimeMode>,  // Display mode for the time
    pub time_zone_offset: Option<f64>,  // timezone offset in 1/4 hour increments
    pub backlight_mode: Option<FitFieldBacklightMode>,  // Mode for backlight
    pub activity_tracker_enabled: Option<bool>,  // Enabled state of the activity tracker functionality
    pub clock_time: Option<FitFieldDateTime>,  // UTC timestamp used to set the devices clock and date
    pub pages_enabled: Option<u16>,  // Bitfield  to configure enabled screens for each supported loop
    pub move_alert_enabled: Option<bool>,  // Enabled state of the move alert
    pub date_mode: Option<FitFieldDateMode>,  // Display mode for the date
    pub display_orientation: Option<FitFieldDisplayOrientation>,  
    pub mounting_side: Option<FitFieldSide>,  
    pub default_page: Option<u16>,  // Bitfield to indicate one page as default for each supported loop
    pub autosync_min_steps: Option<u16>,  // Minimum steps before an autosync can occur
    pub autosync_min_time: Option<u16>,  // Minimum minutes before an autosync can occur
    pub lactate_threshold_autodetect_enabled: Option<bool>,  // Enable auto-detect setting for the lactate threshold feature.
    pub ble_auto_upload_enabled: Option<bool>,  // Automatically upload using BLE
    pub auto_sync_frequency: Option<FitFieldAutoSyncFrequency>,  // Helps to conserve battery by changing modes
    pub auto_activity_detect: Option<FitFieldAutoActivityDetect>,  // Allows setting specific activities auto-activity detect enabled/disabled settings
    pub number_of_screens: Option<u8>,  // Number of screens configured to display
    pub smart_notification_display_orientation: Option<FitFieldDisplayOrientation>,  // Smart Notification display orientation
    
}
impl<'a> FitMessageDeviceSettings<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageDeviceSettings<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageDeviceSettings {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            active_time_zone: None,
            utc_offset: None,
            time_offset: None,
            time_mode: None,
            time_zone_offset: None,
            backlight_mode: None,
            activity_tracker_enabled: None,
            clock_time: None,
            pages_enabled: None,
            move_alert_enabled: None,
            date_mode: None,
            display_orientation: None,
            mounting_side: None,
            default_page: None,
            autosync_min_steps: None,
            autosync_min_time: None,
            lactate_threshold_autodetect_enabled: None,
            ble_auto_upload_enabled: None,
            auto_sync_frequency: None,
            auto_activity_detect: None,
            number_of_screens: None,
            smart_notification_display_orientation: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageDeviceSettings::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageDeviceSettings:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageDeviceSettings<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // active_time_zone
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.active_time_zone = Some(val);
                    Ok(())
                },
            
                1 => {  // utc_offset
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.utc_offset = Some(val);
                    Ok(())
                },
            
                2 => {  // time_offset
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_offset = Some(val);
                    Ok(())
                },
            
                4 => {  // time_mode
                    let (val, outp) = FitFieldTimeMode::parse(inp)?;
                    inp = outp;
                    message.time_mode = Some(val);
                    Ok(())
                },
            
                5 => {  // time_zone_offset
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.time_zone_offset = Some(val as f64 / 4 as f64);
                    Ok(())
                },
            
                12 => {  // backlight_mode
                    let (val, outp) = FitFieldBacklightMode::parse(inp)?;
                    inp = outp;
                    message.backlight_mode = Some(val);
                    Ok(())
                },
            
                36 => {  // activity_tracker_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.activity_tracker_enabled = Some(val);
                    Ok(())
                },
            
                39 => {  // clock_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.clock_time = Some(val);
                    Ok(())
                },
            
                40 => {  // pages_enabled
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.pages_enabled = Some(val);
                    Ok(())
                },
            
                46 => {  // move_alert_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.move_alert_enabled = Some(val);
                    Ok(())
                },
            
                47 => {  // date_mode
                    let (val, outp) = FitFieldDateMode::parse(inp)?;
                    inp = outp;
                    message.date_mode = Some(val);
                    Ok(())
                },
            
                55 => {  // display_orientation
                    let (val, outp) = FitFieldDisplayOrientation::parse(inp)?;
                    inp = outp;
                    message.display_orientation = Some(val);
                    Ok(())
                },
            
                56 => {  // mounting_side
                    let (val, outp) = FitFieldSide::parse(inp)?;
                    inp = outp;
                    message.mounting_side = Some(val);
                    Ok(())
                },
            
                57 => {  // default_page
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.default_page = Some(val);
                    Ok(())
                },
            
                58 => {  // autosync_min_steps
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.autosync_min_steps = Some(val);
                    Ok(())
                },
            
                59 => {  // autosync_min_time
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.autosync_min_time = Some(val);
                    Ok(())
                },
            
                80 => {  // lactate_threshold_autodetect_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.lactate_threshold_autodetect_enabled = Some(val);
                    Ok(())
                },
            
                86 => {  // ble_auto_upload_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.ble_auto_upload_enabled = Some(val);
                    Ok(())
                },
            
                89 => {  // auto_sync_frequency
                    let (val, outp) = FitFieldAutoSyncFrequency::parse(inp)?;
                    inp = outp;
                    message.auto_sync_frequency = Some(val);
                    Ok(())
                },
            
                90 => {  // auto_activity_detect
                    let (val, outp) = FitFieldAutoActivityDetect::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.auto_activity_detect = Some(val);
                    Ok(())
                },
            
                94 => {  // number_of_screens
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.number_of_screens = Some(val);
                    Ok(())
                },
            
                95 => {  // smart_notification_display_orientation
                    let (val, outp) = FitFieldDisplayOrientation::parse(inp)?;
                    inp = outp;
                    message.smart_notification_display_orientation = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageEventSubfieldData {
    Default(u32),
    CommTimeout(FitFieldCommTimeoutType),
    FitnessEquipmentState(FitFieldFitnessEquipmentState),
    SpeedHighAlert(u32),
    TimerTrigger(FitFieldTimerTrigger),
    CadLowAlert(u16),
    SportPoint(u32),
    GearChangeData(u32),
    PowerLowAlert(u16),
    HrHighAlert(u8),
    CalorieDurationAlert(u32),
    PowerHighAlert(u16),
    HrLowAlert(u8),
    CadHighAlert(u16),
    RiderPosition(FitFieldRiderPositionType),
    CoursePointIndex(FitFieldMessageIndex),
    TimeDurationAlert(u32),
    DistanceDurationAlert(u32),
    BatteryLevel(u16),
    VirtualPartnerSpeed(u16),
    SpeedLowAlert(u32),
}

impl FitMessageEventSubfieldData {
    fn parse<'a>(message: &FitMessageEvent<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageEventSubfieldData,  &'a [u8])> {
        
        match message.event {
        
            Some(FitFieldEvent::Timer) => {
                let (val, o) = FitFieldTimerTrigger::parse(inp)?;
                return Ok((FitMessageEventSubfieldData::TimerTrigger(val), o))
            },
        
            Some(FitFieldEvent::CoursePoint) => {
                let (val, o) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::CoursePointIndex(val), o))
            },
        
            Some(FitFieldEvent::Battery) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::BatteryLevel(val), o))
            },
        
            Some(FitFieldEvent::VirtualPartnerPace) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::VirtualPartnerSpeed(val), o))
            },
        
            Some(FitFieldEvent::HrHighAlert) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageEventSubfieldData::HrHighAlert(val), o))
            },
        
            Some(FitFieldEvent::HrLowAlert) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageEventSubfieldData::HrLowAlert(val), o))
            },
        
            Some(FitFieldEvent::SpeedHighAlert) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::SpeedHighAlert(val), o))
            },
        
            Some(FitFieldEvent::SpeedLowAlert) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::SpeedLowAlert(val), o))
            },
        
            Some(FitFieldEvent::CadHighAlert) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::CadHighAlert(val), o))
            },
        
            Some(FitFieldEvent::CadLowAlert) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::CadLowAlert(val), o))
            },
        
            Some(FitFieldEvent::PowerHighAlert) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::PowerHighAlert(val), o))
            },
        
            Some(FitFieldEvent::PowerLowAlert) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::PowerLowAlert(val), o))
            },
        
            Some(FitFieldEvent::TimeDurationAlert) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::TimeDurationAlert(val), o))
            },
        
            Some(FitFieldEvent::DistanceDurationAlert) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::DistanceDurationAlert(val), o))
            },
        
            Some(FitFieldEvent::CalorieDurationAlert) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::CalorieDurationAlert(val), o))
            },
        
            Some(FitFieldEvent::FitnessEquipment) => {
                let (val, o) = FitFieldFitnessEquipmentState::parse(inp)?;
                return Ok((FitMessageEventSubfieldData::FitnessEquipmentState(val), o))
            },
        
            Some(FitFieldEvent::SportPoint) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::SportPoint(val), o))
            },
        
            Some(FitFieldEvent::FrontGearChange) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::GearChangeData(val), o))
            },
        
            Some(FitFieldEvent::RearGearChange) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::GearChangeData(val), o))
            },
        
            Some(FitFieldEvent::RiderPositionChange) => {
                let (val, o) = FitFieldRiderPositionType::parse(inp)?;
                return Ok((FitMessageEventSubfieldData::RiderPosition(val), o))
            },
        
            Some(FitFieldEvent::CommTimeout) => {
                let (val, o) = FitFieldCommTimeoutType::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageEventSubfieldData::CommTimeout(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageEventSubfieldData::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageEvent<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub event: Option<FitFieldEvent>,  
    pub event_type: Option<FitFieldEventType>,  
    pub data16: Option<u16>,  
    pub data: Option<FitMessageEventSubfieldData>,  
    pub event_group: Option<u8>,  
    pub score: Option<u16>,  // Do not populate directly.  Autogenerated by decoder for sport_point subfield components
    pub opponent_score: Option<u16>,  // Do not populate directly.  Autogenerated by decoder for sport_point subfield components
    pub front_gear_num: Option<u8>,  // Do not populate directly.  Autogenerated by decoder for gear_change subfield components.  Front gear number. 1 is innermost.
    pub front_gear: Option<u8>,  // Do not populate directly.  Autogenerated by decoder for gear_change subfield components.  Number of front teeth.
    pub rear_gear_num: Option<u8>,  // Do not populate directly.  Autogenerated by decoder for gear_change subfield components.  Rear gear number. 1 is innermost.
    pub rear_gear: Option<u8>,  // Do not populate directly.  Autogenerated by decoder for gear_change subfield components.  Number of rear teeth.
    pub device_index: Option<FitFieldDeviceIndex>,  
    
}
impl<'a> FitMessageEvent<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageEvent<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageEvent {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            event: None,
            event_type: None,
            data16: None,
            data: None,
            event_group: None,
            score: None,
            opponent_score: None,
            front_gear_num: None,
            front_gear: None,
            rear_gear_num: None,
            rear_gear: None,
            device_index: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageEvent::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageEvent:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageEvent<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // event
                    let (val, outp) = FitFieldEvent::parse(inp)?;
                    inp = outp;
                    message.event = Some(val);
                    Ok(())
                },
            
                1 => {  // event_type
                    let (val, outp) = FitFieldEventType::parse(inp)?;
                    inp = outp;
                    message.event_type = Some(val);
                    Ok(())
                },
            
                2 => {  // data16
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.data16 = Some(val);
                    Ok(())
                },
            
                3 => {  // data
                    let (val, outp) = FitMessageEventSubfieldData::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.data = Some(val);
                    Ok(())
                },
            
                4 => {  // event_group
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.event_group = Some(val);
                    Ok(())
                },
            
                7 => {  // score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.score = Some(val);
                    Ok(())
                },
            
                8 => {  // opponent_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.opponent_score = Some(val);
                    Ok(())
                },
            
                9 => {  // front_gear_num
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.front_gear_num = val;
                    Ok(())
                },
            
                10 => {  // front_gear
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.front_gear = val;
                    Ok(())
                },
            
                11 => {  // rear_gear_num
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.rear_gear_num = val;
                    Ok(())
                },
            
                12 => {  // rear_gear
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.rear_gear = val;
                    Ok(())
                },
            
                13 => {  // device_index
                    let (val, outp) = FitFieldDeviceIndex::parse(inp)?;
                    inp = outp;
                    message.device_index = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageExdDataConceptConfiguration<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub screen_index: Option<u8>,  
    pub concept_field: Option<&'a [u8]>,  
    pub field_id: Option<u8>,  
    pub concept_index: Option<u8>,  
    pub data_page: Option<u8>,  
    pub concept_key: Option<u8>,  
    pub scaling: Option<u8>,  
    pub data_units: Option<FitFieldExdDataUnits>,  
    pub qualifier: Option<FitFieldExdQualifiers>,  
    pub descriptor: Option<FitFieldExdDescriptors>,  
    pub is_signed: Option<bool>,  
    
}
impl<'a> FitMessageExdDataConceptConfiguration<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageExdDataConceptConfiguration<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageExdDataConceptConfiguration {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            screen_index: None,
            concept_field: None,
            field_id: None,
            concept_index: None,
            data_page: None,
            concept_key: None,
            scaling: None,
            data_units: None,
            qualifier: None,
            descriptor: None,
            is_signed: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageExdDataConceptConfiguration::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageExdDataConceptConfiguration:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageExdDataConceptConfiguration<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // screen_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.screen_index = Some(val);
                    Ok(())
                },
            
                1 => {  // concept_field
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.concept_field = Some(val);
                    Ok(())
                },
            
                2 => {  // field_id
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.field_id = Some(val);
                    Ok(())
                },
            
                3 => {  // concept_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.concept_index = Some(val);
                    Ok(())
                },
            
                4 => {  // data_page
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.data_page = Some(val);
                    Ok(())
                },
            
                5 => {  // concept_key
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.concept_key = Some(val);
                    Ok(())
                },
            
                6 => {  // scaling
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.scaling = Some(val);
                    Ok(())
                },
            
                8 => {  // data_units
                    let (val, outp) = FitFieldExdDataUnits::parse(inp)?;
                    inp = outp;
                    message.data_units = Some(val);
                    Ok(())
                },
            
                9 => {  // qualifier
                    let (val, outp) = FitFieldExdQualifiers::parse(inp)?;
                    inp = outp;
                    message.qualifier = Some(val);
                    Ok(())
                },
            
                10 => {  // descriptor
                    let (val, outp) = FitFieldExdDescriptors::parse(inp)?;
                    inp = outp;
                    message.descriptor = Some(val);
                    Ok(())
                },
            
                11 => {  // is_signed
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.is_signed = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageExdDataFieldConfiguration<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub screen_index: Option<u8>,  
    pub concept_field: Option<&'a [u8]>,  
    pub field_id: Option<u8>,  
    pub concept_count: Option<u8>,  
    pub display_type: Option<FitFieldExdDisplayType>,  
    pub title: Option<String>,  
    
}
impl<'a> FitMessageExdDataFieldConfiguration<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageExdDataFieldConfiguration<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageExdDataFieldConfiguration {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            screen_index: None,
            concept_field: None,
            field_id: None,
            concept_count: None,
            display_type: None,
            title: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageExdDataFieldConfiguration::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageExdDataFieldConfiguration:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageExdDataFieldConfiguration<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // screen_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.screen_index = Some(val);
                    Ok(())
                },
            
                1 => {  // concept_field
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.concept_field = Some(val);
                    Ok(())
                },
            
                2 => {  // field_id
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.field_id = Some(val);
                    Ok(())
                },
            
                3 => {  // concept_count
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.concept_count = Some(val);
                    Ok(())
                },
            
                4 => {  // display_type
                    let (val, outp) = FitFieldExdDisplayType::parse(inp)?;
                    inp = outp;
                    message.display_type = Some(val);
                    Ok(())
                },
            
                5 => {  // title
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.title = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageExdScreenConfiguration<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub screen_index: Option<u8>,  
    pub field_count: Option<u8>,  // number of fields in screen
    pub layout: Option<FitFieldExdLayout>,  
    pub screen_enabled: Option<bool>,  
    
}
impl<'a> FitMessageExdScreenConfiguration<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageExdScreenConfiguration<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageExdScreenConfiguration {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            screen_index: None,
            field_count: None,
            layout: None,
            screen_enabled: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageExdScreenConfiguration::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageExdScreenConfiguration:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageExdScreenConfiguration<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // screen_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.screen_index = Some(val);
                    Ok(())
                },
            
                1 => {  // field_count
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.field_count = Some(val);
                    Ok(())
                },
            
                2 => {  // layout
                    let (val, outp) = FitFieldExdLayout::parse(inp)?;
                    inp = outp;
                    message.layout = Some(val);
                    Ok(())
                },
            
                3 => {  // screen_enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.screen_enabled = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageFieldCapabilities<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub file: Option<FitFieldFile>,  
    pub mesg_num: Option<FitFieldMesgNum>,  
    pub field_num: Option<u8>,  
    pub count: Option<u16>,  
    
}
impl<'a> FitMessageFieldCapabilities<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageFieldCapabilities<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageFieldCapabilities {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            file: None,
            mesg_num: None,
            field_num: None,
            count: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageFieldCapabilities::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageFieldCapabilities:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageFieldCapabilities<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // file
                    let (val, outp) = FitFieldFile::parse(inp)?;
                    inp = outp;
                    message.file = Some(val);
                    Ok(())
                },
            
                1 => {  // mesg_num
                    let (val, outp) = FitFieldMesgNum::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.mesg_num = Some(val);
                    Ok(())
                },
            
                2 => {  // field_num
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.field_num = Some(val);
                    Ok(())
                },
            
                3 => {  // count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.count = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageFieldDescription<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub developer_data_index: Option<u8>,  
    pub field_definition_number: Option<u8>,  
    pub fit_base_type_id: Option<FitFieldFitBaseType>,  
    pub field_name: Option<String>,  
    pub array: Option<u8>,  
    pub components: Option<String>,  
    pub scale: Option<u8>,  
    pub offset: Option<i8>,  
    pub units: Option<String>,  
    pub bits: Option<String>,  
    pub accumulate: Option<String>,  
    pub fit_base_unit_id: Option<FitFieldFitBaseUnit>,  
    pub native_mesg_num: Option<FitFieldMesgNum>,  
    pub native_field_num: Option<u8>,  
    
}
impl<'a> FitMessageFieldDescription<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageFieldDescription<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageFieldDescription {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            developer_data_index: None,
            field_definition_number: None,
            fit_base_type_id: None,
            field_name: None,
            array: None,
            components: None,
            scale: None,
            offset: None,
            units: None,
            bits: None,
            accumulate: None,
            fit_base_unit_id: None,
            native_mesg_num: None,
            native_field_num: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageFieldDescription::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageFieldDescription:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageFieldDescription<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // developer_data_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.developer_data_index = Some(val);
                    Ok(())
                },
            
                1 => {  // field_definition_number
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.field_definition_number = Some(val);
                    Ok(())
                },
            
                2 => {  // fit_base_type_id
                    let (val, outp) = FitFieldFitBaseType::parse(inp)?;
                    inp = outp;
                    message.fit_base_type_id = Some(val);
                    Ok(())
                },
            
                3 => {  // field_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.field_name = Some(val);
                    Ok(())
                },
            
                4 => {  // array
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.array = Some(val);
                    Ok(())
                },
            
                5 => {  // components
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.components = Some(val);
                    Ok(())
                },
            
                6 => {  // scale
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.scale = Some(val);
                    Ok(())
                },
            
                7 => {  // offset
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.offset = Some(val);
                    Ok(())
                },
            
                8 => {  // units
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.units = Some(val);
                    Ok(())
                },
            
                9 => {  // bits
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.bits = Some(val);
                    Ok(())
                },
            
                10 => {  // accumulate
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.accumulate = Some(val);
                    Ok(())
                },
            
                13 => {  // fit_base_unit_id
                    let (val, outp) = FitFieldFitBaseUnit::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.fit_base_unit_id = Some(val);
                    Ok(())
                },
            
                14 => {  // native_mesg_num
                    let (val, outp) = FitFieldMesgNum::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.native_mesg_num = Some(val);
                    Ok(())
                },
            
                15 => {  // native_field_num
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.native_field_num = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageFileCapabilities<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub ftype: Option<FitFieldFile>,  
    pub flags: Option<FitFieldFileFlags>,  
    pub directory: Option<String>,  
    pub max_count: Option<u16>,  
    pub max_size: Option<u32>,  
    
}
impl<'a> FitMessageFileCapabilities<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageFileCapabilities<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageFileCapabilities {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            ftype: None,
            flags: None,
            directory: None,
            max_count: None,
            max_size: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageFileCapabilities::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageFileCapabilities:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageFileCapabilities<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // ftype
                    let (val, outp) = FitFieldFile::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                1 => {  // flags
                    let (val, outp) = FitFieldFileFlags::parse(inp)?;
                    inp = outp;
                    message.flags = Some(val);
                    Ok(())
                },
            
                2 => {  // directory
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.directory = Some(val);
                    Ok(())
                },
            
                3 => {  // max_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_count = Some(val);
                    Ok(())
                },
            
                4 => {  // max_size
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_size = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageFileCreator<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub software_version: Option<u16>,  
    pub hardware_version: Option<u8>,  
    
}
impl<'a> FitMessageFileCreator<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageFileCreator<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageFileCreator {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            software_version: None,
            hardware_version: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageFileCreator::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageFileCreator:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageFileCreator<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // software_version
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.software_version = Some(val);
                    Ok(())
                },
            
                1 => {  // hardware_version
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.hardware_version = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageFileIdSubfieldProduct {
    Default(u16),
    GarminProduct(FitFieldGarminProduct),
}

impl FitMessageFileIdSubfieldProduct {
    fn parse<'a>(message: &FitMessageFileId<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageFileIdSubfieldProduct,  &'a [u8])> {
        
        match message.manufacturer {
        
            Some(FitFieldManufacturer::Garmin) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageFileIdSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::Dynastream) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageFileIdSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::DynastreamOem) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageFileIdSubfieldProduct::GarminProduct(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
        Ok((FitMessageFileIdSubfieldProduct::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageFileId<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub ftype: Option<FitFieldFile>,  
    pub manufacturer: Option<FitFieldManufacturer>,  
    pub product: Option<FitMessageFileIdSubfieldProduct>,  
    pub serial_number: Option<u32>,  
    pub time_created: Option<FitFieldDateTime>,  // Only set for files that are can be created/erased.
    pub number: Option<u16>,  // Only set for files that are not created/erased.
    pub product_name: Option<String>,  // Optional free form string to indicate the devices name or model
    
}
impl<'a> FitMessageFileId<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageFileId<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageFileId {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            ftype: None,
            manufacturer: None,
            product: None,
            serial_number: None,
            time_created: None,
            number: None,
            product_name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageFileId::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageFileId:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageFileId<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // ftype
                    let (val, outp) = FitFieldFile::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                1 => {  // manufacturer
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer = Some(val);
                    Ok(())
                },
            
                2 => {  // product
                    let (val, outp) = FitMessageFileIdSubfieldProduct::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.product = Some(val);
                    Ok(())
                },
            
                3 => {  // serial_number
                    let (val, outp) = parse_uint32z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.serial_number = val;
                    Ok(())
                },
            
                4 => {  // time_created
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_created = Some(val);
                    Ok(())
                },
            
                5 => {  // number
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.number = Some(val);
                    Ok(())
                },
            
                8 => {  // product_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.product_name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageGoal<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub sport: Option<FitFieldSport>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    pub start_date: Option<FitFieldDateTime>,  
    pub end_date: Option<FitFieldDateTime>,  
    pub ftype: Option<FitFieldGoal>,  
    pub value: Option<u32>,  
    pub repeat: Option<bool>,  
    pub target_value: Option<u32>,  
    pub recurrence: Option<FitFieldGoalRecurrence>,  
    pub recurrence_value: Option<u16>,  
    pub enabled: Option<bool>,  
    pub source: Option<FitFieldGoalSource>,  
    
}
impl<'a> FitMessageGoal<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageGoal<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageGoal {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            sport: None,
            sub_sport: None,
            start_date: None,
            end_date: None,
            ftype: None,
            value: None,
            repeat: None,
            target_value: None,
            recurrence: None,
            recurrence_value: None,
            enabled: None,
            source: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageGoal::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageGoal:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageGoal<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                1 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                2 => {  // start_date
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_date = Some(val);
                    Ok(())
                },
            
                3 => {  // end_date
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_date = Some(val);
                    Ok(())
                },
            
                4 => {  // ftype
                    let (val, outp) = FitFieldGoal::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                5 => {  // value
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.value = Some(val);
                    Ok(())
                },
            
                6 => {  // repeat
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.repeat = Some(val);
                    Ok(())
                },
            
                7 => {  // target_value
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.target_value = Some(val);
                    Ok(())
                },
            
                8 => {  // recurrence
                    let (val, outp) = FitFieldGoalRecurrence::parse(inp)?;
                    inp = outp;
                    message.recurrence = Some(val);
                    Ok(())
                },
            
                9 => {  // recurrence_value
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.recurrence_value = Some(val);
                    Ok(())
                },
            
                10 => {  // enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                11 => {  // source
                    let (val, outp) = FitFieldGoalSource::parse(inp)?;
                    inp = outp;
                    message.source = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageGpsMetadata<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp.
    pub timestamp_ms: Option<u16>,  // Millisecond part of the timestamp.
    pub position_lat: Option<i32>,  
    pub position_long: Option<i32>,  
    pub enhanced_altitude: Option<f64>,  
    pub enhanced_speed: Option<f64>,  
    pub heading: Option<f64>,  
    pub utc_timestamp: Option<FitFieldDateTime>,  // Used to correlate UTC to system time if the timestamp of the message is in system time.  This UTC time is derived from the GPS data.
    pub velocity: Option<f64>,  // velocity[0] is lon velocity.  Velocity[1] is lat velocity.  Velocity[2] is altitude velocity.
    
}
impl<'a> FitMessageGpsMetadata<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageGpsMetadata<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageGpsMetadata {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            position_lat: None,
            position_long: None,
            enhanced_altitude: None,
            enhanced_speed: None,
            heading: None,
            utc_timestamp: None,
            velocity: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageGpsMetadata::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageGpsMetadata:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageGpsMetadata<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_lat = Some(val);
                    Ok(())
                },
            
                2 => {  // position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_long = Some(val);
                    Ok(())
                },
            
                3 => {  // enhanced_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                4 => {  // enhanced_speed
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                5 => {  // heading
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.heading = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                6 => {  // utc_timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.utc_timestamp = Some(val);
                    Ok(())
                },
            
                7 => {  // velocity
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.velocity = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageGyroscopeData<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp
    pub timestamp_ms: Option<u16>,  // Millisecond part of the timestamp.
    pub sample_time_offset: Option<u16>,  // Each time in the array describes the time at which the gyro sample with the corrosponding index was taken. Limited to 30 samples in each message. The samples may span across seconds. Array size must match the number of samples in gyro_x and gyro_y and gyro_z
    pub gyro_x: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub gyro_y: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub gyro_z: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub calibrated_gyro_x: Option<f32>,  // Calibrated gyro reading
    pub calibrated_gyro_y: Option<f32>,  // Calibrated gyro reading
    pub calibrated_gyro_z: Option<f32>,  // Calibrated gyro reading
    
}
impl<'a> FitMessageGyroscopeData<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageGyroscopeData<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageGyroscopeData {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            sample_time_offset: None,
            gyro_x: None,
            gyro_y: None,
            gyro_z: None,
            calibrated_gyro_x: None,
            calibrated_gyro_y: None,
            calibrated_gyro_z: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageGyroscopeData::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageGyroscopeData:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageGyroscopeData<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // sample_time_offset
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sample_time_offset = Some(val);
                    Ok(())
                },
            
                2 => {  // gyro_x
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.gyro_x = Some(val);
                    Ok(())
                },
            
                3 => {  // gyro_y
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.gyro_y = Some(val);
                    Ok(())
                },
            
                4 => {  // gyro_z
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.gyro_z = Some(val);
                    Ok(())
                },
            
                5 => {  // calibrated_gyro_x
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_gyro_x = Some(val);
                    Ok(())
                },
            
                6 => {  // calibrated_gyro_y
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_gyro_y = Some(val);
                    Ok(())
                },
            
                7 => {  // calibrated_gyro_z
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_gyro_z = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageHr<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub fractional_timestamp: Option<f64>,  
    pub time256: Option<f64>,  
    pub filtered_bpm: Option<u8>,  
    pub event_timestamp: Option<f64>,  
    pub event_timestamp_12: Option<&'a [u8]>,  
    
}
impl<'a> FitMessageHr<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageHr<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageHr {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            fractional_timestamp: None,
            time256: None,
            filtered_bpm: None,
            event_timestamp: None,
            event_timestamp_12: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageHr::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageHr:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageHr<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // fractional_timestamp
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.fractional_timestamp = Some(val as f64 / 32768 as f64);
                    Ok(())
                },
            
                1 => {  // time256
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.time256 = Some(val as f64 / 256 as f64);
                    Ok(())
                },
            
                6 => {  // filtered_bpm
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.filtered_bpm = Some(val);
                    Ok(())
                },
            
                9 => {  // event_timestamp
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.event_timestamp = Some(val as f64 / 1024 as f64);
                    Ok(())
                },
            
                10 => {  // event_timestamp_12
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.event_timestamp_12 = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageHrZone<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub high_bpm: Option<u8>,  
    pub name: Option<String>,  
    
}
impl<'a> FitMessageHrZone<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageHrZone<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageHrZone {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            high_bpm: None,
            name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageHrZone::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageHrZone:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageHrZone<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                1 => {  // high_bpm
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.high_bpm = Some(val);
                    Ok(())
                },
            
                2 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageHrmProfile<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub enabled: Option<bool>,  
    pub hrm_ant_id: Option<u16>,  
    pub log_hrv: Option<bool>,  
    pub hrm_ant_id_trans_type: Option<u8>,  
    
}
impl<'a> FitMessageHrmProfile<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageHrmProfile<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageHrmProfile {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            enabled: None,
            hrm_ant_id: None,
            log_hrv: None,
            hrm_ant_id_trans_type: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageHrmProfile::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageHrmProfile:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageHrmProfile<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                1 => {  // hrm_ant_id
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.hrm_ant_id = val;
                    Ok(())
                },
            
                2 => {  // log_hrv
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.log_hrv = Some(val);
                    Ok(())
                },
            
                3 => {  // hrm_ant_id_trans_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.hrm_ant_id_trans_type = val;
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageHrv<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub time: Option<f64>,  // Time between beats
    
}
impl<'a> FitMessageHrv<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageHrv<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageHrv {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            time: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageHrv::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageHrv:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageHrv<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // time
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageLapSubfieldTotalCycles {
    Default(u32),
    TotalStrides(u32),
}

impl FitMessageLapSubfieldTotalCycles {
    fn parse<'a>(message: &FitMessageLap<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageLapSubfieldTotalCycles,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Running) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageLapSubfieldTotalCycles::TotalStrides(val), o))
            },
        
            Some(FitFieldSport::Walking) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageLapSubfieldTotalCycles::TotalStrides(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageLapSubfieldTotalCycles::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageLapSubfieldAvgCadence {
    Default(u8),
    AvgRunningCadence(u8),
}

impl FitMessageLapSubfieldAvgCadence {
    fn parse<'a>(message: &FitMessageLap<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageLapSubfieldAvgCadence,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Running) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageLapSubfieldAvgCadence::AvgRunningCadence(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint8(inp)?;
        Ok((FitMessageLapSubfieldAvgCadence::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageLapSubfieldMaxCadence {
    Default(u8),
    MaxRunningCadence(u8),
}

impl FitMessageLapSubfieldMaxCadence {
    fn parse<'a>(message: &FitMessageLap<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageLapSubfieldMaxCadence,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Running) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageLapSubfieldMaxCadence::MaxRunningCadence(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint8(inp)?;
        Ok((FitMessageLapSubfieldMaxCadence::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageLap<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub timestamp: Option<FitFieldDateTime>,  // Lap end time.
    pub event: Option<FitFieldEvent>,  
    pub event_type: Option<FitFieldEventType>,  
    pub start_time: Option<FitFieldDateTime>,  
    pub start_position_lat: Option<i32>,  
    pub start_position_long: Option<i32>,  
    pub end_position_lat: Option<i32>,  
    pub end_position_long: Option<i32>,  
    pub total_elapsed_time: Option<f64>,  // Time (includes pauses)
    pub total_timer_time: Option<f64>,  // Timer Time (excludes pauses)
    pub total_distance: Option<f64>,  
    pub total_cycles: Option<FitMessageLapSubfieldTotalCycles>,  
    pub total_calories: Option<u16>,  
    pub total_fat_calories: Option<u16>,  // If New Leaf
    pub avg_speed: Option<f64>,  
    pub max_speed: Option<f64>,  
    pub avg_heart_rate: Option<u8>,  
    pub max_heart_rate: Option<u8>,  
    pub avg_cadence: Option<FitMessageLapSubfieldAvgCadence>,  // total_cycles / total_timer_time if non_zero_avg_cadence otherwise total_cycles / total_elapsed_time
    pub max_cadence: Option<FitMessageLapSubfieldMaxCadence>,  
    pub avg_power: Option<u16>,  // total_power / total_timer_time if non_zero_avg_power otherwise total_power / total_elapsed_time
    pub max_power: Option<u16>,  
    pub total_ascent: Option<u16>,  
    pub total_descent: Option<u16>,  
    pub intensity: Option<FitFieldIntensity>,  
    pub lap_trigger: Option<FitFieldLapTrigger>,  
    pub sport: Option<FitFieldSport>,  
    pub event_group: Option<u8>,  
    pub num_lengths: Option<u16>,  // # of lengths of swim pool
    pub normalized_power: Option<u16>,  
    pub left_right_balance: Option<FitFieldLeftRightBalance100>,  
    pub first_length_index: Option<u16>,  
    pub avg_stroke_distance: Option<f64>,  
    pub swim_stroke: Option<FitFieldSwimStroke>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    pub num_active_lengths: Option<u16>,  // # of active lengths of swim pool
    pub total_work: Option<u32>,  
    pub avg_altitude: Option<f64>,  
    pub max_altitude: Option<f64>,  
    pub gps_accuracy: Option<u8>,  
    pub avg_grade: Option<f64>,  
    pub avg_pos_grade: Option<f64>,  
    pub avg_neg_grade: Option<f64>,  
    pub max_pos_grade: Option<f64>,  
    pub max_neg_grade: Option<f64>,  
    pub avg_temperature: Option<i8>,  
    pub max_temperature: Option<i8>,  
    pub total_moving_time: Option<f64>,  
    pub avg_pos_vertical_speed: Option<f64>,  
    pub avg_neg_vertical_speed: Option<f64>,  
    pub max_pos_vertical_speed: Option<f64>,  
    pub max_neg_vertical_speed: Option<f64>,  
    pub time_in_hr_zone: Option<f64>,  
    pub time_in_speed_zone: Option<f64>,  
    pub time_in_cadence_zone: Option<f64>,  
    pub time_in_power_zone: Option<f64>,  
    pub repetition_num: Option<u16>,  
    pub min_altitude: Option<f64>,  
    pub min_heart_rate: Option<u8>,  
    pub wkt_step_index: Option<FitFieldMessageIndex>,  
    pub opponent_score: Option<u16>,  
    pub stroke_count: Option<u16>,  // stroke_type enum used as the index
    pub zone_count: Option<u16>,  // zone number used as the index
    pub avg_vertical_oscillation: Option<f64>,  
    pub avg_stance_time_percent: Option<f64>,  
    pub avg_stance_time: Option<f64>,  
    pub avg_fractional_cadence: Option<f64>,  // fractional part of the avg_cadence
    pub max_fractional_cadence: Option<f64>,  // fractional part of the max_cadence
    pub total_fractional_cycles: Option<f64>,  // fractional part of the total_cycles
    pub player_score: Option<u16>,  
    pub avg_total_hemoglobin_conc: Option<f64>,  // Avg saturated and unsaturated hemoglobin
    pub min_total_hemoglobin_conc: Option<f64>,  // Min saturated and unsaturated hemoglobin
    pub max_total_hemoglobin_conc: Option<f64>,  // Max saturated and unsaturated hemoglobin
    pub avg_saturated_hemoglobin_percent: Option<f64>,  // Avg percentage of hemoglobin saturated with oxygen
    pub min_saturated_hemoglobin_percent: Option<f64>,  // Min percentage of hemoglobin saturated with oxygen
    pub max_saturated_hemoglobin_percent: Option<f64>,  // Max percentage of hemoglobin saturated with oxygen
    pub avg_left_torque_effectiveness: Option<f64>,  
    pub avg_right_torque_effectiveness: Option<f64>,  
    pub avg_left_pedal_smoothness: Option<f64>,  
    pub avg_right_pedal_smoothness: Option<f64>,  
    pub avg_combined_pedal_smoothness: Option<f64>,  
    pub time_standing: Option<f64>,  // Total time spent in the standing position
    pub stand_count: Option<u16>,  // Number of transitions to the standing state
    pub avg_left_pco: Option<i8>,  // Average left platform center offset
    pub avg_right_pco: Option<i8>,  // Average right platform center offset
    pub avg_left_power_phase: Option<f64>,  // Average left power phase angles. Data value indexes defined by power_phase_type.
    pub avg_left_power_phase_peak: Option<f64>,  // Average left power phase peak angles. Data value indexes  defined by power_phase_type.
    pub avg_right_power_phase: Option<f64>,  // Average right power phase angles. Data value indexes defined by power_phase_type.
    pub avg_right_power_phase_peak: Option<f64>,  // Average right power phase peak angles. Data value indexes  defined by power_phase_type.
    pub avg_power_position: Option<u16>,  // Average power by position. Data value indexes defined by rider_position_type.
    pub max_power_position: Option<u16>,  // Maximum power by position. Data value indexes defined by rider_position_type.
    pub avg_cadence_position: Option<u8>,  // Average cadence by position. Data value indexes defined by rider_position_type.
    pub max_cadence_position: Option<u8>,  // Maximum cadence by position. Data value indexes defined by rider_position_type.
    pub enhanced_avg_speed: Option<f64>,  
    pub enhanced_max_speed: Option<f64>,  
    pub enhanced_avg_altitude: Option<f64>,  
    pub enhanced_min_altitude: Option<f64>,  
    pub enhanced_max_altitude: Option<f64>,  
    pub avg_lev_motor_power: Option<u16>,  // lev average motor power during lap
    pub max_lev_motor_power: Option<u16>,  // lev maximum motor power during lap
    pub lev_battery_consumption: Option<f64>,  // lev battery consumption during lap
    pub avg_vertical_ratio: Option<f64>,  
    pub avg_stance_time_balance: Option<f64>,  
    pub avg_step_length: Option<f64>,  
    
}
impl<'a> FitMessageLap<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageLap<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageLap {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            timestamp: None,
            event: None,
            event_type: None,
            start_time: None,
            start_position_lat: None,
            start_position_long: None,
            end_position_lat: None,
            end_position_long: None,
            total_elapsed_time: None,
            total_timer_time: None,
            total_distance: None,
            total_cycles: None,
            total_calories: None,
            total_fat_calories: None,
            avg_speed: None,
            max_speed: None,
            avg_heart_rate: None,
            max_heart_rate: None,
            avg_cadence: None,
            max_cadence: None,
            avg_power: None,
            max_power: None,
            total_ascent: None,
            total_descent: None,
            intensity: None,
            lap_trigger: None,
            sport: None,
            event_group: None,
            num_lengths: None,
            normalized_power: None,
            left_right_balance: None,
            first_length_index: None,
            avg_stroke_distance: None,
            swim_stroke: None,
            sub_sport: None,
            num_active_lengths: None,
            total_work: None,
            avg_altitude: None,
            max_altitude: None,
            gps_accuracy: None,
            avg_grade: None,
            avg_pos_grade: None,
            avg_neg_grade: None,
            max_pos_grade: None,
            max_neg_grade: None,
            avg_temperature: None,
            max_temperature: None,
            total_moving_time: None,
            avg_pos_vertical_speed: None,
            avg_neg_vertical_speed: None,
            max_pos_vertical_speed: None,
            max_neg_vertical_speed: None,
            time_in_hr_zone: None,
            time_in_speed_zone: None,
            time_in_cadence_zone: None,
            time_in_power_zone: None,
            repetition_num: None,
            min_altitude: None,
            min_heart_rate: None,
            wkt_step_index: None,
            opponent_score: None,
            stroke_count: None,
            zone_count: None,
            avg_vertical_oscillation: None,
            avg_stance_time_percent: None,
            avg_stance_time: None,
            avg_fractional_cadence: None,
            max_fractional_cadence: None,
            total_fractional_cycles: None,
            player_score: None,
            avg_total_hemoglobin_conc: None,
            min_total_hemoglobin_conc: None,
            max_total_hemoglobin_conc: None,
            avg_saturated_hemoglobin_percent: None,
            min_saturated_hemoglobin_percent: None,
            max_saturated_hemoglobin_percent: None,
            avg_left_torque_effectiveness: None,
            avg_right_torque_effectiveness: None,
            avg_left_pedal_smoothness: None,
            avg_right_pedal_smoothness: None,
            avg_combined_pedal_smoothness: None,
            time_standing: None,
            stand_count: None,
            avg_left_pco: None,
            avg_right_pco: None,
            avg_left_power_phase: None,
            avg_left_power_phase_peak: None,
            avg_right_power_phase: None,
            avg_right_power_phase_peak: None,
            avg_power_position: None,
            max_power_position: None,
            avg_cadence_position: None,
            max_cadence_position: None,
            enhanced_avg_speed: None,
            enhanced_max_speed: None,
            enhanced_avg_altitude: None,
            enhanced_min_altitude: None,
            enhanced_max_altitude: None,
            avg_lev_motor_power: None,
            max_lev_motor_power: None,
            lev_battery_consumption: None,
            avg_vertical_ratio: None,
            avg_stance_time_balance: None,
            avg_step_length: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageLap::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageLap:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageLap<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // event
                    let (val, outp) = FitFieldEvent::parse(inp)?;
                    inp = outp;
                    message.event = Some(val);
                    Ok(())
                },
            
                1 => {  // event_type
                    let (val, outp) = FitFieldEventType::parse(inp)?;
                    inp = outp;
                    message.event_type = Some(val);
                    Ok(())
                },
            
                2 => {  // start_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_time = Some(val);
                    Ok(())
                },
            
                3 => {  // start_position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_position_lat = Some(val);
                    Ok(())
                },
            
                4 => {  // start_position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_position_long = Some(val);
                    Ok(())
                },
            
                5 => {  // end_position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_position_lat = Some(val);
                    Ok(())
                },
            
                6 => {  // end_position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_position_long = Some(val);
                    Ok(())
                },
            
                7 => {  // total_elapsed_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_elapsed_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                8 => {  // total_timer_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_timer_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                9 => {  // total_distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                10 => {  // total_cycles
                    let (val, outp) = FitMessageLapSubfieldTotalCycles::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.total_cycles = Some(val);
                    Ok(())
                },
            
                11 => {  // total_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_calories = Some(val);
                    Ok(())
                },
            
                12 => {  // total_fat_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_fat_calories = Some(val);
                    Ok(())
                },
            
                13 => {  // avg_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                14 => {  // max_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                15 => {  // avg_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_heart_rate = Some(val);
                    Ok(())
                },
            
                16 => {  // max_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_heart_rate = Some(val);
                    Ok(())
                },
            
                17 => {  // avg_cadence
                    let (val, outp) = FitMessageLapSubfieldAvgCadence::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.avg_cadence = Some(val);
                    Ok(())
                },
            
                18 => {  // max_cadence
                    let (val, outp) = FitMessageLapSubfieldMaxCadence::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.max_cadence = Some(val);
                    Ok(())
                },
            
                19 => {  // avg_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_power = Some(val);
                    Ok(())
                },
            
                20 => {  // max_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_power = Some(val);
                    Ok(())
                },
            
                21 => {  // total_ascent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_ascent = Some(val);
                    Ok(())
                },
            
                22 => {  // total_descent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_descent = Some(val);
                    Ok(())
                },
            
                23 => {  // intensity
                    let (val, outp) = FitFieldIntensity::parse(inp)?;
                    inp = outp;
                    message.intensity = Some(val);
                    Ok(())
                },
            
                24 => {  // lap_trigger
                    let (val, outp) = FitFieldLapTrigger::parse(inp)?;
                    inp = outp;
                    message.lap_trigger = Some(val);
                    Ok(())
                },
            
                25 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                26 => {  // event_group
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.event_group = Some(val);
                    Ok(())
                },
            
                32 => {  // num_lengths
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.num_lengths = Some(val);
                    Ok(())
                },
            
                33 => {  // normalized_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.normalized_power = Some(val);
                    Ok(())
                },
            
                34 => {  // left_right_balance
                    let (val, outp) = FitFieldLeftRightBalance100::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.left_right_balance = Some(val);
                    Ok(())
                },
            
                35 => {  // first_length_index
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.first_length_index = Some(val);
                    Ok(())
                },
            
                37 => {  // avg_stroke_distance
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stroke_distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                38 => {  // swim_stroke
                    let (val, outp) = FitFieldSwimStroke::parse(inp)?;
                    inp = outp;
                    message.swim_stroke = Some(val);
                    Ok(())
                },
            
                39 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                40 => {  // num_active_lengths
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.num_active_lengths = Some(val);
                    Ok(())
                },
            
                41 => {  // total_work
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_work = Some(val);
                    Ok(())
                },
            
                42 => {  // avg_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                43 => {  // max_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                44 => {  // gps_accuracy
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.gps_accuracy = Some(val);
                    Ok(())
                },
            
                45 => {  // avg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                46 => {  // avg_pos_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_pos_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                47 => {  // avg_neg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_neg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                48 => {  // max_pos_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_pos_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                49 => {  // max_neg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_neg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                50 => {  // avg_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_temperature = Some(val);
                    Ok(())
                },
            
                51 => {  // max_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.max_temperature = Some(val);
                    Ok(())
                },
            
                52 => {  // total_moving_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_moving_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                53 => {  // avg_pos_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_pos_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                54 => {  // avg_neg_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_neg_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                55 => {  // max_pos_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_pos_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                56 => {  // max_neg_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_neg_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                57 => {  // time_in_hr_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_hr_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                58 => {  // time_in_speed_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_speed_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                59 => {  // time_in_cadence_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_cadence_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                60 => {  // time_in_power_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_power_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                61 => {  // repetition_num
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.repetition_num = Some(val);
                    Ok(())
                },
            
                62 => {  // min_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                63 => {  // min_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.min_heart_rate = Some(val);
                    Ok(())
                },
            
                71 => {  // wkt_step_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.wkt_step_index = Some(val);
                    Ok(())
                },
            
                74 => {  // opponent_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.opponent_score = Some(val);
                    Ok(())
                },
            
                75 => {  // stroke_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stroke_count = Some(val);
                    Ok(())
                },
            
                76 => {  // zone_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.zone_count = Some(val);
                    Ok(())
                },
            
                77 => {  // avg_vertical_oscillation
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_vertical_oscillation = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                78 => {  // avg_stance_time_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stance_time_percent = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                79 => {  // avg_stance_time
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stance_time = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                80 => {  // avg_fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                81 => {  // max_fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                82 => {  // total_fractional_cycles
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.total_fractional_cycles = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                83 => {  // player_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.player_score = Some(val);
                    Ok(())
                },
            
                84 => {  // avg_total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                85 => {  // min_total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                86 => {  // max_total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                87 => {  // avg_saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                88 => {  // min_saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                89 => {  // max_saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                91 => {  // avg_left_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                92 => {  // avg_right_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                93 => {  // avg_left_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                94 => {  // avg_right_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                95 => {  // avg_combined_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_combined_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                98 => {  // time_standing
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_standing = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                99 => {  // stand_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stand_count = Some(val);
                    Ok(())
                },
            
                100 => {  // avg_left_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_left_pco = Some(val);
                    Ok(())
                },
            
                101 => {  // avg_right_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_right_pco = Some(val);
                    Ok(())
                },
            
                102 => {  // avg_left_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                103 => {  // avg_left_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                104 => {  // avg_right_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                105 => {  // avg_right_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                106 => {  // avg_power_position
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_power_position = Some(val);
                    Ok(())
                },
            
                107 => {  // max_power_position
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_power_position = Some(val);
                    Ok(())
                },
            
                108 => {  // avg_cadence_position
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_cadence_position = Some(val);
                    Ok(())
                },
            
                109 => {  // max_cadence_position
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_cadence_position = Some(val);
                    Ok(())
                },
            
                110 => {  // enhanced_avg_speed
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_avg_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                111 => {  // enhanced_max_speed
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_max_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                112 => {  // enhanced_avg_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_avg_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                113 => {  // enhanced_min_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_min_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                114 => {  // enhanced_max_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_max_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                115 => {  // avg_lev_motor_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_lev_motor_power = Some(val);
                    Ok(())
                },
            
                116 => {  // max_lev_motor_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_lev_motor_power = Some(val);
                    Ok(())
                },
            
                117 => {  // lev_battery_consumption
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.lev_battery_consumption = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                118 => {  // avg_vertical_ratio
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_vertical_ratio = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                119 => {  // avg_stance_time_balance
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stance_time_balance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                120 => {  // avg_step_length
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_step_length = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageLength<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub timestamp: Option<FitFieldDateTime>,  
    pub event: Option<FitFieldEvent>,  
    pub event_type: Option<FitFieldEventType>,  
    pub start_time: Option<FitFieldDateTime>,  
    pub total_elapsed_time: Option<f64>,  
    pub total_timer_time: Option<f64>,  
    pub total_strokes: Option<u16>,  
    pub avg_speed: Option<f64>,  
    pub swim_stroke: Option<FitFieldSwimStroke>,  
    pub avg_swimming_cadence: Option<u8>,  
    pub event_group: Option<u8>,  
    pub total_calories: Option<u16>,  
    pub length_type: Option<FitFieldLengthType>,  
    pub player_score: Option<u16>,  
    pub opponent_score: Option<u16>,  
    pub stroke_count: Option<u16>,  // stroke_type enum used as the index
    pub zone_count: Option<u16>,  // zone number used as the index
    
}
impl<'a> FitMessageLength<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageLength<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageLength {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            timestamp: None,
            event: None,
            event_type: None,
            start_time: None,
            total_elapsed_time: None,
            total_timer_time: None,
            total_strokes: None,
            avg_speed: None,
            swim_stroke: None,
            avg_swimming_cadence: None,
            event_group: None,
            total_calories: None,
            length_type: None,
            player_score: None,
            opponent_score: None,
            stroke_count: None,
            zone_count: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageLength::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageLength:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageLength<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // event
                    let (val, outp) = FitFieldEvent::parse(inp)?;
                    inp = outp;
                    message.event = Some(val);
                    Ok(())
                },
            
                1 => {  // event_type
                    let (val, outp) = FitFieldEventType::parse(inp)?;
                    inp = outp;
                    message.event_type = Some(val);
                    Ok(())
                },
            
                2 => {  // start_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_time = Some(val);
                    Ok(())
                },
            
                3 => {  // total_elapsed_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_elapsed_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                4 => {  // total_timer_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_timer_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                5 => {  // total_strokes
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_strokes = Some(val);
                    Ok(())
                },
            
                6 => {  // avg_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                7 => {  // swim_stroke
                    let (val, outp) = FitFieldSwimStroke::parse(inp)?;
                    inp = outp;
                    message.swim_stroke = Some(val);
                    Ok(())
                },
            
                9 => {  // avg_swimming_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_swimming_cadence = Some(val);
                    Ok(())
                },
            
                10 => {  // event_group
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.event_group = Some(val);
                    Ok(())
                },
            
                11 => {  // total_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_calories = Some(val);
                    Ok(())
                },
            
                12 => {  // length_type
                    let (val, outp) = FitFieldLengthType::parse(inp)?;
                    inp = outp;
                    message.length_type = Some(val);
                    Ok(())
                },
            
                18 => {  // player_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.player_score = Some(val);
                    Ok(())
                },
            
                19 => {  // opponent_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.opponent_score = Some(val);
                    Ok(())
                },
            
                20 => {  // stroke_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stroke_count = Some(val);
                    Ok(())
                },
            
                21 => {  // zone_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.zone_count = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageMagnetometerData<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp
    pub timestamp_ms: Option<u16>,  // Millisecond part of the timestamp.
    pub sample_time_offset: Option<u16>,  // Each time in the array describes the time at which the compass sample with the corrosponding index was taken. Limited to 30 samples in each message. The samples may span across seconds. Array size must match the number of samples in cmps_x and cmps_y and cmps_z
    pub mag_x: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub mag_y: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub mag_z: Option<u16>,  // These are the raw ADC reading. Maximum number of samples is 30 in each message. The samples may span across seconds. A conversion will need to be done on this data once read.
    pub calibrated_mag_x: Option<f32>,  // Calibrated Magnetometer reading
    pub calibrated_mag_y: Option<f32>,  // Calibrated Magnetometer reading
    pub calibrated_mag_z: Option<f32>,  // Calibrated Magnetometer reading
    
}
impl<'a> FitMessageMagnetometerData<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageMagnetometerData<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageMagnetometerData {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            sample_time_offset: None,
            mag_x: None,
            mag_y: None,
            mag_z: None,
            calibrated_mag_x: None,
            calibrated_mag_y: None,
            calibrated_mag_z: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageMagnetometerData::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageMagnetometerData:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageMagnetometerData<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // sample_time_offset
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sample_time_offset = Some(val);
                    Ok(())
                },
            
                2 => {  // mag_x
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.mag_x = Some(val);
                    Ok(())
                },
            
                3 => {  // mag_y
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.mag_y = Some(val);
                    Ok(())
                },
            
                4 => {  // mag_z
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.mag_z = Some(val);
                    Ok(())
                },
            
                5 => {  // calibrated_mag_x
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_mag_x = Some(val);
                    Ok(())
                },
            
                6 => {  // calibrated_mag_y
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_mag_y = Some(val);
                    Ok(())
                },
            
                7 => {  // calibrated_mag_z
                    let (val, outp) = parse_float32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibrated_mag_z = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageMemoGlob<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub part_index: Option<u32>,  // Sequence number of memo blocks
    pub memo: Option<&'a [u8]>,  // Block of utf8 bytes 
    pub message_number: Option<u16>,  // Allows relating glob to another mesg  If used only required for first part of each memo_glob
    pub message_index: Option<FitFieldMessageIndex>,  // Index of external mesg
    
}
impl<'a> FitMessageMemoGlob<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageMemoGlob<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageMemoGlob {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            part_index: None,
            memo: None,
            message_number: None,
            message_index: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageMemoGlob::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageMemoGlob:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageMemoGlob<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                250 => {  // part_index
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.part_index = Some(val);
                    Ok(())
                },
            
                0 => {  // memo
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.memo = Some(val);
                    Ok(())
                },
            
                1 => {  // message_number
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_number = Some(val);
                    Ok(())
                },
            
                2 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageMesgCapabilitiesSubfieldCount {
    Default(u16),
    MaxPerFile(u16),
    MaxPerFileType(u16),
    NumPerFile(u16),
}

impl FitMessageMesgCapabilitiesSubfieldCount {
    fn parse<'a>(message: &FitMessageMesgCapabilities<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageMesgCapabilitiesSubfieldCount,  &'a [u8])> {
        
        match message.count_type {
        
            Some(FitFieldMesgCount::NumPerFile) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMesgCapabilitiesSubfieldCount::NumPerFile(val), o))
            },
        
            Some(FitFieldMesgCount::MaxPerFile) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMesgCapabilitiesSubfieldCount::MaxPerFile(val), o))
            },
        
            Some(FitFieldMesgCount::MaxPerFileType) => {
                let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMesgCapabilitiesSubfieldCount::MaxPerFileType(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
        Ok((FitMessageMesgCapabilitiesSubfieldCount::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageMesgCapabilities<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub file: Option<FitFieldFile>,  
    pub mesg_num: Option<FitFieldMesgNum>,  
    pub count_type: Option<FitFieldMesgCount>,  
    pub count: Option<FitMessageMesgCapabilitiesSubfieldCount>,  
    
}
impl<'a> FitMessageMesgCapabilities<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageMesgCapabilities<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageMesgCapabilities {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            file: None,
            mesg_num: None,
            count_type: None,
            count: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageMesgCapabilities::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageMesgCapabilities:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageMesgCapabilities<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // file
                    let (val, outp) = FitFieldFile::parse(inp)?;
                    inp = outp;
                    message.file = Some(val);
                    Ok(())
                },
            
                1 => {  // mesg_num
                    let (val, outp) = FitFieldMesgNum::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.mesg_num = Some(val);
                    Ok(())
                },
            
                2 => {  // count_type
                    let (val, outp) = FitFieldMesgCount::parse(inp)?;
                    inp = outp;
                    message.count_type = Some(val);
                    Ok(())
                },
            
                3 => {  // count
                    let (val, outp) = FitMessageMesgCapabilitiesSubfieldCount::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.count = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageMetZone<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub high_bpm: Option<u8>,  
    pub calories: Option<f64>,  
    pub fat_calories: Option<f64>,  
    
}
impl<'a> FitMessageMetZone<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageMetZone<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageMetZone {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            high_bpm: None,
            calories: None,
            fat_calories: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageMetZone::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageMetZone:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageMetZone<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                1 => {  // high_bpm
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.high_bpm = Some(val);
                    Ok(())
                },
            
                2 => {  // calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calories = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                3 => {  // fat_calories
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.fat_calories = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageMonitoringSubfieldCycles {
    Default(u32),
    Steps(u32),
    Strokes(u32),
}

impl FitMessageMonitoringSubfieldCycles {
    fn parse<'a>(message: &FitMessageMonitoring<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageMonitoringSubfieldCycles,  &'a [u8])> {
        
        match message.activity_type {
        
            Some(FitFieldActivityType::Walking) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMonitoringSubfieldCycles::Steps(val), o))
            },
        
            Some(FitFieldActivityType::Running) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMonitoringSubfieldCycles::Steps(val), o))
            },
        
            Some(FitFieldActivityType::Cycling) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMonitoringSubfieldCycles::Strokes(val), o))
            },
        
            Some(FitFieldActivityType::Swimming) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageMonitoringSubfieldCycles::Strokes(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageMonitoringSubfieldCycles::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageMonitoring<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Must align to logging interval, for example, time must be 00:00:00 for daily log.
    pub device_index: Option<FitFieldDeviceIndex>,  // Associates this data to device_info message.  Not required for file with single device (sensor).
    pub calories: Option<u16>,  // Accumulated total calories.  Maintained by MonitoringReader for each activity_type.  See SDK documentation
    pub distance: Option<f64>,  // Accumulated distance.  Maintained by MonitoringReader for each activity_type.  See SDK documentation.
    pub cycles: Option<FitMessageMonitoringSubfieldCycles>,  // Accumulated cycles.  Maintained by MonitoringReader for each activity_type.  See SDK documentation.
    pub active_time: Option<f64>,  
    pub activity_type: Option<FitFieldActivityType>,  
    pub activity_subtype: Option<FitFieldActivitySubtype>,  
    pub activity_level: Option<FitFieldActivityLevel>,  
    pub distance_16: Option<u16>,  
    pub cycles_16: Option<u16>,  
    pub active_time_16: Option<u16>,  
    pub local_timestamp: Option<FitFieldLocalDateTime>,  // Must align to logging interval, for example, time must be 00:00:00 for daily log.
    pub temperature: Option<f64>,  // Avg temperature during the logging interval ended at timestamp
    pub temperature_min: Option<f64>,  // Min temperature during the logging interval ended at timestamp
    pub temperature_max: Option<f64>,  // Max temperature during the logging interval ended at timestamp
    pub activity_time: Option<u16>,  // Indexed using minute_activity_level enum
    pub active_calories: Option<u16>,  
    pub current_activity_type_intensity: Option<&'a [u8]>,  // Indicates single type / intensity for duration since last monitoring message.
    pub timestamp_min_8: Option<u8>,  
    pub timestamp_16: Option<u16>,  
    pub heart_rate: Option<u8>,  
    pub intensity: Option<f64>,  
    pub duration_min: Option<u16>,  
    pub duration: Option<u32>,  
    pub ascent: Option<f64>,  
    pub descent: Option<f64>,  
    pub moderate_activity_minutes: Option<u16>,  
    pub vigorous_activity_minutes: Option<u16>,  
    
}
impl<'a> FitMessageMonitoring<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageMonitoring<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageMonitoring {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            device_index: None,
            calories: None,
            distance: None,
            cycles: None,
            active_time: None,
            activity_type: None,
            activity_subtype: None,
            activity_level: None,
            distance_16: None,
            cycles_16: None,
            active_time_16: None,
            local_timestamp: None,
            temperature: None,
            temperature_min: None,
            temperature_max: None,
            activity_time: None,
            active_calories: None,
            current_activity_type_intensity: None,
            timestamp_min_8: None,
            timestamp_16: None,
            heart_rate: None,
            intensity: None,
            duration_min: None,
            duration: None,
            ascent: None,
            descent: None,
            moderate_activity_minutes: None,
            vigorous_activity_minutes: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageMonitoring::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageMonitoring:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageMonitoring<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // device_index
                    let (val, outp) = FitFieldDeviceIndex::parse(inp)?;
                    inp = outp;
                    message.device_index = Some(val);
                    Ok(())
                },
            
                1 => {  // calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calories = Some(val);
                    Ok(())
                },
            
                2 => {  // distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                3 => {  // cycles
                    let (val, outp) = FitMessageMonitoringSubfieldCycles::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.cycles = Some(val);
                    Ok(())
                },
            
                4 => {  // active_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.active_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                5 => {  // activity_type
                    let (val, outp) = FitFieldActivityType::parse(inp)?;
                    inp = outp;
                    message.activity_type = Some(val);
                    Ok(())
                },
            
                6 => {  // activity_subtype
                    let (val, outp) = FitFieldActivitySubtype::parse(inp)?;
                    inp = outp;
                    message.activity_subtype = Some(val);
                    Ok(())
                },
            
                7 => {  // activity_level
                    let (val, outp) = FitFieldActivityLevel::parse(inp)?;
                    inp = outp;
                    message.activity_level = Some(val);
                    Ok(())
                },
            
                8 => {  // distance_16
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.distance_16 = Some(val);
                    Ok(())
                },
            
                9 => {  // cycles_16
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.cycles_16 = Some(val);
                    Ok(())
                },
            
                10 => {  // active_time_16
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.active_time_16 = Some(val);
                    Ok(())
                },
            
                11 => {  // local_timestamp
                    let (val, outp) = FitFieldLocalDateTime::parse(inp, message.definition_message.endianness, tz_offset)?;
                    inp = outp;
                    message.local_timestamp = Some(val);
                    Ok(())
                },
            
                12 => {  // temperature
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.temperature = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                14 => {  // temperature_min
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.temperature_min = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                15 => {  // temperature_max
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.temperature_max = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                16 => {  // activity_time
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.activity_time = Some(val);
                    Ok(())
                },
            
                19 => {  // active_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.active_calories = Some(val);
                    Ok(())
                },
            
                24 => {  // current_activity_type_intensity
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.current_activity_type_intensity = Some(val);
                    Ok(())
                },
            
                25 => {  // timestamp_min_8
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.timestamp_min_8 = Some(val);
                    Ok(())
                },
            
                26 => {  // timestamp_16
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_16 = Some(val);
                    Ok(())
                },
            
                27 => {  // heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.heart_rate = Some(val);
                    Ok(())
                },
            
                28 => {  // intensity
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.intensity = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                29 => {  // duration_min
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.duration_min = Some(val);
                    Ok(())
                },
            
                30 => {  // duration
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.duration = Some(val);
                    Ok(())
                },
            
                31 => {  // ascent
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.ascent = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                32 => {  // descent
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.descent = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                33 => {  // moderate_activity_minutes
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.moderate_activity_minutes = Some(val);
                    Ok(())
                },
            
                34 => {  // vigorous_activity_minutes
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.vigorous_activity_minutes = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageMonitoringInfo<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub local_timestamp: Option<FitFieldLocalDateTime>,  // Use to convert activity timestamps to local time if device does not support time zone and daylight savings time correction.
    pub activity_type: Option<FitFieldActivityType>,  
    pub cycles_to_distance: Option<f64>,  // Indexed by activity_type
    pub cycles_to_calories: Option<f64>,  // Indexed by activity_type
    pub resting_metabolic_rate: Option<u16>,  
    
}
impl<'a> FitMessageMonitoringInfo<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageMonitoringInfo<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageMonitoringInfo {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            local_timestamp: None,
            activity_type: None,
            cycles_to_distance: None,
            cycles_to_calories: None,
            resting_metabolic_rate: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageMonitoringInfo::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageMonitoringInfo:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageMonitoringInfo<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // local_timestamp
                    let (val, outp) = FitFieldLocalDateTime::parse(inp, message.definition_message.endianness, tz_offset)?;
                    inp = outp;
                    message.local_timestamp = Some(val);
                    Ok(())
                },
            
                1 => {  // activity_type
                    let (val, outp) = FitFieldActivityType::parse(inp)?;
                    inp = outp;
                    message.activity_type = Some(val);
                    Ok(())
                },
            
                3 => {  // cycles_to_distance
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.cycles_to_distance = Some(val as f64 / 5000 as f64);
                    Ok(())
                },
            
                4 => {  // cycles_to_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.cycles_to_calories = Some(val as f64 / 5000 as f64);
                    Ok(())
                },
            
                5 => {  // resting_metabolic_rate
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.resting_metabolic_rate = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageNmeaSentence<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Timestamp message was output
    pub timestamp_ms: Option<u16>,  // Fractional part of timestamp, added to timestamp
    pub sentence: Option<String>,  // NMEA sentence
    
}
impl<'a> FitMessageNmeaSentence<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageNmeaSentence<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageNmeaSentence {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            sentence: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageNmeaSentence::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageNmeaSentence:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageNmeaSentence<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // sentence
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.sentence = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageObdiiData<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Timestamp message was output
    pub timestamp_ms: Option<u16>,  // Fractional part of timestamp, added to timestamp
    pub time_offset: Option<u16>,  // Offset of PID reading [i] from start_timestamp+start_timestamp_ms. Readings may span accross seconds.
    pub pid: Option<&'a [u8]>,  // Parameter ID
    pub raw_data: Option<&'a [u8]>,  // Raw parameter data
    pub pid_data_size: Option<u8>,  // Optional, data size of PID[i].  If not specified refer to SAE J1979.
    pub system_time: Option<u32>,  // System time associated with sample expressed in ms, can be used instead of time_offset.  There will be a system_time value for each raw_data element.  For multibyte pids the system_time is repeated.
    pub start_timestamp: Option<FitFieldDateTime>,  // Timestamp of first sample recorded in the message.  Used with time_offset to generate time of each sample
    pub start_timestamp_ms: Option<u16>,  // Fractional part of start_timestamp
    
}
impl<'a> FitMessageObdiiData<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageObdiiData<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageObdiiData {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            time_offset: None,
            pid: None,
            raw_data: None,
            pid_data_size: None,
            system_time: None,
            start_timestamp: None,
            start_timestamp_ms: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageObdiiData::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageObdiiData:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageObdiiData<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // time_offset
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_offset = Some(val);
                    Ok(())
                },
            
                2 => {  // pid
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.pid = Some(val);
                    Ok(())
                },
            
                3 => {  // raw_data
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.raw_data = Some(val);
                    Ok(())
                },
            
                4 => {  // pid_data_size
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.pid_data_size = Some(val);
                    Ok(())
                },
            
                5 => {  // system_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.system_time = Some(val);
                    Ok(())
                },
            
                6 => {  // start_timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_timestamp = Some(val);
                    Ok(())
                },
            
                7 => {  // start_timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_timestamp_ms = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageOhrSettings<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub enabled: Option<FitFieldSwitch>,  
    
}
impl<'a> FitMessageOhrSettings<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageOhrSettings<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageOhrSettings {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            enabled: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageOhrSettings::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageOhrSettings:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageOhrSettings<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // enabled
                    let (val, outp) = FitFieldSwitch::parse(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessagePowerZone<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub high_value: Option<u16>,  
    pub name: Option<String>,  
    
}
impl<'a> FitMessagePowerZone<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessagePowerZone<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessagePowerZone {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            high_value: None,
            name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessagePowerZone::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessagePowerZone:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessagePowerZone<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                1 => {  // high_value
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.high_value = Some(val);
                    Ok(())
                },
            
                2 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageRecord<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub position_lat: Option<i32>,  
    pub position_long: Option<i32>,  
    pub altitude: Option<f64>,  
    pub heart_rate: Option<u8>,  
    pub cadence: Option<u8>,  
    pub distance: Option<f64>,  
    pub speed: Option<f64>,  
    pub power: Option<u16>,  
    pub compressed_speed_distance: Option<&'a [u8]>,  
    pub grade: Option<f64>,  
    pub resistance: Option<u8>,  // Relative. 0 is none  254 is Max.
    pub time_from_course: Option<f64>,  
    pub cycle_length: Option<f64>,  
    pub temperature: Option<i8>,  
    pub speed_1s: Option<f64>,  // Speed at 1s intervals.  Timestamp field indicates time of last array element.
    pub cycles: Option<u8>,  
    pub total_cycles: Option<u32>,  
    pub compressed_accumulated_power: Option<u16>,  
    pub accumulated_power: Option<u32>,  
    pub left_right_balance: Option<FitFieldLeftRightBalance>,  
    pub gps_accuracy: Option<u8>,  
    pub vertical_speed: Option<f64>,  
    pub calories: Option<u16>,  
    pub vertical_oscillation: Option<f64>,  
    pub stance_time_percent: Option<f64>,  
    pub stance_time: Option<f64>,  
    pub activity_type: Option<FitFieldActivityType>,  
    pub left_torque_effectiveness: Option<f64>,  
    pub right_torque_effectiveness: Option<f64>,  
    pub left_pedal_smoothness: Option<f64>,  
    pub right_pedal_smoothness: Option<f64>,  
    pub combined_pedal_smoothness: Option<f64>,  
    pub time128: Option<f64>,  
    pub stroke_type: Option<FitFieldStrokeType>,  
    pub zone: Option<u8>,  
    pub ball_speed: Option<f64>,  
    pub cadence256: Option<f64>,  // Log cadence and fractional cadence for backwards compatability
    pub fractional_cadence: Option<f64>,  
    pub total_hemoglobin_conc: Option<f64>,  // Total saturated and unsaturated hemoglobin
    pub total_hemoglobin_conc_min: Option<f64>,  // Min saturated and unsaturated hemoglobin
    pub total_hemoglobin_conc_max: Option<f64>,  // Max saturated and unsaturated hemoglobin
    pub saturated_hemoglobin_percent: Option<f64>,  // Percentage of hemoglobin saturated with oxygen
    pub saturated_hemoglobin_percent_min: Option<f64>,  // Min percentage of hemoglobin saturated with oxygen
    pub saturated_hemoglobin_percent_max: Option<f64>,  // Max percentage of hemoglobin saturated with oxygen
    pub device_index: Option<FitFieldDeviceIndex>,  
    pub left_pco: Option<i8>,  // Left platform center offset
    pub right_pco: Option<i8>,  // Right platform center offset
    pub left_power_phase: Option<f64>,  // Left power phase angles. Data value indexes defined by power_phase_type.
    pub left_power_phase_peak: Option<f64>,  // Left power phase peak angles. Data value indexes defined by power_phase_type.
    pub right_power_phase: Option<f64>,  // Right power phase angles. Data value indexes defined by power_phase_type.
    pub right_power_phase_peak: Option<f64>,  // Right power phase peak angles. Data value indexes defined by power_phase_type.
    pub enhanced_speed: Option<f64>,  
    pub enhanced_altitude: Option<f64>,  
    pub battery_soc: Option<f64>,  // lev battery state of charge
    pub motor_power: Option<u16>,  // lev motor power
    pub vertical_ratio: Option<f64>,  
    pub stance_time_balance: Option<f64>,  
    pub step_length: Option<f64>,  
    
}
impl<'a> FitMessageRecord<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageRecord<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageRecord {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            position_lat: None,
            position_long: None,
            altitude: None,
            heart_rate: None,
            cadence: None,
            distance: None,
            speed: None,
            power: None,
            compressed_speed_distance: None,
            grade: None,
            resistance: None,
            time_from_course: None,
            cycle_length: None,
            temperature: None,
            speed_1s: None,
            cycles: None,
            total_cycles: None,
            compressed_accumulated_power: None,
            accumulated_power: None,
            left_right_balance: None,
            gps_accuracy: None,
            vertical_speed: None,
            calories: None,
            vertical_oscillation: None,
            stance_time_percent: None,
            stance_time: None,
            activity_type: None,
            left_torque_effectiveness: None,
            right_torque_effectiveness: None,
            left_pedal_smoothness: None,
            right_pedal_smoothness: None,
            combined_pedal_smoothness: None,
            time128: None,
            stroke_type: None,
            zone: None,
            ball_speed: None,
            cadence256: None,
            fractional_cadence: None,
            total_hemoglobin_conc: None,
            total_hemoglobin_conc_min: None,
            total_hemoglobin_conc_max: None,
            saturated_hemoglobin_percent: None,
            saturated_hemoglobin_percent_min: None,
            saturated_hemoglobin_percent_max: None,
            device_index: None,
            left_pco: None,
            right_pco: None,
            left_power_phase: None,
            left_power_phase_peak: None,
            right_power_phase: None,
            right_power_phase_peak: None,
            enhanced_speed: None,
            enhanced_altitude: None,
            battery_soc: None,
            motor_power: None,
            vertical_ratio: None,
            stance_time_balance: None,
            step_length: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageRecord::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageRecord:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageRecord<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_lat = Some(val);
                    Ok(())
                },
            
                1 => {  // position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_long = Some(val);
                    Ok(())
                },
            
                2 => {  // altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                3 => {  // heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.heart_rate = Some(val);
                    Ok(())
                },
            
                4 => {  // cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.cadence = Some(val);
                    Ok(())
                },
            
                5 => {  // distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                6 => {  // speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                7 => {  // power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.power = Some(val);
                    Ok(())
                },
            
                8 => {  // compressed_speed_distance
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.compressed_speed_distance = Some(val);
                    Ok(())
                },
            
                9 => {  // grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                10 => {  // resistance
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.resistance = Some(val);
                    Ok(())
                },
            
                11 => {  // time_from_course
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_from_course = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                12 => {  // cycle_length
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.cycle_length = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                13 => {  // temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.temperature = Some(val);
                    Ok(())
                },
            
                17 => {  // speed_1s
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.speed_1s = Some(val as f64 / 16 as f64);
                    Ok(())
                },
            
                18 => {  // cycles
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.cycles = Some(val);
                    Ok(())
                },
            
                19 => {  // total_cycles
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_cycles = Some(val);
                    Ok(())
                },
            
                28 => {  // compressed_accumulated_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.compressed_accumulated_power = Some(val);
                    Ok(())
                },
            
                29 => {  // accumulated_power
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.accumulated_power = Some(val);
                    Ok(())
                },
            
                30 => {  // left_right_balance
                    let (val, outp) = FitFieldLeftRightBalance::parse(inp)?;
                    inp = outp;
                    message.left_right_balance = Some(val);
                    Ok(())
                },
            
                31 => {  // gps_accuracy
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.gps_accuracy = Some(val);
                    Ok(())
                },
            
                32 => {  // vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                33 => {  // calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calories = Some(val);
                    Ok(())
                },
            
                39 => {  // vertical_oscillation
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.vertical_oscillation = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                40 => {  // stance_time_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stance_time_percent = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                41 => {  // stance_time
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stance_time = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                42 => {  // activity_type
                    let (val, outp) = FitFieldActivityType::parse(inp)?;
                    inp = outp;
                    message.activity_type = Some(val);
                    Ok(())
                },
            
                43 => {  // left_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.left_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                44 => {  // right_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.right_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                45 => {  // left_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.left_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                46 => {  // right_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.right_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                47 => {  // combined_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.combined_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                48 => {  // time128
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.time128 = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                49 => {  // stroke_type
                    let (val, outp) = FitFieldStrokeType::parse(inp)?;
                    inp = outp;
                    message.stroke_type = Some(val);
                    Ok(())
                },
            
                50 => {  // zone
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.zone = Some(val);
                    Ok(())
                },
            
                51 => {  // ball_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.ball_speed = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                52 => {  // cadence256
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.cadence256 = Some(val as f64 / 256 as f64);
                    Ok(())
                },
            
                53 => {  // fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                54 => {  // total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                55 => {  // total_hemoglobin_conc_min
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_hemoglobin_conc_min = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                56 => {  // total_hemoglobin_conc_max
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_hemoglobin_conc_max = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                57 => {  // saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                58 => {  // saturated_hemoglobin_percent_min
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.saturated_hemoglobin_percent_min = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                59 => {  // saturated_hemoglobin_percent_max
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.saturated_hemoglobin_percent_max = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                62 => {  // device_index
                    let (val, outp) = FitFieldDeviceIndex::parse(inp)?;
                    inp = outp;
                    message.device_index = Some(val);
                    Ok(())
                },
            
                67 => {  // left_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.left_pco = Some(val);
                    Ok(())
                },
            
                68 => {  // right_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.right_pco = Some(val);
                    Ok(())
                },
            
                69 => {  // left_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.left_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                70 => {  // left_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.left_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                71 => {  // right_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.right_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                72 => {  // right_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.right_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                73 => {  // enhanced_speed
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                78 => {  // enhanced_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                81 => {  // battery_soc
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.battery_soc = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                82 => {  // motor_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.motor_power = Some(val);
                    Ok(())
                },
            
                83 => {  // vertical_ratio
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.vertical_ratio = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                84 => {  // stance_time_balance
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stance_time_balance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                85 => {  // step_length
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.step_length = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageScheduleSubfieldProduct {
    Default(u16),
    GarminProduct(FitFieldGarminProduct),
}

impl FitMessageScheduleSubfieldProduct {
    fn parse<'a>(message: &FitMessageSchedule<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageScheduleSubfieldProduct,  &'a [u8])> {
        
        match message.manufacturer {
        
            Some(FitFieldManufacturer::Garmin) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageScheduleSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::Dynastream) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageScheduleSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::DynastreamOem) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageScheduleSubfieldProduct::GarminProduct(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
        Ok((FitMessageScheduleSubfieldProduct::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageSchedule<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub manufacturer: Option<FitFieldManufacturer>,  // Corresponds to file_id of scheduled workout / course.
    pub product: Option<FitMessageScheduleSubfieldProduct>,  // Corresponds to file_id of scheduled workout / course.
    pub serial_number: Option<u32>,  // Corresponds to file_id of scheduled workout / course.
    pub time_created: Option<FitFieldDateTime>,  // Corresponds to file_id of scheduled workout / course.
    pub completed: Option<bool>,  // TRUE if this activity has been started
    pub ftype: Option<FitFieldSchedule>,  
    pub scheduled_time: Option<FitFieldLocalDateTime>,  
    
}
impl<'a> FitMessageSchedule<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSchedule<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSchedule {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            manufacturer: None,
            product: None,
            serial_number: None,
            time_created: None,
            completed: None,
            ftype: None,
            scheduled_time: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSchedule::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSchedule:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSchedule<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // manufacturer
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer = Some(val);
                    Ok(())
                },
            
                1 => {  // product
                    let (val, outp) = FitMessageScheduleSubfieldProduct::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.product = Some(val);
                    Ok(())
                },
            
                2 => {  // serial_number
                    let (val, outp) = parse_uint32z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.serial_number = val;
                    Ok(())
                },
            
                3 => {  // time_created
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_created = Some(val);
                    Ok(())
                },
            
                4 => {  // completed
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.completed = Some(val);
                    Ok(())
                },
            
                5 => {  // ftype
                    let (val, outp) = FitFieldSchedule::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                6 => {  // scheduled_time
                    let (val, outp) = FitFieldLocalDateTime::parse(inp, message.definition_message.endianness, tz_offset)?;
                    inp = outp;
                    message.scheduled_time = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSdmProfile<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub enabled: Option<bool>,  
    pub sdm_ant_id: Option<u16>,  
    pub sdm_cal_factor: Option<f64>,  
    pub odometer: Option<f64>,  
    pub speed_source: Option<bool>,  // Use footpod for speed source instead of GPS
    pub sdm_ant_id_trans_type: Option<u8>,  
    pub odometer_rollover: Option<u8>,  // Rollover counter that can be used to extend the odometer
    
}
impl<'a> FitMessageSdmProfile<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSdmProfile<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSdmProfile {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            enabled: None,
            sdm_ant_id: None,
            sdm_cal_factor: None,
            odometer: None,
            speed_source: None,
            sdm_ant_id_trans_type: None,
            odometer_rollover: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSdmProfile::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSdmProfile:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSdmProfile<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                1 => {  // sdm_ant_id
                    let (val, outp) = parse_uint16z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sdm_ant_id = val;
                    Ok(())
                },
            
                2 => {  // sdm_cal_factor
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sdm_cal_factor = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                3 => {  // odometer
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.odometer = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                4 => {  // speed_source
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.speed_source = Some(val);
                    Ok(())
                },
            
                5 => {  // sdm_ant_id_trans_type
                    let (val, outp) = parse_uint8z(inp)?;
                    inp = outp;
                    message.sdm_ant_id_trans_type = val;
                    Ok(())
                },
            
                7 => {  // odometer_rollover
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.odometer_rollover = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSegmentFile<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub file_uuid: Option<String>,  // UUID of the segment file
    pub enabled: Option<bool>,  // Enabled state of the segment file
    pub user_profile_primary_key: Option<u32>,  // Primary key of the user that created the segment file
    pub leader_type: Option<FitFieldSegmentLeaderboardType>,  // Leader type of each leader in the segment file
    pub leader_group_primary_key: Option<u32>,  // Group primary key of each leader in the segment file
    pub leader_activity_id: Option<u32>,  // Activity ID of each leader in the segment file
    pub leader_activity_id_string: Option<String>,  // String version of the activity ID of each leader in the segment file. 21 characters long for each ID, express in decimal
    pub default_race_leader: Option<u8>,  // Index for the Leader Board entry selected as the default race participant
    
}
impl<'a> FitMessageSegmentFile<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSegmentFile<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSegmentFile {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            file_uuid: None,
            enabled: None,
            user_profile_primary_key: None,
            leader_type: None,
            leader_group_primary_key: None,
            leader_activity_id: None,
            leader_activity_id_string: None,
            default_race_leader: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSegmentFile::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSegmentFile:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSegmentFile<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                1 => {  // file_uuid
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.file_uuid = Some(val);
                    Ok(())
                },
            
                3 => {  // enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                4 => {  // user_profile_primary_key
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.user_profile_primary_key = Some(val);
                    Ok(())
                },
            
                7 => {  // leader_type
                    let (val, outp) = FitFieldSegmentLeaderboardType::parse(inp)?;
                    inp = outp;
                    message.leader_type = Some(val);
                    Ok(())
                },
            
                8 => {  // leader_group_primary_key
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.leader_group_primary_key = Some(val);
                    Ok(())
                },
            
                9 => {  // leader_activity_id
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.leader_activity_id = Some(val);
                    Ok(())
                },
            
                10 => {  // leader_activity_id_string
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.leader_activity_id_string = Some(val);
                    Ok(())
                },
            
                11 => {  // default_race_leader
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.default_race_leader = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSegmentId<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub name: Option<String>,  // Friendly name assigned to segment
    pub uuid: Option<String>,  // UUID of the segment
    pub sport: Option<FitFieldSport>,  // Sport associated with the segment
    pub enabled: Option<bool>,  // Segment enabled for evaluation
    pub user_profile_primary_key: Option<u32>,  // Primary key of the user that created the segment 
    pub device_id: Option<u32>,  // ID of the device that created the segment
    pub default_race_leader: Option<u8>,  // Index for the Leader Board entry selected as the default race participant
    pub delete_status: Option<FitFieldSegmentDeleteStatus>,  // Indicates if any segments should be deleted
    pub selection_type: Option<FitFieldSegmentSelectionType>,  // Indicates how the segment was selected to be sent to the device
    
}
impl<'a> FitMessageSegmentId<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSegmentId<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSegmentId {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            name: None,
            uuid: None,
            sport: None,
            enabled: None,
            user_profile_primary_key: None,
            device_id: None,
            default_race_leader: None,
            delete_status: None,
            selection_type: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSegmentId::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSegmentId:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSegmentId<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                1 => {  // uuid
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.uuid = Some(val);
                    Ok(())
                },
            
                2 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                3 => {  // enabled
                    let (val, outp) = parse_bool(inp)?;
                    inp = outp;
                    message.enabled = Some(val);
                    Ok(())
                },
            
                4 => {  // user_profile_primary_key
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.user_profile_primary_key = Some(val);
                    Ok(())
                },
            
                5 => {  // device_id
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.device_id = Some(val);
                    Ok(())
                },
            
                6 => {  // default_race_leader
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.default_race_leader = Some(val);
                    Ok(())
                },
            
                7 => {  // delete_status
                    let (val, outp) = FitFieldSegmentDeleteStatus::parse(inp)?;
                    inp = outp;
                    message.delete_status = Some(val);
                    Ok(())
                },
            
                8 => {  // selection_type
                    let (val, outp) = FitFieldSegmentSelectionType::parse(inp)?;
                    inp = outp;
                    message.selection_type = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageSegmentLapSubfieldTotalCycles {
    Default(u32),
    TotalStrokes(u32),
}

impl FitMessageSegmentLapSubfieldTotalCycles {
    fn parse<'a>(message: &FitMessageSegmentLap<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageSegmentLapSubfieldTotalCycles,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Cycling) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageSegmentLapSubfieldTotalCycles::TotalStrokes(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageSegmentLapSubfieldTotalCycles::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageSegmentLap<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub timestamp: Option<FitFieldDateTime>,  // Lap end time.
    pub event: Option<FitFieldEvent>,  
    pub event_type: Option<FitFieldEventType>,  
    pub start_time: Option<FitFieldDateTime>,  
    pub start_position_lat: Option<i32>,  
    pub start_position_long: Option<i32>,  
    pub end_position_lat: Option<i32>,  
    pub end_position_long: Option<i32>,  
    pub total_elapsed_time: Option<f64>,  // Time (includes pauses)
    pub total_timer_time: Option<f64>,  // Timer Time (excludes pauses)
    pub total_distance: Option<f64>,  
    pub total_cycles: Option<FitMessageSegmentLapSubfieldTotalCycles>,  
    pub total_calories: Option<u16>,  
    pub total_fat_calories: Option<u16>,  // If New Leaf
    pub avg_speed: Option<f64>,  
    pub max_speed: Option<f64>,  
    pub avg_heart_rate: Option<u8>,  
    pub max_heart_rate: Option<u8>,  
    pub avg_cadence: Option<u8>,  // total_cycles / total_timer_time if non_zero_avg_cadence otherwise total_cycles / total_elapsed_time
    pub max_cadence: Option<u8>,  
    pub avg_power: Option<u16>,  // total_power / total_timer_time if non_zero_avg_power otherwise total_power / total_elapsed_time
    pub max_power: Option<u16>,  
    pub total_ascent: Option<u16>,  
    pub total_descent: Option<u16>,  
    pub sport: Option<FitFieldSport>,  
    pub event_group: Option<u8>,  
    pub nec_lat: Option<i32>,  // North east corner latitude.
    pub nec_long: Option<i32>,  // North east corner longitude.
    pub swc_lat: Option<i32>,  // South west corner latitude.
    pub swc_long: Option<i32>,  // South west corner latitude.
    pub name: Option<String>,  
    pub normalized_power: Option<u16>,  
    pub left_right_balance: Option<FitFieldLeftRightBalance100>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    pub total_work: Option<u32>,  
    pub avg_altitude: Option<f64>,  
    pub max_altitude: Option<f64>,  
    pub gps_accuracy: Option<u8>,  
    pub avg_grade: Option<f64>,  
    pub avg_pos_grade: Option<f64>,  
    pub avg_neg_grade: Option<f64>,  
    pub max_pos_grade: Option<f64>,  
    pub max_neg_grade: Option<f64>,  
    pub avg_temperature: Option<i8>,  
    pub max_temperature: Option<i8>,  
    pub total_moving_time: Option<f64>,  
    pub avg_pos_vertical_speed: Option<f64>,  
    pub avg_neg_vertical_speed: Option<f64>,  
    pub max_pos_vertical_speed: Option<f64>,  
    pub max_neg_vertical_speed: Option<f64>,  
    pub time_in_hr_zone: Option<f64>,  
    pub time_in_speed_zone: Option<f64>,  
    pub time_in_cadence_zone: Option<f64>,  
    pub time_in_power_zone: Option<f64>,  
    pub repetition_num: Option<u16>,  
    pub min_altitude: Option<f64>,  
    pub min_heart_rate: Option<u8>,  
    pub active_time: Option<f64>,  
    pub wkt_step_index: Option<FitFieldMessageIndex>,  
    pub sport_event: Option<FitFieldSportEvent>,  
    pub avg_left_torque_effectiveness: Option<f64>,  
    pub avg_right_torque_effectiveness: Option<f64>,  
    pub avg_left_pedal_smoothness: Option<f64>,  
    pub avg_right_pedal_smoothness: Option<f64>,  
    pub avg_combined_pedal_smoothness: Option<f64>,  
    pub status: Option<FitFieldSegmentLapStatus>,  
    pub uuid: Option<String>,  
    pub avg_fractional_cadence: Option<f64>,  // fractional part of the avg_cadence
    pub max_fractional_cadence: Option<f64>,  // fractional part of the max_cadence
    pub total_fractional_cycles: Option<f64>,  // fractional part of the total_cycles
    pub front_gear_shift_count: Option<u16>,  
    pub rear_gear_shift_count: Option<u16>,  
    pub time_standing: Option<f64>,  // Total time spent in the standing position
    pub stand_count: Option<u16>,  // Number of transitions to the standing state
    pub avg_left_pco: Option<i8>,  // Average left platform center offset
    pub avg_right_pco: Option<i8>,  // Average right platform center offset
    pub avg_left_power_phase: Option<f64>,  // Average left power phase angles. Data value indexes defined by power_phase_type.
    pub avg_left_power_phase_peak: Option<f64>,  // Average left power phase peak angles. Data value indexes defined by power_phase_type.
    pub avg_right_power_phase: Option<f64>,  // Average right power phase angles. Data value indexes defined by power_phase_type.
    pub avg_right_power_phase_peak: Option<f64>,  // Average right power phase peak angles. Data value indexes defined by power_phase_type.
    pub avg_power_position: Option<u16>,  // Average power by position. Data value indexes defined by rider_position_type.
    pub max_power_position: Option<u16>,  // Maximum power by position. Data value indexes defined by rider_position_type.
    pub avg_cadence_position: Option<u8>,  // Average cadence by position. Data value indexes defined by rider_position_type.
    pub max_cadence_position: Option<u8>,  // Maximum cadence by position. Data value indexes defined by rider_position_type.
    pub manufacturer: Option<FitFieldManufacturer>,  // Manufacturer that produced the segment
    
}
impl<'a> FitMessageSegmentLap<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSegmentLap<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSegmentLap {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            timestamp: None,
            event: None,
            event_type: None,
            start_time: None,
            start_position_lat: None,
            start_position_long: None,
            end_position_lat: None,
            end_position_long: None,
            total_elapsed_time: None,
            total_timer_time: None,
            total_distance: None,
            total_cycles: None,
            total_calories: None,
            total_fat_calories: None,
            avg_speed: None,
            max_speed: None,
            avg_heart_rate: None,
            max_heart_rate: None,
            avg_cadence: None,
            max_cadence: None,
            avg_power: None,
            max_power: None,
            total_ascent: None,
            total_descent: None,
            sport: None,
            event_group: None,
            nec_lat: None,
            nec_long: None,
            swc_lat: None,
            swc_long: None,
            name: None,
            normalized_power: None,
            left_right_balance: None,
            sub_sport: None,
            total_work: None,
            avg_altitude: None,
            max_altitude: None,
            gps_accuracy: None,
            avg_grade: None,
            avg_pos_grade: None,
            avg_neg_grade: None,
            max_pos_grade: None,
            max_neg_grade: None,
            avg_temperature: None,
            max_temperature: None,
            total_moving_time: None,
            avg_pos_vertical_speed: None,
            avg_neg_vertical_speed: None,
            max_pos_vertical_speed: None,
            max_neg_vertical_speed: None,
            time_in_hr_zone: None,
            time_in_speed_zone: None,
            time_in_cadence_zone: None,
            time_in_power_zone: None,
            repetition_num: None,
            min_altitude: None,
            min_heart_rate: None,
            active_time: None,
            wkt_step_index: None,
            sport_event: None,
            avg_left_torque_effectiveness: None,
            avg_right_torque_effectiveness: None,
            avg_left_pedal_smoothness: None,
            avg_right_pedal_smoothness: None,
            avg_combined_pedal_smoothness: None,
            status: None,
            uuid: None,
            avg_fractional_cadence: None,
            max_fractional_cadence: None,
            total_fractional_cycles: None,
            front_gear_shift_count: None,
            rear_gear_shift_count: None,
            time_standing: None,
            stand_count: None,
            avg_left_pco: None,
            avg_right_pco: None,
            avg_left_power_phase: None,
            avg_left_power_phase_peak: None,
            avg_right_power_phase: None,
            avg_right_power_phase_peak: None,
            avg_power_position: None,
            max_power_position: None,
            avg_cadence_position: None,
            max_cadence_position: None,
            manufacturer: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSegmentLap::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSegmentLap:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSegmentLap<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // event
                    let (val, outp) = FitFieldEvent::parse(inp)?;
                    inp = outp;
                    message.event = Some(val);
                    Ok(())
                },
            
                1 => {  // event_type
                    let (val, outp) = FitFieldEventType::parse(inp)?;
                    inp = outp;
                    message.event_type = Some(val);
                    Ok(())
                },
            
                2 => {  // start_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_time = Some(val);
                    Ok(())
                },
            
                3 => {  // start_position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_position_lat = Some(val);
                    Ok(())
                },
            
                4 => {  // start_position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_position_long = Some(val);
                    Ok(())
                },
            
                5 => {  // end_position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_position_lat = Some(val);
                    Ok(())
                },
            
                6 => {  // end_position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_position_long = Some(val);
                    Ok(())
                },
            
                7 => {  // total_elapsed_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_elapsed_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                8 => {  // total_timer_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_timer_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                9 => {  // total_distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                10 => {  // total_cycles
                    let (val, outp) = FitMessageSegmentLapSubfieldTotalCycles::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.total_cycles = Some(val);
                    Ok(())
                },
            
                11 => {  // total_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_calories = Some(val);
                    Ok(())
                },
            
                12 => {  // total_fat_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_fat_calories = Some(val);
                    Ok(())
                },
            
                13 => {  // avg_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                14 => {  // max_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                15 => {  // avg_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_heart_rate = Some(val);
                    Ok(())
                },
            
                16 => {  // max_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_heart_rate = Some(val);
                    Ok(())
                },
            
                17 => {  // avg_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_cadence = Some(val);
                    Ok(())
                },
            
                18 => {  // max_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_cadence = Some(val);
                    Ok(())
                },
            
                19 => {  // avg_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_power = Some(val);
                    Ok(())
                },
            
                20 => {  // max_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_power = Some(val);
                    Ok(())
                },
            
                21 => {  // total_ascent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_ascent = Some(val);
                    Ok(())
                },
            
                22 => {  // total_descent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_descent = Some(val);
                    Ok(())
                },
            
                23 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                24 => {  // event_group
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.event_group = Some(val);
                    Ok(())
                },
            
                25 => {  // nec_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.nec_lat = Some(val);
                    Ok(())
                },
            
                26 => {  // nec_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.nec_long = Some(val);
                    Ok(())
                },
            
                27 => {  // swc_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.swc_lat = Some(val);
                    Ok(())
                },
            
                28 => {  // swc_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.swc_long = Some(val);
                    Ok(())
                },
            
                29 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                30 => {  // normalized_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.normalized_power = Some(val);
                    Ok(())
                },
            
                31 => {  // left_right_balance
                    let (val, outp) = FitFieldLeftRightBalance100::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.left_right_balance = Some(val);
                    Ok(())
                },
            
                32 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                33 => {  // total_work
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_work = Some(val);
                    Ok(())
                },
            
                34 => {  // avg_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                35 => {  // max_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                36 => {  // gps_accuracy
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.gps_accuracy = Some(val);
                    Ok(())
                },
            
                37 => {  // avg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                38 => {  // avg_pos_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_pos_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                39 => {  // avg_neg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_neg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                40 => {  // max_pos_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_pos_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                41 => {  // max_neg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_neg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                42 => {  // avg_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_temperature = Some(val);
                    Ok(())
                },
            
                43 => {  // max_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.max_temperature = Some(val);
                    Ok(())
                },
            
                44 => {  // total_moving_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_moving_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                45 => {  // avg_pos_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_pos_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                46 => {  // avg_neg_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_neg_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                47 => {  // max_pos_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_pos_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                48 => {  // max_neg_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_neg_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                49 => {  // time_in_hr_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_hr_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                50 => {  // time_in_speed_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_speed_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                51 => {  // time_in_cadence_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_cadence_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                52 => {  // time_in_power_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_power_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                53 => {  // repetition_num
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.repetition_num = Some(val);
                    Ok(())
                },
            
                54 => {  // min_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                55 => {  // min_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.min_heart_rate = Some(val);
                    Ok(())
                },
            
                56 => {  // active_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.active_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                57 => {  // wkt_step_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.wkt_step_index = Some(val);
                    Ok(())
                },
            
                58 => {  // sport_event
                    let (val, outp) = FitFieldSportEvent::parse(inp)?;
                    inp = outp;
                    message.sport_event = Some(val);
                    Ok(())
                },
            
                59 => {  // avg_left_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                60 => {  // avg_right_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                61 => {  // avg_left_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                62 => {  // avg_right_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                63 => {  // avg_combined_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_combined_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                64 => {  // status
                    let (val, outp) = FitFieldSegmentLapStatus::parse(inp)?;
                    inp = outp;
                    message.status = Some(val);
                    Ok(())
                },
            
                65 => {  // uuid
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.uuid = Some(val);
                    Ok(())
                },
            
                66 => {  // avg_fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                67 => {  // max_fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                68 => {  // total_fractional_cycles
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.total_fractional_cycles = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                69 => {  // front_gear_shift_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.front_gear_shift_count = Some(val);
                    Ok(())
                },
            
                70 => {  // rear_gear_shift_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.rear_gear_shift_count = Some(val);
                    Ok(())
                },
            
                71 => {  // time_standing
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_standing = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                72 => {  // stand_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stand_count = Some(val);
                    Ok(())
                },
            
                73 => {  // avg_left_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_left_pco = Some(val);
                    Ok(())
                },
            
                74 => {  // avg_right_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_right_pco = Some(val);
                    Ok(())
                },
            
                75 => {  // avg_left_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                76 => {  // avg_left_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                77 => {  // avg_right_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                78 => {  // avg_right_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                79 => {  // avg_power_position
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_power_position = Some(val);
                    Ok(())
                },
            
                80 => {  // max_power_position
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_power_position = Some(val);
                    Ok(())
                },
            
                81 => {  // avg_cadence_position
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_cadence_position = Some(val);
                    Ok(())
                },
            
                82 => {  // max_cadence_position
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_cadence_position = Some(val);
                    Ok(())
                },
            
                83 => {  // manufacturer
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSegmentLeaderboardEntry<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub name: Option<String>,  // Friendly name assigned to leader
    pub ftype: Option<FitFieldSegmentLeaderboardType>,  // Leader classification
    pub group_primary_key: Option<u32>,  // Primary user ID of this leader
    pub activity_id: Option<u32>,  // ID of the activity associated with this leader time
    pub segment_time: Option<f64>,  // Segment Time (includes pauses)
    pub activity_id_string: Option<String>,  // String version of the activity_id. 21 characters long, express in decimal
    
}
impl<'a> FitMessageSegmentLeaderboardEntry<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSegmentLeaderboardEntry<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSegmentLeaderboardEntry {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            name: None,
            ftype: None,
            group_primary_key: None,
            activity_id: None,
            segment_time: None,
            activity_id_string: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSegmentLeaderboardEntry::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSegmentLeaderboardEntry:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSegmentLeaderboardEntry<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                1 => {  // ftype
                    let (val, outp) = FitFieldSegmentLeaderboardType::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                2 => {  // group_primary_key
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.group_primary_key = Some(val);
                    Ok(())
                },
            
                3 => {  // activity_id
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.activity_id = Some(val);
                    Ok(())
                },
            
                4 => {  // segment_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.segment_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                5 => {  // activity_id_string
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.activity_id_string = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSegmentPoint<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub position_lat: Option<i32>,  
    pub position_long: Option<i32>,  
    pub distance: Option<f64>,  // Accumulated distance along the segment at the described point
    pub altitude: Option<f64>,  // Accumulated altitude along the segment at the described point
    pub leader_time: Option<f64>,  // Accumualted time each leader board member required to reach the described point. This value is zero for all leader board members at the starting point of the segment. 
    
}
impl<'a> FitMessageSegmentPoint<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSegmentPoint<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSegmentPoint {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            position_lat: None,
            position_long: None,
            distance: None,
            altitude: None,
            leader_time: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSegmentPoint::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSegmentPoint:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSegmentPoint<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                1 => {  // position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_lat = Some(val);
                    Ok(())
                },
            
                2 => {  // position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.position_long = Some(val);
                    Ok(())
                },
            
                3 => {  // distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                4 => {  // altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                5 => {  // leader_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.leader_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageSessionSubfieldTotalCycles {
    Default(u32),
    TotalStrides(u32),
}

impl FitMessageSessionSubfieldTotalCycles {
    fn parse<'a>(message: &FitMessageSession<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageSessionSubfieldTotalCycles,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Running) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageSessionSubfieldTotalCycles::TotalStrides(val), o))
            },
        
            Some(FitFieldSport::Walking) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageSessionSubfieldTotalCycles::TotalStrides(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageSessionSubfieldTotalCycles::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageSessionSubfieldAvgCadence {
    Default(u8),
    AvgRunningCadence(u8),
}

impl FitMessageSessionSubfieldAvgCadence {
    fn parse<'a>(message: &FitMessageSession<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageSessionSubfieldAvgCadence,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Running) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageSessionSubfieldAvgCadence::AvgRunningCadence(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint8(inp)?;
        Ok((FitMessageSessionSubfieldAvgCadence::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageSessionSubfieldMaxCadence {
    Default(u8),
    MaxRunningCadence(u8),
}

impl FitMessageSessionSubfieldMaxCadence {
    fn parse<'a>(message: &FitMessageSession<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageSessionSubfieldMaxCadence,  &'a [u8])> {
        
        match message.sport {
        
            Some(FitFieldSport::Running) => {
                let (val, o) = parse_uint8(inp)?;
                return Ok((FitMessageSessionSubfieldMaxCadence::MaxRunningCadence(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint8(inp)?;
        Ok((FitMessageSessionSubfieldMaxCadence::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageSession<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  // Selected bit is set for the current session.
    pub timestamp: Option<FitFieldDateTime>,  // Sesson end time.
    pub event: Option<FitFieldEvent>,  // session
    pub event_type: Option<FitFieldEventType>,  // stop
    pub start_time: Option<FitFieldDateTime>,  
    pub start_position_lat: Option<i32>,  
    pub start_position_long: Option<i32>,  
    pub sport: Option<FitFieldSport>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    pub total_elapsed_time: Option<f64>,  // Time (includes pauses)
    pub total_timer_time: Option<f64>,  // Timer Time (excludes pauses)
    pub total_distance: Option<f64>,  
    pub total_cycles: Option<FitMessageSessionSubfieldTotalCycles>,  
    pub total_calories: Option<u16>,  
    pub total_fat_calories: Option<u16>,  
    pub avg_speed: Option<f64>,  // total_distance / total_timer_time
    pub max_speed: Option<f64>,  
    pub avg_heart_rate: Option<u8>,  // average heart rate (excludes pause time)
    pub max_heart_rate: Option<u8>,  
    pub avg_cadence: Option<FitMessageSessionSubfieldAvgCadence>,  // total_cycles / total_timer_time if non_zero_avg_cadence otherwise total_cycles / total_elapsed_time
    pub max_cadence: Option<FitMessageSessionSubfieldMaxCadence>,  
    pub avg_power: Option<u16>,  // total_power / total_timer_time if non_zero_avg_power otherwise total_power / total_elapsed_time
    pub max_power: Option<u16>,  
    pub total_ascent: Option<u16>,  
    pub total_descent: Option<u16>,  
    pub total_training_effect: Option<f64>,  
    pub first_lap_index: Option<u16>,  
    pub num_laps: Option<u16>,  
    pub event_group: Option<u8>,  
    pub trigger: Option<FitFieldSessionTrigger>,  
    pub nec_lat: Option<i32>,  
    pub nec_long: Option<i32>,  
    pub swc_lat: Option<i32>,  
    pub swc_long: Option<i32>,  
    pub normalized_power: Option<u16>,  
    pub training_stress_score: Option<f64>,  
    pub intensity_factor: Option<f64>,  
    pub left_right_balance: Option<FitFieldLeftRightBalance100>,  
    pub avg_stroke_count: Option<f64>,  
    pub avg_stroke_distance: Option<f64>,  
    pub swim_stroke: Option<FitFieldSwimStroke>,  
    pub pool_length: Option<f64>,  
    pub threshold_power: Option<u16>,  
    pub pool_length_unit: Option<FitFieldDisplayMeasure>,  
    pub num_active_lengths: Option<u16>,  // # of active lengths of swim pool
    pub total_work: Option<u32>,  
    pub avg_altitude: Option<f64>,  
    pub max_altitude: Option<f64>,  
    pub gps_accuracy: Option<u8>,  
    pub avg_grade: Option<f64>,  
    pub avg_pos_grade: Option<f64>,  
    pub avg_neg_grade: Option<f64>,  
    pub max_pos_grade: Option<f64>,  
    pub max_neg_grade: Option<f64>,  
    pub avg_temperature: Option<i8>,  
    pub max_temperature: Option<i8>,  
    pub total_moving_time: Option<f64>,  
    pub avg_pos_vertical_speed: Option<f64>,  
    pub avg_neg_vertical_speed: Option<f64>,  
    pub max_pos_vertical_speed: Option<f64>,  
    pub max_neg_vertical_speed: Option<f64>,  
    pub min_heart_rate: Option<u8>,  
    pub time_in_hr_zone: Option<f64>,  
    pub time_in_speed_zone: Option<f64>,  
    pub time_in_cadence_zone: Option<f64>,  
    pub time_in_power_zone: Option<f64>,  
    pub avg_lap_time: Option<f64>,  
    pub best_lap_index: Option<u16>,  
    pub min_altitude: Option<f64>,  
    pub player_score: Option<u16>,  
    pub opponent_score: Option<u16>,  
    pub opponent_name: Option<String>,  
    pub stroke_count: Option<u16>,  // stroke_type enum used as the index
    pub zone_count: Option<u16>,  // zone number used as the index
    pub max_ball_speed: Option<f64>,  
    pub avg_ball_speed: Option<f64>,  
    pub avg_vertical_oscillation: Option<f64>,  
    pub avg_stance_time_percent: Option<f64>,  
    pub avg_stance_time: Option<f64>,  
    pub avg_fractional_cadence: Option<f64>,  // fractional part of the avg_cadence
    pub max_fractional_cadence: Option<f64>,  // fractional part of the max_cadence
    pub total_fractional_cycles: Option<f64>,  // fractional part of the total_cycles
    pub avg_total_hemoglobin_conc: Option<f64>,  // Avg saturated and unsaturated hemoglobin
    pub min_total_hemoglobin_conc: Option<f64>,  // Min saturated and unsaturated hemoglobin
    pub max_total_hemoglobin_conc: Option<f64>,  // Max saturated and unsaturated hemoglobin
    pub avg_saturated_hemoglobin_percent: Option<f64>,  // Avg percentage of hemoglobin saturated with oxygen
    pub min_saturated_hemoglobin_percent: Option<f64>,  // Min percentage of hemoglobin saturated with oxygen
    pub max_saturated_hemoglobin_percent: Option<f64>,  // Max percentage of hemoglobin saturated with oxygen
    pub avg_left_torque_effectiveness: Option<f64>,  
    pub avg_right_torque_effectiveness: Option<f64>,  
    pub avg_left_pedal_smoothness: Option<f64>,  
    pub avg_right_pedal_smoothness: Option<f64>,  
    pub avg_combined_pedal_smoothness: Option<f64>,  
    pub sport_index: Option<u8>,  
    pub time_standing: Option<f64>,  // Total time spend in the standing position
    pub stand_count: Option<u16>,  // Number of transitions to the standing state
    pub avg_left_pco: Option<i8>,  // Average platform center offset Left
    pub avg_right_pco: Option<i8>,  // Average platform center offset Right
    pub avg_left_power_phase: Option<f64>,  // Average left power phase angles. Indexes defined by power_phase_type.
    pub avg_left_power_phase_peak: Option<f64>,  // Average left power phase peak angles. Data value indexes defined by power_phase_type.
    pub avg_right_power_phase: Option<f64>,  // Average right power phase angles. Data value indexes defined by power_phase_type.
    pub avg_right_power_phase_peak: Option<f64>,  // Average right power phase peak angles data value indexes  defined by power_phase_type.
    pub avg_power_position: Option<u16>,  // Average power by position. Data value indexes defined by rider_position_type.
    pub max_power_position: Option<u16>,  // Maximum power by position. Data value indexes defined by rider_position_type.
    pub avg_cadence_position: Option<u8>,  // Average cadence by position. Data value indexes defined by rider_position_type.
    pub max_cadence_position: Option<u8>,  // Maximum cadence by position. Data value indexes defined by rider_position_type.
    pub enhanced_avg_speed: Option<f64>,  // total_distance / total_timer_time
    pub enhanced_max_speed: Option<f64>,  
    pub enhanced_avg_altitude: Option<f64>,  
    pub enhanced_min_altitude: Option<f64>,  
    pub enhanced_max_altitude: Option<f64>,  
    pub avg_lev_motor_power: Option<u16>,  // lev average motor power during session
    pub max_lev_motor_power: Option<u16>,  // lev maximum motor power during session
    pub lev_battery_consumption: Option<f64>,  // lev battery consumption during session
    pub avg_vertical_ratio: Option<f64>,  
    pub avg_stance_time_balance: Option<f64>,  
    pub avg_step_length: Option<f64>,  
    pub total_anaerobic_training_effect: Option<f64>,  
    
}
impl<'a> FitMessageSession<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSession<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSession {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            timestamp: None,
            event: None,
            event_type: None,
            start_time: None,
            start_position_lat: None,
            start_position_long: None,
            sport: None,
            sub_sport: None,
            total_elapsed_time: None,
            total_timer_time: None,
            total_distance: None,
            total_cycles: None,
            total_calories: None,
            total_fat_calories: None,
            avg_speed: None,
            max_speed: None,
            avg_heart_rate: None,
            max_heart_rate: None,
            avg_cadence: None,
            max_cadence: None,
            avg_power: None,
            max_power: None,
            total_ascent: None,
            total_descent: None,
            total_training_effect: None,
            first_lap_index: None,
            num_laps: None,
            event_group: None,
            trigger: None,
            nec_lat: None,
            nec_long: None,
            swc_lat: None,
            swc_long: None,
            normalized_power: None,
            training_stress_score: None,
            intensity_factor: None,
            left_right_balance: None,
            avg_stroke_count: None,
            avg_stroke_distance: None,
            swim_stroke: None,
            pool_length: None,
            threshold_power: None,
            pool_length_unit: None,
            num_active_lengths: None,
            total_work: None,
            avg_altitude: None,
            max_altitude: None,
            gps_accuracy: None,
            avg_grade: None,
            avg_pos_grade: None,
            avg_neg_grade: None,
            max_pos_grade: None,
            max_neg_grade: None,
            avg_temperature: None,
            max_temperature: None,
            total_moving_time: None,
            avg_pos_vertical_speed: None,
            avg_neg_vertical_speed: None,
            max_pos_vertical_speed: None,
            max_neg_vertical_speed: None,
            min_heart_rate: None,
            time_in_hr_zone: None,
            time_in_speed_zone: None,
            time_in_cadence_zone: None,
            time_in_power_zone: None,
            avg_lap_time: None,
            best_lap_index: None,
            min_altitude: None,
            player_score: None,
            opponent_score: None,
            opponent_name: None,
            stroke_count: None,
            zone_count: None,
            max_ball_speed: None,
            avg_ball_speed: None,
            avg_vertical_oscillation: None,
            avg_stance_time_percent: None,
            avg_stance_time: None,
            avg_fractional_cadence: None,
            max_fractional_cadence: None,
            total_fractional_cycles: None,
            avg_total_hemoglobin_conc: None,
            min_total_hemoglobin_conc: None,
            max_total_hemoglobin_conc: None,
            avg_saturated_hemoglobin_percent: None,
            min_saturated_hemoglobin_percent: None,
            max_saturated_hemoglobin_percent: None,
            avg_left_torque_effectiveness: None,
            avg_right_torque_effectiveness: None,
            avg_left_pedal_smoothness: None,
            avg_right_pedal_smoothness: None,
            avg_combined_pedal_smoothness: None,
            sport_index: None,
            time_standing: None,
            stand_count: None,
            avg_left_pco: None,
            avg_right_pco: None,
            avg_left_power_phase: None,
            avg_left_power_phase_peak: None,
            avg_right_power_phase: None,
            avg_right_power_phase_peak: None,
            avg_power_position: None,
            max_power_position: None,
            avg_cadence_position: None,
            max_cadence_position: None,
            enhanced_avg_speed: None,
            enhanced_max_speed: None,
            enhanced_avg_altitude: None,
            enhanced_min_altitude: None,
            enhanced_max_altitude: None,
            avg_lev_motor_power: None,
            max_lev_motor_power: None,
            lev_battery_consumption: None,
            avg_vertical_ratio: None,
            avg_stance_time_balance: None,
            avg_step_length: None,
            total_anaerobic_training_effect: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSession::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSession:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSession<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // event
                    let (val, outp) = FitFieldEvent::parse(inp)?;
                    inp = outp;
                    message.event = Some(val);
                    Ok(())
                },
            
                1 => {  // event_type
                    let (val, outp) = FitFieldEventType::parse(inp)?;
                    inp = outp;
                    message.event_type = Some(val);
                    Ok(())
                },
            
                2 => {  // start_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_time = Some(val);
                    Ok(())
                },
            
                3 => {  // start_position_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_position_lat = Some(val);
                    Ok(())
                },
            
                4 => {  // start_position_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_position_long = Some(val);
                    Ok(())
                },
            
                5 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                6 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                7 => {  // total_elapsed_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_elapsed_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                8 => {  // total_timer_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_timer_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                9 => {  // total_distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                10 => {  // total_cycles
                    let (val, outp) = FitMessageSessionSubfieldTotalCycles::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.total_cycles = Some(val);
                    Ok(())
                },
            
                11 => {  // total_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_calories = Some(val);
                    Ok(())
                },
            
                13 => {  // total_fat_calories
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_fat_calories = Some(val);
                    Ok(())
                },
            
                14 => {  // avg_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                15 => {  // max_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                16 => {  // avg_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_heart_rate = Some(val);
                    Ok(())
                },
            
                17 => {  // max_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_heart_rate = Some(val);
                    Ok(())
                },
            
                18 => {  // avg_cadence
                    let (val, outp) = FitMessageSessionSubfieldAvgCadence::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.avg_cadence = Some(val);
                    Ok(())
                },
            
                19 => {  // max_cadence
                    let (val, outp) = FitMessageSessionSubfieldMaxCadence::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.max_cadence = Some(val);
                    Ok(())
                },
            
                20 => {  // avg_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_power = Some(val);
                    Ok(())
                },
            
                21 => {  // max_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_power = Some(val);
                    Ok(())
                },
            
                22 => {  // total_ascent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_ascent = Some(val);
                    Ok(())
                },
            
                23 => {  // total_descent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_descent = Some(val);
                    Ok(())
                },
            
                24 => {  // total_training_effect
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.total_training_effect = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                25 => {  // first_lap_index
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.first_lap_index = Some(val);
                    Ok(())
                },
            
                26 => {  // num_laps
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.num_laps = Some(val);
                    Ok(())
                },
            
                27 => {  // event_group
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.event_group = Some(val);
                    Ok(())
                },
            
                28 => {  // trigger
                    let (val, outp) = FitFieldSessionTrigger::parse(inp)?;
                    inp = outp;
                    message.trigger = Some(val);
                    Ok(())
                },
            
                29 => {  // nec_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.nec_lat = Some(val);
                    Ok(())
                },
            
                30 => {  // nec_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.nec_long = Some(val);
                    Ok(())
                },
            
                31 => {  // swc_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.swc_lat = Some(val);
                    Ok(())
                },
            
                32 => {  // swc_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.swc_long = Some(val);
                    Ok(())
                },
            
                34 => {  // normalized_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.normalized_power = Some(val);
                    Ok(())
                },
            
                35 => {  // training_stress_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.training_stress_score = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                36 => {  // intensity_factor
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.intensity_factor = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                37 => {  // left_right_balance
                    let (val, outp) = FitFieldLeftRightBalance100::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.left_right_balance = Some(val);
                    Ok(())
                },
            
                41 => {  // avg_stroke_count
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stroke_count = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                42 => {  // avg_stroke_distance
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stroke_distance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                43 => {  // swim_stroke
                    let (val, outp) = FitFieldSwimStroke::parse(inp)?;
                    inp = outp;
                    message.swim_stroke = Some(val);
                    Ok(())
                },
            
                44 => {  // pool_length
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.pool_length = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                45 => {  // threshold_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.threshold_power = Some(val);
                    Ok(())
                },
            
                46 => {  // pool_length_unit
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.pool_length_unit = Some(val);
                    Ok(())
                },
            
                47 => {  // num_active_lengths
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.num_active_lengths = Some(val);
                    Ok(())
                },
            
                48 => {  // total_work
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_work = Some(val);
                    Ok(())
                },
            
                49 => {  // avg_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                50 => {  // max_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                51 => {  // gps_accuracy
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.gps_accuracy = Some(val);
                    Ok(())
                },
            
                52 => {  // avg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                53 => {  // avg_pos_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_pos_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                54 => {  // avg_neg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_neg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                55 => {  // max_pos_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_pos_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                56 => {  // max_neg_grade
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_neg_grade = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                57 => {  // avg_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_temperature = Some(val);
                    Ok(())
                },
            
                58 => {  // max_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.max_temperature = Some(val);
                    Ok(())
                },
            
                59 => {  // total_moving_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.total_moving_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                60 => {  // avg_pos_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_pos_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                61 => {  // avg_neg_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_neg_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                62 => {  // max_pos_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_pos_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                63 => {  // max_neg_vertical_speed
                    let (val, outp) = parse_sint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_neg_vertical_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                64 => {  // min_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.min_heart_rate = Some(val);
                    Ok(())
                },
            
                65 => {  // time_in_hr_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_hr_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                66 => {  // time_in_speed_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_speed_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                67 => {  // time_in_cadence_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_cadence_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                68 => {  // time_in_power_zone
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_in_power_zone = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                69 => {  // avg_lap_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_lap_time = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                70 => {  // best_lap_index
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.best_lap_index = Some(val);
                    Ok(())
                },
            
                71 => {  // min_altitude
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                82 => {  // player_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.player_score = Some(val);
                    Ok(())
                },
            
                83 => {  // opponent_score
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.opponent_score = Some(val);
                    Ok(())
                },
            
                84 => {  // opponent_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.opponent_name = Some(val);
                    Ok(())
                },
            
                85 => {  // stroke_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stroke_count = Some(val);
                    Ok(())
                },
            
                86 => {  // zone_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.zone_count = Some(val);
                    Ok(())
                },
            
                87 => {  // max_ball_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_ball_speed = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                88 => {  // avg_ball_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_ball_speed = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                89 => {  // avg_vertical_oscillation
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_vertical_oscillation = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                90 => {  // avg_stance_time_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stance_time_percent = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                91 => {  // avg_stance_time
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stance_time = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                92 => {  // avg_fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                93 => {  // max_fractional_cadence
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_fractional_cadence = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                94 => {  // total_fractional_cycles
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.total_fractional_cycles = Some(val as f64 / 128 as f64);
                    Ok(())
                },
            
                95 => {  // avg_total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                96 => {  // min_total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                97 => {  // max_total_hemoglobin_conc
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_total_hemoglobin_conc = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                98 => {  // avg_saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                99 => {  // min_saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.min_saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                100 => {  // max_saturated_hemoglobin_percent
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_saturated_hemoglobin_percent = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                101 => {  // avg_left_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                102 => {  // avg_right_torque_effectiveness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_torque_effectiveness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                103 => {  // avg_left_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                104 => {  // avg_right_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                105 => {  // avg_combined_pedal_smoothness
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_combined_pedal_smoothness = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                111 => {  // sport_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.sport_index = Some(val);
                    Ok(())
                },
            
                112 => {  // time_standing
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_standing = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                113 => {  // stand_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.stand_count = Some(val);
                    Ok(())
                },
            
                114 => {  // avg_left_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_left_pco = Some(val);
                    Ok(())
                },
            
                115 => {  // avg_right_pco
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.avg_right_pco = Some(val);
                    Ok(())
                },
            
                116 => {  // avg_left_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                117 => {  // avg_left_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_left_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                118 => {  // avg_right_power_phase
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_power_phase = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                119 => {  // avg_right_power_phase_peak
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_right_power_phase_peak = Some(val as f64 / 0.7111111 as f64);
                    Ok(())
                },
            
                120 => {  // avg_power_position
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_power_position = Some(val);
                    Ok(())
                },
            
                121 => {  // max_power_position
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_power_position = Some(val);
                    Ok(())
                },
            
                122 => {  // avg_cadence_position
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.avg_cadence_position = Some(val);
                    Ok(())
                },
            
                123 => {  // max_cadence_position
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_cadence_position = Some(val);
                    Ok(())
                },
            
                124 => {  // enhanced_avg_speed
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_avg_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                125 => {  // enhanced_max_speed
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_max_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                126 => {  // enhanced_avg_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_avg_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                127 => {  // enhanced_min_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_min_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                128 => {  // enhanced_max_altitude
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.enhanced_max_altitude = Some((val as f64 / 5 as f64) - (500 as f64));
                    Ok(())
                },
            
                129 => {  // avg_lev_motor_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_lev_motor_power = Some(val);
                    Ok(())
                },
            
                130 => {  // max_lev_motor_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.max_lev_motor_power = Some(val);
                    Ok(())
                },
            
                131 => {  // lev_battery_consumption
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.lev_battery_consumption = Some(val as f64 / 2 as f64);
                    Ok(())
                },
            
                132 => {  // avg_vertical_ratio
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_vertical_ratio = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                133 => {  // avg_stance_time_balance
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_stance_time_balance = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                134 => {  // avg_step_length
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.avg_step_length = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                137 => {  // total_anaerobic_training_effect
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.total_anaerobic_training_effect = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageSlaveDeviceSubfieldProduct {
    Default(u16),
    GarminProduct(FitFieldGarminProduct),
}

impl FitMessageSlaveDeviceSubfieldProduct {
    fn parse<'a>(message: &FitMessageSlaveDevice<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageSlaveDeviceSubfieldProduct,  &'a [u8])> {
        
        match message.manufacturer {
        
            Some(FitFieldManufacturer::Garmin) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageSlaveDeviceSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::Dynastream) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageSlaveDeviceSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::DynastreamOem) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageSlaveDeviceSubfieldProduct::GarminProduct(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
        Ok((FitMessageSlaveDeviceSubfieldProduct::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageSlaveDevice<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub manufacturer: Option<FitFieldManufacturer>,  
    pub product: Option<FitMessageSlaveDeviceSubfieldProduct>,  
    
}
impl<'a> FitMessageSlaveDevice<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSlaveDevice<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSlaveDevice {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            manufacturer: None,
            product: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSlaveDevice::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSlaveDevice:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSlaveDevice<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // manufacturer
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer = Some(val);
                    Ok(())
                },
            
                1 => {  // product
                    let (val, outp) = FitMessageSlaveDeviceSubfieldProduct::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.product = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSoftware<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub version: Option<f64>,  
    pub part_number: Option<String>,  
    
}
impl<'a> FitMessageSoftware<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSoftware<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSoftware {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            version: None,
            part_number: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSoftware::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSoftware:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSoftware<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                3 => {  // version
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.version = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                5 => {  // part_number
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.part_number = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSpeedZone<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub high_value: Option<f64>,  
    pub name: Option<String>,  
    
}
impl<'a> FitMessageSpeedZone<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSpeedZone<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSpeedZone {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            high_value: None,
            name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSpeedZone::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSpeedZone:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSpeedZone<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // high_value
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.high_value = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                1 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageSport<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub sport: Option<FitFieldSport>,  
    pub sub_sport: Option<FitFieldSubSport>,  
    pub name: Option<String>,  
    
}
impl<'a> FitMessageSport<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageSport<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageSport {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            sport: None,
            sub_sport: None,
            name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageSport::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageSport:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageSport<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                1 => {  // sub_sport
                    let (val, outp) = FitFieldSubSport::parse(inp)?;
                    inp = outp;
                    message.sub_sport = Some(val);
                    Ok(())
                },
            
                3 => {  // name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor {
    Default(u32),
    GyroCalFactor(u32),
    AccelCalFactor(u32),
}

impl FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor {
    fn parse<'a>(message: &FitMessageThreeDSensorCalibration<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor,  &'a [u8])> {
        
        match message.sensor_type {
        
            Some(FitFieldSensorType::Accelerometer) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor::AccelCalFactor(val), o))
            },
        
            Some(FitFieldSensorType::Gyroscope) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor::GyroCalFactor(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageThreeDSensorCalibration<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp
    pub sensor_type: Option<FitFieldSensorType>,  // Indicates which sensor the calibration is for
    pub calibration_factor: Option<FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor>,  // Calibration factor used to convert from raw ADC value to degrees, g,  etc.
    pub calibration_divisor: Option<u32>,  // Calibration factor divisor
    pub level_shift: Option<u32>,  // Level shift value used to shift the ADC value back into range
    pub offset_cal: Option<i32>,  // Internal calibration factors, one for each: xy, yx, zx
    pub orientation_matrix: Option<f64>,  // 3 x 3 rotation matrix (row major)
    
}
impl<'a> FitMessageThreeDSensorCalibration<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageThreeDSensorCalibration<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageThreeDSensorCalibration {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            sensor_type: None,
            calibration_factor: None,
            calibration_divisor: None,
            level_shift: None,
            offset_cal: None,
            orientation_matrix: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageThreeDSensorCalibration::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageThreeDSensorCalibration:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageThreeDSensorCalibration<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // sensor_type
                    let (val, outp) = FitFieldSensorType::parse(inp)?;
                    inp = outp;
                    message.sensor_type = Some(val);
                    Ok(())
                },
            
                1 => {  // calibration_factor
                    let (val, outp) = FitMessageThreeDSensorCalibrationSubfieldCalibrationFactor::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.calibration_factor = Some(val);
                    Ok(())
                },
            
                2 => {  // calibration_divisor
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calibration_divisor = Some(val);
                    Ok(())
                },
            
                3 => {  // level_shift
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.level_shift = Some(val);
                    Ok(())
                },
            
                4 => {  // offset_cal
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.offset_cal = Some(val);
                    Ok(())
                },
            
                5 => {  // orientation_matrix
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.orientation_matrix = Some(val as f64 / 65535 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageTimestampCorrelation<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of UTC timestamp at the time the system timestamp was recorded.
    pub fractional_timestamp: Option<f64>,  // Fractional part of the UTC timestamp at the time the system timestamp was recorded.
    pub system_timestamp: Option<FitFieldDateTime>,  // Whole second part of the system timestamp
    pub fractional_system_timestamp: Option<f64>,  // Fractional part of the system timestamp
    pub local_timestamp: Option<FitFieldLocalDateTime>,  // timestamp epoch expressed in local time used to convert timestamps to local time 
    pub timestamp_ms: Option<u16>,  // Millisecond part of the UTC timestamp at the time the system timestamp was recorded.
    pub system_timestamp_ms: Option<u16>,  // Millisecond part of the system timestamp
    
}
impl<'a> FitMessageTimestampCorrelation<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageTimestampCorrelation<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageTimestampCorrelation {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            fractional_timestamp: None,
            system_timestamp: None,
            fractional_system_timestamp: None,
            local_timestamp: None,
            timestamp_ms: None,
            system_timestamp_ms: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageTimestampCorrelation::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageTimestampCorrelation:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageTimestampCorrelation<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // fractional_timestamp
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.fractional_timestamp = Some(val as f64 / 32768 as f64);
                    Ok(())
                },
            
                1 => {  // system_timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.system_timestamp = Some(val);
                    Ok(())
                },
            
                2 => {  // fractional_system_timestamp
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.fractional_system_timestamp = Some(val as f64 / 32768 as f64);
                    Ok(())
                },
            
                3 => {  // local_timestamp
                    let (val, outp) = FitFieldLocalDateTime::parse(inp, message.definition_message.endianness, tz_offset)?;
                    inp = outp;
                    message.local_timestamp = Some(val);
                    Ok(())
                },
            
                4 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                5 => {  // system_timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.system_timestamp_ms = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageTotals<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub timestamp: Option<FitFieldDateTime>,  
    pub timer_time: Option<u32>,  // Excludes pauses
    pub distance: Option<u32>,  
    pub calories: Option<u32>,  
    pub sport: Option<FitFieldSport>,  
    pub elapsed_time: Option<u32>,  // Includes pauses
    pub sessions: Option<u16>,  
    pub active_time: Option<u32>,  
    pub sport_index: Option<u8>,  
    
}
impl<'a> FitMessageTotals<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageTotals<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageTotals {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            timestamp: None,
            timer_time: None,
            distance: None,
            calories: None,
            sport: None,
            elapsed_time: None,
            sessions: None,
            active_time: None,
            sport_index: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageTotals::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageTotals:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageTotals<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timer_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timer_time = Some(val);
                    Ok(())
                },
            
                1 => {  // distance
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.distance = Some(val);
                    Ok(())
                },
            
                2 => {  // calories
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.calories = Some(val);
                    Ok(())
                },
            
                3 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                4 => {  // elapsed_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.elapsed_time = Some(val);
                    Ok(())
                },
            
                5 => {  // sessions
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sessions = Some(val);
                    Ok(())
                },
            
                6 => {  // active_time
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.active_time = Some(val);
                    Ok(())
                },
            
                9 => {  // sport_index
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.sport_index = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageTrainingFileSubfieldProduct {
    Default(u16),
    GarminProduct(FitFieldGarminProduct),
}

impl FitMessageTrainingFileSubfieldProduct {
    fn parse<'a>(message: &FitMessageTrainingFile<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageTrainingFileSubfieldProduct,  &'a [u8])> {
        
        match message.manufacturer {
        
            Some(FitFieldManufacturer::Garmin) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageTrainingFileSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::Dynastream) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageTrainingFileSubfieldProduct::GarminProduct(val), o))
            },
        
            Some(FitFieldManufacturer::DynastreamOem) => {
                let (val, o) = FitFieldGarminProduct::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageTrainingFileSubfieldProduct::GarminProduct(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint16(inp, message.definition_message.endianness)?;
        Ok((FitMessageTrainingFileSubfieldProduct::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageTrainingFile<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub ftype: Option<FitFieldFile>,  
    pub manufacturer: Option<FitFieldManufacturer>,  
    pub product: Option<FitMessageTrainingFileSubfieldProduct>,  
    pub serial_number: Option<u32>,  
    pub time_created: Option<FitFieldDateTime>,  
    
}
impl<'a> FitMessageTrainingFile<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageTrainingFile<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageTrainingFile {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            ftype: None,
            manufacturer: None,
            product: None,
            serial_number: None,
            time_created: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageTrainingFile::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageTrainingFile:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageTrainingFile<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // ftype
                    let (val, outp) = FitFieldFile::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                1 => {  // manufacturer
                    let (val, outp) = FitFieldManufacturer::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.manufacturer = Some(val);
                    Ok(())
                },
            
                2 => {  // product
                    let (val, outp) = FitMessageTrainingFileSubfieldProduct::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.product = Some(val);
                    Ok(())
                },
            
                3 => {  // serial_number
                    let (val, outp) = parse_uint32z(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.serial_number = val;
                    Ok(())
                },
            
                4 => {  // time_created
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.time_created = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageUserProfile<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub friendly_name: Option<String>,  
    pub gender: Option<FitFieldGender>,  
    pub age: Option<u8>,  
    pub height: Option<f64>,  
    pub weight: Option<f64>,  
    pub language: Option<FitFieldLanguage>,  
    pub elev_setting: Option<FitFieldDisplayMeasure>,  
    pub weight_setting: Option<FitFieldDisplayMeasure>,  
    pub resting_heart_rate: Option<u8>,  
    pub default_max_running_heart_rate: Option<u8>,  
    pub default_max_biking_heart_rate: Option<u8>,  
    pub default_max_heart_rate: Option<u8>,  
    pub hr_setting: Option<FitFieldDisplayHeart>,  
    pub speed_setting: Option<FitFieldDisplayMeasure>,  
    pub dist_setting: Option<FitFieldDisplayMeasure>,  
    pub power_setting: Option<FitFieldDisplayPower>,  
    pub activity_class: Option<FitFieldActivityClass>,  
    pub position_setting: Option<FitFieldDisplayPosition>,  
    pub temperature_setting: Option<FitFieldDisplayMeasure>,  
    pub local_id: Option<FitFieldUserLocalId>,  
    pub global_id: Option<&'a [u8]>,  
    pub wake_time: Option<FitFieldLocaltimeIntoDay>,  // Typical wake time
    pub sleep_time: Option<FitFieldLocaltimeIntoDay>,  // Typical bed time
    pub height_setting: Option<FitFieldDisplayMeasure>,  
    pub user_running_step_length: Option<f64>,  // User defined running step length set to 0 for auto length
    pub user_walking_step_length: Option<f64>,  // User defined walking step length set to 0 for auto length
    
}
impl<'a> FitMessageUserProfile<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageUserProfile<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageUserProfile {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            friendly_name: None,
            gender: None,
            age: None,
            height: None,
            weight: None,
            language: None,
            elev_setting: None,
            weight_setting: None,
            resting_heart_rate: None,
            default_max_running_heart_rate: None,
            default_max_biking_heart_rate: None,
            default_max_heart_rate: None,
            hr_setting: None,
            speed_setting: None,
            dist_setting: None,
            power_setting: None,
            activity_class: None,
            position_setting: None,
            temperature_setting: None,
            local_id: None,
            global_id: None,
            wake_time: None,
            sleep_time: None,
            height_setting: None,
            user_running_step_length: None,
            user_walking_step_length: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageUserProfile::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageUserProfile:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageUserProfile<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // friendly_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.friendly_name = Some(val);
                    Ok(())
                },
            
                1 => {  // gender
                    let (val, outp) = FitFieldGender::parse(inp)?;
                    inp = outp;
                    message.gender = Some(val);
                    Ok(())
                },
            
                2 => {  // age
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.age = Some(val);
                    Ok(())
                },
            
                3 => {  // height
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.height = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                4 => {  // weight
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.weight = Some(val as f64 / 10 as f64);
                    Ok(())
                },
            
                5 => {  // language
                    let (val, outp) = FitFieldLanguage::parse(inp)?;
                    inp = outp;
                    message.language = Some(val);
                    Ok(())
                },
            
                6 => {  // elev_setting
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.elev_setting = Some(val);
                    Ok(())
                },
            
                7 => {  // weight_setting
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.weight_setting = Some(val);
                    Ok(())
                },
            
                8 => {  // resting_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.resting_heart_rate = Some(val);
                    Ok(())
                },
            
                9 => {  // default_max_running_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.default_max_running_heart_rate = Some(val);
                    Ok(())
                },
            
                10 => {  // default_max_biking_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.default_max_biking_heart_rate = Some(val);
                    Ok(())
                },
            
                11 => {  // default_max_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.default_max_heart_rate = Some(val);
                    Ok(())
                },
            
                12 => {  // hr_setting
                    let (val, outp) = FitFieldDisplayHeart::parse(inp)?;
                    inp = outp;
                    message.hr_setting = Some(val);
                    Ok(())
                },
            
                13 => {  // speed_setting
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.speed_setting = Some(val);
                    Ok(())
                },
            
                14 => {  // dist_setting
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.dist_setting = Some(val);
                    Ok(())
                },
            
                16 => {  // power_setting
                    let (val, outp) = FitFieldDisplayPower::parse(inp)?;
                    inp = outp;
                    message.power_setting = Some(val);
                    Ok(())
                },
            
                17 => {  // activity_class
                    let (val, outp) = FitFieldActivityClass::parse(inp)?;
                    inp = outp;
                    message.activity_class = Some(val);
                    Ok(())
                },
            
                18 => {  // position_setting
                    let (val, outp) = FitFieldDisplayPosition::parse(inp)?;
                    inp = outp;
                    message.position_setting = Some(val);
                    Ok(())
                },
            
                21 => {  // temperature_setting
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.temperature_setting = Some(val);
                    Ok(())
                },
            
                22 => {  // local_id
                    let (val, outp) = FitFieldUserLocalId::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.local_id = Some(val);
                    Ok(())
                },
            
                23 => {  // global_id
                    let (val, outp) = parse_byte(inp, field.field_size)?;
                    inp = outp;
                    message.global_id = Some(val);
                    Ok(())
                },
            
                28 => {  // wake_time
                    let (val, outp) = FitFieldLocaltimeIntoDay::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.wake_time = Some(val);
                    Ok(())
                },
            
                29 => {  // sleep_time
                    let (val, outp) = FitFieldLocaltimeIntoDay::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.sleep_time = Some(val);
                    Ok(())
                },
            
                30 => {  // height_setting
                    let (val, outp) = FitFieldDisplayMeasure::parse(inp)?;
                    inp = outp;
                    message.height_setting = Some(val);
                    Ok(())
                },
            
                31 => {  // user_running_step_length
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.user_running_step_length = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                32 => {  // user_walking_step_length
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.user_walking_step_length = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageVideo<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub url: Option<String>,  
    pub hosting_provider: Option<String>,  
    pub duration: Option<u32>,  // Playback time of video
    
}
impl<'a> FitMessageVideo<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageVideo<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageVideo {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            url: None,
            hosting_provider: None,
            duration: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageVideo::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageVideo:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageVideo<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // url
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.url = Some(val);
                    Ok(())
                },
            
                1 => {  // hosting_provider
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.hosting_provider = Some(val);
                    Ok(())
                },
            
                2 => {  // duration
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.duration = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageVideoClip<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub clip_number: Option<u16>,  
    pub start_timestamp: Option<FitFieldDateTime>,  
    pub start_timestamp_ms: Option<u16>,  
    pub end_timestamp: Option<FitFieldDateTime>,  
    pub end_timestamp_ms: Option<u16>,  
    pub clip_start: Option<u32>,  // Start of clip in video time
    pub clip_end: Option<u32>,  // End of clip in video time
    
}
impl<'a> FitMessageVideoClip<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageVideoClip<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageVideoClip {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            clip_number: None,
            start_timestamp: None,
            start_timestamp_ms: None,
            end_timestamp: None,
            end_timestamp_ms: None,
            clip_start: None,
            clip_end: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageVideoClip::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageVideoClip:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageVideoClip<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                0 => {  // clip_number
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.clip_number = Some(val);
                    Ok(())
                },
            
                1 => {  // start_timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_timestamp = Some(val);
                    Ok(())
                },
            
                2 => {  // start_timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.start_timestamp_ms = Some(val);
                    Ok(())
                },
            
                3 => {  // end_timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_timestamp = Some(val);
                    Ok(())
                },
            
                4 => {  // end_timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.end_timestamp_ms = Some(val);
                    Ok(())
                },
            
                6 => {  // clip_start
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.clip_start = Some(val);
                    Ok(())
                },
            
                7 => {  // clip_end
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.clip_end = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageVideoDescription<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  // Long descriptions will be split into multiple parts
    pub message_count: Option<u16>,  // Total number of description parts
    pub text: Option<String>,  
    
}
impl<'a> FitMessageVideoDescription<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageVideoDescription<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageVideoDescription {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            message_count: None,
            text: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageVideoDescription::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageVideoDescription:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageVideoDescription<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // message_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_count = Some(val);
                    Ok(())
                },
            
                1 => {  // text
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.text = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageVideoFrame<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // Whole second part of the timestamp
    pub timestamp_ms: Option<u16>,  // Millisecond part of the timestamp.
    pub frame_number: Option<u32>,  // Number of the frame that the timestamp and timestamp_ms correlate to
    
}
impl<'a> FitMessageVideoFrame<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageVideoFrame<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageVideoFrame {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            timestamp_ms: None,
            frame_number: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageVideoFrame::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageVideoFrame:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageVideoFrame<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // timestamp_ms
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp_ms = Some(val);
                    Ok(())
                },
            
                1 => {  // frame_number
                    let (val, outp) = parse_uint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.frame_number = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageVideoTitle<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  // Long titles will be split into multiple parts
    pub message_count: Option<u16>,  // Total number of title parts
    pub text: Option<String>,  
    
}
impl<'a> FitMessageVideoTitle<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageVideoTitle<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageVideoTitle {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            message_count: None,
            text: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageVideoTitle::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageVideoTitle:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageVideoTitle<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // message_count
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_count = Some(val);
                    Ok(())
                },
            
                1 => {  // text
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.text = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageWatchfaceSettingsSubfieldLayout<'a> {
    Default(&'a [u8]),
    AnalogLayout(FitFieldAnalogWatchfaceLayout),
    DigitalLayout(FitFieldDigitalWatchfaceLayout),
}

impl<'a> FitMessageWatchfaceSettingsSubfieldLayout<'a> {
    fn parse(message: &FitMessageWatchfaceSettings<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageWatchfaceSettingsSubfieldLayout<'a>,  &'a [u8])> {
        
        match message.mode {
        
            Some(FitFieldWatchfaceMode::Digital) => {
                let (val, o) = FitFieldDigitalWatchfaceLayout::parse(inp)?;
                return Ok((FitMessageWatchfaceSettingsSubfieldLayout::DigitalLayout(val), o))
            },
        
            Some(FitFieldWatchfaceMode::Analog) => {
                let (val, o) = FitFieldAnalogWatchfaceLayout::parse(inp)?;
                return Ok((FitMessageWatchfaceSettingsSubfieldLayout::AnalogLayout(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_byte(inp, field.field_size)?;
        Ok((FitMessageWatchfaceSettingsSubfieldLayout::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageWatchfaceSettings<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub mode: Option<FitFieldWatchfaceMode>,  
    pub layout: Option<FitMessageWatchfaceSettingsSubfieldLayout<'a>>,  
    
}
impl<'a> FitMessageWatchfaceSettings<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageWatchfaceSettings<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageWatchfaceSettings {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            mode: None,
            layout: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageWatchfaceSettings::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageWatchfaceSettings:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageWatchfaceSettings<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // mode
                    let (val, outp) = FitFieldWatchfaceMode::parse(inp)?;
                    inp = outp;
                    message.mode = Some(val);
                    Ok(())
                },
            
                1 => {  // layout
                    let (val, outp) = FitMessageWatchfaceSettingsSubfieldLayout::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.layout = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageWeatherAlert<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub report_id: Option<String>,  // Unique identifier from GCS report ID string, length is 12
    pub issue_time: Option<FitFieldDateTime>,  // Time alert was issued
    pub expire_time: Option<FitFieldDateTime>,  // Time alert expires
    pub severity: Option<FitFieldWeatherSeverity>,  // Warning, Watch, Advisory, Statement
    pub ftype: Option<FitFieldWeatherSevereType>,  // Tornado, Severe Thunderstorm, etc.
    
}
impl<'a> FitMessageWeatherAlert<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageWeatherAlert<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageWeatherAlert {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            report_id: None,
            issue_time: None,
            expire_time: None,
            severity: None,
            ftype: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageWeatherAlert::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageWeatherAlert:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageWeatherAlert<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // report_id
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.report_id = Some(val);
                    Ok(())
                },
            
                1 => {  // issue_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.issue_time = Some(val);
                    Ok(())
                },
            
                2 => {  // expire_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.expire_time = Some(val);
                    Ok(())
                },
            
                3 => {  // severity
                    let (val, outp) = FitFieldWeatherSeverity::parse(inp)?;
                    inp = outp;
                    message.severity = Some(val);
                    Ok(())
                },
            
                4 => {  // ftype
                    let (val, outp) = FitFieldWeatherSevereType::parse(inp)?;
                    inp = outp;
                    message.ftype = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageWeatherConditions<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  // time of update for current conditions, else forecast time
    pub weather_report: Option<FitFieldWeatherReport>,  // Current or forecast
    pub temperature: Option<i8>,  
    pub condition: Option<FitFieldWeatherStatus>,  // Corresponds to GSC Response weatherIcon field
    pub wind_direction: Option<u16>,  
    pub wind_speed: Option<f64>,  
    pub precipitation_probability: Option<u8>,  // range 0-100
    pub temperature_feels_like: Option<i8>,  // Heat Index if  GCS heatIdx above or equal to 90F or wind chill if GCS windChill below or equal to 32F
    pub relative_humidity: Option<u8>,  
    pub location: Option<String>,  // string corresponding to GCS response location string
    pub observed_at_time: Option<FitFieldDateTime>,  
    pub observed_location_lat: Option<i32>,  
    pub observed_location_long: Option<i32>,  
    pub day_of_week: Option<FitFieldDayOfWeek>,  
    pub high_temperature: Option<i8>,  
    pub low_temperature: Option<i8>,  
    
}
impl<'a> FitMessageWeatherConditions<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageWeatherConditions<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageWeatherConditions {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            weather_report: None,
            temperature: None,
            condition: None,
            wind_direction: None,
            wind_speed: None,
            precipitation_probability: None,
            temperature_feels_like: None,
            relative_humidity: None,
            location: None,
            observed_at_time: None,
            observed_location_lat: None,
            observed_location_long: None,
            day_of_week: None,
            high_temperature: None,
            low_temperature: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageWeatherConditions::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageWeatherConditions:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageWeatherConditions<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // weather_report
                    let (val, outp) = FitFieldWeatherReport::parse(inp)?;
                    inp = outp;
                    message.weather_report = Some(val);
                    Ok(())
                },
            
                1 => {  // temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.temperature = Some(val);
                    Ok(())
                },
            
                2 => {  // condition
                    let (val, outp) = FitFieldWeatherStatus::parse(inp)?;
                    inp = outp;
                    message.condition = Some(val);
                    Ok(())
                },
            
                3 => {  // wind_direction
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.wind_direction = Some(val);
                    Ok(())
                },
            
                4 => {  // wind_speed
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.wind_speed = Some(val as f64 / 1000 as f64);
                    Ok(())
                },
            
                5 => {  // precipitation_probability
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.precipitation_probability = Some(val);
                    Ok(())
                },
            
                6 => {  // temperature_feels_like
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.temperature_feels_like = Some(val);
                    Ok(())
                },
            
                7 => {  // relative_humidity
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.relative_humidity = Some(val);
                    Ok(())
                },
            
                8 => {  // location
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.location = Some(val);
                    Ok(())
                },
            
                9 => {  // observed_at_time
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.observed_at_time = Some(val);
                    Ok(())
                },
            
                10 => {  // observed_location_lat
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.observed_location_lat = Some(val);
                    Ok(())
                },
            
                11 => {  // observed_location_long
                    let (val, outp) = parse_sint32(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.observed_location_long = Some(val);
                    Ok(())
                },
            
                12 => {  // day_of_week
                    let (val, outp) = FitFieldDayOfWeek::parse(inp)?;
                    inp = outp;
                    message.day_of_week = Some(val);
                    Ok(())
                },
            
                13 => {  // high_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.high_temperature = Some(val);
                    Ok(())
                },
            
                14 => {  // low_temperature
                    let (val, outp) = parse_sint8(inp)?;
                    inp = outp;
                    message.low_temperature = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageWeightScale<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub timestamp: Option<FitFieldDateTime>,  
    pub weight: Option<FitFieldWeight>,  
    pub percent_fat: Option<f64>,  
    pub percent_hydration: Option<f64>,  
    pub visceral_fat_mass: Option<f64>,  
    pub bone_mass: Option<f64>,  
    pub muscle_mass: Option<f64>,  
    pub basal_met: Option<f64>,  
    pub physique_rating: Option<u8>,  
    pub active_met: Option<f64>,  // ~4kJ per kcal, 0.25 allows max 16384 kcal
    pub metabolic_age: Option<u8>,  
    pub visceral_fat_rating: Option<u8>,  
    pub user_profile_index: Option<FitFieldMessageIndex>,  // Associates this weight scale message to a user.  This corresponds to the index of the user profile message in the weight scale file.
    
}
impl<'a> FitMessageWeightScale<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageWeightScale<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageWeightScale {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            timestamp: None,
            weight: None,
            percent_fat: None,
            percent_hydration: None,
            visceral_fat_mass: None,
            bone_mass: None,
            muscle_mass: None,
            basal_met: None,
            physique_rating: None,
            active_met: None,
            metabolic_age: None,
            visceral_fat_rating: None,
            user_profile_index: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageWeightScale::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageWeightScale:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageWeightScale<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                253 => {  // timestamp
                    let (val, outp) = FitFieldDateTime::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.timestamp = Some(val);
                    Ok(())
                },
            
                0 => {  // weight
                    let (val, outp) = FitFieldWeight::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.weight = Some(val);
                    Ok(())
                },
            
                1 => {  // percent_fat
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.percent_fat = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                2 => {  // percent_hydration
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.percent_hydration = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                3 => {  // visceral_fat_mass
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.visceral_fat_mass = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                4 => {  // bone_mass
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.bone_mass = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                5 => {  // muscle_mass
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.muscle_mass = Some(val as f64 / 100 as f64);
                    Ok(())
                },
            
                7 => {  // basal_met
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.basal_met = Some(val as f64 / 4 as f64);
                    Ok(())
                },
            
                8 => {  // physique_rating
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.physique_rating = Some(val);
                    Ok(())
                },
            
                9 => {  // active_met
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.active_met = Some(val as f64 / 4 as f64);
                    Ok(())
                },
            
                10 => {  // metabolic_age
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.metabolic_age = Some(val);
                    Ok(())
                },
            
                11 => {  // visceral_fat_rating
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.visceral_fat_rating = Some(val);
                    Ok(())
                },
            
                12 => {  // user_profile_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.user_profile_index = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageWorkout<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub sport: Option<FitFieldSport>,  
    pub capabilities: Option<FitFieldWorkoutCapabilities>,  
    pub num_valid_steps: Option<u16>,  // number of valid steps
    pub wkt_name: Option<String>,  
    
}
impl<'a> FitMessageWorkout<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageWorkout<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageWorkout {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            sport: None,
            capabilities: None,
            num_valid_steps: None,
            wkt_name: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageWorkout::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageWorkout:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageWorkout<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                4 => {  // sport
                    let (val, outp) = FitFieldSport::parse(inp)?;
                    inp = outp;
                    message.sport = Some(val);
                    Ok(())
                },
            
                5 => {  // capabilities
                    let (val, outp) = FitFieldWorkoutCapabilities::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.capabilities = Some(val);
                    Ok(())
                },
            
                6 => {  // num_valid_steps
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.num_valid_steps = Some(val);
                    Ok(())
                },
            
                8 => {  // wkt_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.wkt_name = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitMessageWorkoutStepSubfieldDurationValue {
    Default(u32),
    DurationDistance(u32),
    DurationTime(u32),
    DurationPower(FitFieldWorkoutPower),
    DurationStep(u32),
    DurationHr(FitFieldWorkoutHr),
    DurationCalories(u32),
}

impl FitMessageWorkoutStepSubfieldDurationValue {
    fn parse<'a>(message: &FitMessageWorkoutStep<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageWorkoutStepSubfieldDurationValue,  &'a [u8])> {
        
        match message.duration_type {
        
            Some(FitFieldWktStepDuration::Time) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationTime(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepetitionTime) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationTime(val), o))
            },
        
            Some(FitFieldWktStepDuration::Distance) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationDistance(val), o))
            },
        
            Some(FitFieldWktStepDuration::HrLessThan) => {
                let (val, o) = FitFieldWorkoutHr::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationHr(val), o))
            },
        
            Some(FitFieldWktStepDuration::HrGreaterThan) => {
                let (val, o) = FitFieldWorkoutHr::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationHr(val), o))
            },
        
            Some(FitFieldWktStepDuration::Calories) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationCalories(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilStepsCmplt) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilTime) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilDistance) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilCalories) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilHrLessThan) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilHrGreaterThan) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilPowerLessThan) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilPowerGreaterThan) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationStep(val), o))
            },
        
            Some(FitFieldWktStepDuration::PowerLessThan) => {
                let (val, o) = FitFieldWorkoutPower::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationPower(val), o))
            },
        
            Some(FitFieldWktStepDuration::PowerGreaterThan) => {
                let (val, o) = FitFieldWorkoutPower::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldDurationValue::DurationPower(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageWorkoutStepSubfieldDurationValue::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageWorkoutStepSubfieldTargetValue {
    Default(u32),
    RepeatHr(FitFieldWorkoutHr),
    RepeatCalories(u32),
    RepeatTime(u32),
    RepeatPower(FitFieldWorkoutPower),
    RepeatSteps(u32),
    TargetHrZone(u32),
    RepeatDistance(u32),
    TargetPowerZone(u32),
}

impl FitMessageWorkoutStepSubfieldTargetValue {
    fn parse<'a>(message: &FitMessageWorkoutStep<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageWorkoutStepSubfieldTargetValue,  &'a [u8])> {
        
        match message.duration_type {
        
            Some(FitFieldWktStepDuration::RepeatUntilStepsCmplt) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatSteps(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilTime) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatTime(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilDistance) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatDistance(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilCalories) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatCalories(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilHrLessThan) => {
                let (val, o) = FitFieldWorkoutHr::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatHr(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilHrGreaterThan) => {
                let (val, o) = FitFieldWorkoutHr::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatHr(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilPowerLessThan) => {
                let (val, o) = FitFieldWorkoutPower::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatPower(val), o))
            },
        
            Some(FitFieldWktStepDuration::RepeatUntilPowerGreaterThan) => {
                let (val, o) = FitFieldWorkoutPower::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::RepeatPower(val), o))
            },
        
            _ => (),
        }
        
        match message.target_type {
        
            Some(FitFieldWktStepTarget::HeartRate) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::TargetHrZone(val), o))
            },
        
            Some(FitFieldWktStepTarget::Power) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldTargetValue::TargetPowerZone(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageWorkoutStepSubfieldTargetValue::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageWorkoutStepSubfieldCustomTargetValueLow {
    Default(u32),
    CustomTargetHeartRateLow(FitFieldWorkoutHr),
    CustomTargetCadenceLow(u32),
    CustomTargetPowerLow(FitFieldWorkoutPower),
    CustomTargetSpeedLow(u32),
}

impl FitMessageWorkoutStepSubfieldCustomTargetValueLow {
    fn parse<'a>(message: &FitMessageWorkoutStep<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageWorkoutStepSubfieldCustomTargetValueLow,  &'a [u8])> {
        
        match message.target_type {
        
            Some(FitFieldWktStepTarget::Speed) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueLow::CustomTargetSpeedLow(val), o))
            },
        
            Some(FitFieldWktStepTarget::HeartRate) => {
                let (val, o) = FitFieldWorkoutHr::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueLow::CustomTargetHeartRateLow(val), o))
            },
        
            Some(FitFieldWktStepTarget::Cadence) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueLow::CustomTargetCadenceLow(val), o))
            },
        
            Some(FitFieldWktStepTarget::Power) => {
                let (val, o) = FitFieldWorkoutPower::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueLow::CustomTargetPowerLow(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageWorkoutStepSubfieldCustomTargetValueLow::Default(val),o))
    }
}
#[derive(Debug)]
pub enum FitMessageWorkoutStepSubfieldCustomTargetValueHigh {
    Default(u32),
    CustomTargetCadenceHigh(u32),
    CustomTargetPowerHigh(FitFieldWorkoutPower),
    CustomTargetSpeedHigh(u32),
    CustomTargetHeartRateHigh(FitFieldWorkoutHr),
}

impl FitMessageWorkoutStepSubfieldCustomTargetValueHigh {
    fn parse<'a>(message: &FitMessageWorkoutStep<'a>, inp: &'a [u8], field: &FitFieldDefinition, tz_offset: f64) -> Result<(FitMessageWorkoutStepSubfieldCustomTargetValueHigh,  &'a [u8])> {
        
        match message.target_type {
        
            Some(FitFieldWktStepTarget::Speed) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueHigh::CustomTargetSpeedHigh(val), o))
            },
        
            Some(FitFieldWktStepTarget::HeartRate) => {
                let (val, o) = FitFieldWorkoutHr::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueHigh::CustomTargetHeartRateHigh(val), o))
            },
        
            Some(FitFieldWktStepTarget::Cadence) => {
                let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueHigh::CustomTargetCadenceHigh(val), o))
            },
        
            Some(FitFieldWktStepTarget::Power) => {
                let (val, o) = FitFieldWorkoutPower::parse(inp, message.definition_message.endianness)?;
                return Ok((FitMessageWorkoutStepSubfieldCustomTargetValueHigh::CustomTargetPowerHigh(val), o))
            },
        
            _ => (),
        }
        
        let (val, o) = parse_uint32(inp, message.definition_message.endianness)?;
        Ok((FitMessageWorkoutStepSubfieldCustomTargetValueHigh::Default(val),o))
    }
}
#[derive(Debug)]
pub struct FitMessageWorkoutStep<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub message_index: Option<FitFieldMessageIndex>,  
    pub wkt_step_name: Option<String>,  
    pub duration_type: Option<FitFieldWktStepDuration>,  
    pub duration_value: Option<FitMessageWorkoutStepSubfieldDurationValue>,  
    pub target_type: Option<FitFieldWktStepTarget>,  
    pub target_value: Option<FitMessageWorkoutStepSubfieldTargetValue>,  
    pub custom_target_value_low: Option<FitMessageWorkoutStepSubfieldCustomTargetValueLow>,  
    pub custom_target_value_high: Option<FitMessageWorkoutStepSubfieldCustomTargetValueHigh>,  
    pub intensity: Option<FitFieldIntensity>,  
    
}
impl<'a> FitMessageWorkoutStep<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageWorkoutStep<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageWorkoutStep {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            message_index: None,
            wkt_step_name: None,
            duration_type: None,
            duration_value: None,
            target_type: None,
            target_value: None,
            custom_target_value_low: None,
            custom_target_value_high: None,
            intensity: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageWorkoutStep::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageWorkoutStep:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageWorkoutStep<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                254 => {  // message_index
                    let (val, outp) = FitFieldMessageIndex::parse(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.message_index = Some(val);
                    Ok(())
                },
            
                0 => {  // wkt_step_name
                    let (val, outp) = parse_string(inp, field.field_size)?;
                    inp = outp;
                    message.wkt_step_name = Some(val);
                    Ok(())
                },
            
                1 => {  // duration_type
                    let (val, outp) = FitFieldWktStepDuration::parse(inp)?;
                    inp = outp;
                    message.duration_type = Some(val);
                    Ok(())
                },
            
                2 => {  // duration_value
                    let (val, outp) = FitMessageWorkoutStepSubfieldDurationValue::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.duration_value = Some(val);
                    Ok(())
                },
            
                3 => {  // target_type
                    let (val, outp) = FitFieldWktStepTarget::parse(inp)?;
                    inp = outp;
                    message.target_type = Some(val);
                    Ok(())
                },
            
                4 => {  // target_value
                    let (val, outp) = FitMessageWorkoutStepSubfieldTargetValue::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.target_value = Some(val);
                    Ok(())
                },
            
                5 => {  // custom_target_value_low
                    let (val, outp) = FitMessageWorkoutStepSubfieldCustomTargetValueLow::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.custom_target_value_low = Some(val);
                    Ok(())
                },
            
                6 => {  // custom_target_value_high
                    let (val, outp) = FitMessageWorkoutStepSubfieldCustomTargetValueHigh::parse(message, inp, &field, tz_offset)?;
                    inp = outp;
                    message.custom_target_value_high = Some(val);
                    Ok(())
                },
            
                7 => {  // intensity
                    let (val, outp) = FitFieldIntensity::parse(inp)?;
                    inp = outp;
                    message.intensity = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub struct FitMessageZonesTarget<'a> {
    header: FitRecordHeader,
    definition_message: Rc<FitDefinitionMessage>,
    developer_fields: Vec<FitFieldDeveloperData<'a>>,
    pub max_heart_rate: Option<u8>,  
    pub threshold_heart_rate: Option<u8>,  
    pub functional_threshold_power: Option<u16>,  
    pub hr_calc_type: Option<FitFieldHrZoneCalc>,  
    pub pwr_calc_type: Option<FitFieldPwrZoneCalc>,  
    
}
impl<'a> FitMessageZonesTarget<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(Rc<FitMessageZonesTarget<'a>>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        let mut message = FitMessageZonesTarget {
            header: header,
            definition_message: Rc::clone(&definition_message),
            developer_fields: vec![],
            max_heart_rate: None,
            threshold_heart_rate: None,
            functional_threshold_power: None,
            hr_calc_type: None,
            pwr_calc_type: None,
        };

        let inp = &input[..(message.definition_message.message_size)];
        let tz_offset = parsing_state.get_timezone_offset();
        let o = match FitMessageZonesTarget::parse_internal(&mut message, input, tz_offset) {
            Ok(o) => o,
            Err(e) => {
                let mut err_string = String::from("Error parsing FitMessageZonesTarget:");
                err_string.push_str(&format!("  parsing these bytes: '{:x?}'", inp));
                err_string.push_str(&format!("  specific error: {:?}", e));
                return Err(Error::message_parse_failed(err_string))
            }
        };

        

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

    fn parse_internal(message: &mut FitMessageZonesTarget<'a>, input: &'a [u8], tz_offset: f64) -> Result<&'a [u8]> {
        let mut inp = input;
        for field in &message.definition_message.field_definitions {
            let parse_result: Result<()> = match field.definition_number {
            
                1 => {  // max_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.max_heart_rate = Some(val);
                    Ok(())
                },
            
                2 => {  // threshold_heart_rate
                    let (val, outp) = parse_uint8(inp)?;
                    inp = outp;
                    message.threshold_heart_rate = Some(val);
                    Ok(())
                },
            
                3 => {  // functional_threshold_power
                    let (val, outp) = parse_uint16(inp, message.definition_message.endianness)?;
                    inp = outp;
                    message.functional_threshold_power = Some(val);
                    Ok(())
                },
            
                5 => {  // hr_calc_type
                    let (val, outp) = FitFieldHrZoneCalc::parse(inp)?;
                    inp = outp;
                    message.hr_calc_type = Some(val);
                    Ok(())
                },
            
                7 => {  // pwr_calc_type
                    let (val, outp) = FitFieldPwrZoneCalc::parse(inp)?;
                    inp = outp;
                    message.pwr_calc_type = Some(val);
                    Ok(())
                },
            
                invalid_field_num => return Err(Error::invalid_field_number(invalid_field_num))
            };
        }
        Ok(inp)
    }
}
#[derive(Debug)]
pub enum FitDataMessage<'a> {
    
    AccelerometerData(Rc<FitMessageAccelerometerData<'a>>),
    Activity(Rc<FitMessageActivity<'a>>),
    AntChannelId(Rc<FitMessageAntChannelId<'a>>),
    AntRx(Rc<FitMessageAntRx<'a>>),
    AntTx(Rc<FitMessageAntTx<'a>>),
    AviationAttitude(Rc<FitMessageAviationAttitude<'a>>),
    BikeProfile(Rc<FitMessageBikeProfile<'a>>),
    BloodPressure(Rc<FitMessageBloodPressure<'a>>),
    CadenceZone(Rc<FitMessageCadenceZone<'a>>),
    CameraEvent(Rc<FitMessageCameraEvent<'a>>),
    Capabilities(Rc<FitMessageCapabilities<'a>>),
    Connectivity(Rc<FitMessageConnectivity<'a>>),
    Course(Rc<FitMessageCourse<'a>>),
    CoursePoint(Rc<FitMessageCoursePoint<'a>>),
    DeveloperDataId(Rc<FitMessageDeveloperDataId<'a>>),
    DeviceInfo(Rc<FitMessageDeviceInfo<'a>>),
    DeviceSettings(Rc<FitMessageDeviceSettings<'a>>),
    Event(Rc<FitMessageEvent<'a>>),
    ExdDataConceptConfiguration(Rc<FitMessageExdDataConceptConfiguration<'a>>),
    ExdDataFieldConfiguration(Rc<FitMessageExdDataFieldConfiguration<'a>>),
    ExdScreenConfiguration(Rc<FitMessageExdScreenConfiguration<'a>>),
    FieldCapabilities(Rc<FitMessageFieldCapabilities<'a>>),
    FieldDescription(Rc<FitMessageFieldDescription<'a>>),
    FileCapabilities(Rc<FitMessageFileCapabilities<'a>>),
    FileCreator(Rc<FitMessageFileCreator<'a>>),
    FileId(Rc<FitMessageFileId<'a>>),
    Goal(Rc<FitMessageGoal<'a>>),
    GpsMetadata(Rc<FitMessageGpsMetadata<'a>>),
    GyroscopeData(Rc<FitMessageGyroscopeData<'a>>),
    Hr(Rc<FitMessageHr<'a>>),
    HrZone(Rc<FitMessageHrZone<'a>>),
    HrmProfile(Rc<FitMessageHrmProfile<'a>>),
    Hrv(Rc<FitMessageHrv<'a>>),
    Lap(Rc<FitMessageLap<'a>>),
    Length(Rc<FitMessageLength<'a>>),
    MagnetometerData(Rc<FitMessageMagnetometerData<'a>>),
    MemoGlob(Rc<FitMessageMemoGlob<'a>>),
    MesgCapabilities(Rc<FitMessageMesgCapabilities<'a>>),
    MetZone(Rc<FitMessageMetZone<'a>>),
    Monitoring(Rc<FitMessageMonitoring<'a>>),
    MonitoringInfo(Rc<FitMessageMonitoringInfo<'a>>),
    NmeaSentence(Rc<FitMessageNmeaSentence<'a>>),
    ObdiiData(Rc<FitMessageObdiiData<'a>>),
    OhrSettings(Rc<FitMessageOhrSettings<'a>>),
    PowerZone(Rc<FitMessagePowerZone<'a>>),
    Record(Rc<FitMessageRecord<'a>>),
    Schedule(Rc<FitMessageSchedule<'a>>),
    SdmProfile(Rc<FitMessageSdmProfile<'a>>),
    SegmentFile(Rc<FitMessageSegmentFile<'a>>),
    SegmentId(Rc<FitMessageSegmentId<'a>>),
    SegmentLap(Rc<FitMessageSegmentLap<'a>>),
    SegmentLeaderboardEntry(Rc<FitMessageSegmentLeaderboardEntry<'a>>),
    SegmentPoint(Rc<FitMessageSegmentPoint<'a>>),
    Session(Rc<FitMessageSession<'a>>),
    SlaveDevice(Rc<FitMessageSlaveDevice<'a>>),
    Software(Rc<FitMessageSoftware<'a>>),
    SpeedZone(Rc<FitMessageSpeedZone<'a>>),
    Sport(Rc<FitMessageSport<'a>>),
    ThreeDSensorCalibration(Rc<FitMessageThreeDSensorCalibration<'a>>),
    TimestampCorrelation(Rc<FitMessageTimestampCorrelation<'a>>),
    Totals(Rc<FitMessageTotals<'a>>),
    TrainingFile(Rc<FitMessageTrainingFile<'a>>),
    UserProfile(Rc<FitMessageUserProfile<'a>>),
    Video(Rc<FitMessageVideo<'a>>),
    VideoClip(Rc<FitMessageVideoClip<'a>>),
    VideoDescription(Rc<FitMessageVideoDescription<'a>>),
    VideoFrame(Rc<FitMessageVideoFrame<'a>>),
    VideoTitle(Rc<FitMessageVideoTitle<'a>>),
    WatchfaceSettings(Rc<FitMessageWatchfaceSettings<'a>>),
    WeatherAlert(Rc<FitMessageWeatherAlert<'a>>),
    WeatherConditions(Rc<FitMessageWeatherConditions<'a>>),
    WeightScale(Rc<FitMessageWeightScale<'a>>),
    Workout(Rc<FitMessageWorkout<'a>>),
    WorkoutStep(Rc<FitMessageWorkoutStep<'a>>),
    ZonesTarget(Rc<FitMessageZonesTarget<'a>>),
}

impl<'a> FitDataMessage<'a> {
    pub fn parse(input: &'a [u8], header: FitRecordHeader, parsing_state: &mut FitParsingState<'a>, offset_secs: Option<u8>) -> Result<(FitDataMessage<'a>, &'a [u8])> {
        let definition_message = parsing_state.get(header.local_mesg_num())?;
        match definition_message.global_mesg_num {
            
            FitFieldMesgNum::AccelerometerData => {
                let (val, o) = FitMessageAccelerometerData::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::AccelerometerData(val), o))
            },
            FitFieldMesgNum::Activity => {
                let (val, o) = FitMessageActivity::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Activity(val), o))
            },
            FitFieldMesgNum::AntChannelId => {
                let (val, o) = FitMessageAntChannelId::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::AntChannelId(val), o))
            },
            FitFieldMesgNum::AntRx => {
                let (val, o) = FitMessageAntRx::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::AntRx(val), o))
            },
            FitFieldMesgNum::AntTx => {
                let (val, o) = FitMessageAntTx::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::AntTx(val), o))
            },
            FitFieldMesgNum::AviationAttitude => {
                let (val, o) = FitMessageAviationAttitude::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::AviationAttitude(val), o))
            },
            FitFieldMesgNum::BikeProfile => {
                let (val, o) = FitMessageBikeProfile::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::BikeProfile(val), o))
            },
            FitFieldMesgNum::BloodPressure => {
                let (val, o) = FitMessageBloodPressure::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::BloodPressure(val), o))
            },
            FitFieldMesgNum::CadenceZone => {
                let (val, o) = FitMessageCadenceZone::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::CadenceZone(val), o))
            },
            FitFieldMesgNum::CameraEvent => {
                let (val, o) = FitMessageCameraEvent::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::CameraEvent(val), o))
            },
            FitFieldMesgNum::Capabilities => {
                let (val, o) = FitMessageCapabilities::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Capabilities(val), o))
            },
            FitFieldMesgNum::Connectivity => {
                let (val, o) = FitMessageConnectivity::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Connectivity(val), o))
            },
            FitFieldMesgNum::Course => {
                let (val, o) = FitMessageCourse::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Course(val), o))
            },
            FitFieldMesgNum::CoursePoint => {
                let (val, o) = FitMessageCoursePoint::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::CoursePoint(val), o))
            },
            FitFieldMesgNum::DeveloperDataId => {
                let (val, o) = FitMessageDeveloperDataId::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::DeveloperDataId(val), o))
            },
            FitFieldMesgNum::DeviceInfo => {
                let (val, o) = FitMessageDeviceInfo::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::DeviceInfo(val), o))
            },
            FitFieldMesgNum::DeviceSettings => {
                let (val, o) = FitMessageDeviceSettings::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::DeviceSettings(val), o))
            },
            FitFieldMesgNum::Event => {
                let (val, o) = FitMessageEvent::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Event(val), o))
            },
            FitFieldMesgNum::ExdDataConceptConfiguration => {
                let (val, o) = FitMessageExdDataConceptConfiguration::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::ExdDataConceptConfiguration(val), o))
            },
            FitFieldMesgNum::ExdDataFieldConfiguration => {
                let (val, o) = FitMessageExdDataFieldConfiguration::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::ExdDataFieldConfiguration(val), o))
            },
            FitFieldMesgNum::ExdScreenConfiguration => {
                let (val, o) = FitMessageExdScreenConfiguration::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::ExdScreenConfiguration(val), o))
            },
            FitFieldMesgNum::FieldCapabilities => {
                let (val, o) = FitMessageFieldCapabilities::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::FieldCapabilities(val), o))
            },
            FitFieldMesgNum::FieldDescription => {
                let (val, o) = FitMessageFieldDescription::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::FieldDescription(val), o))
            },
            FitFieldMesgNum::FileCapabilities => {
                let (val, o) = FitMessageFileCapabilities::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::FileCapabilities(val), o))
            },
            FitFieldMesgNum::FileCreator => {
                let (val, o) = FitMessageFileCreator::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::FileCreator(val), o))
            },
            FitFieldMesgNum::FileId => {
                let (val, o) = FitMessageFileId::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::FileId(val), o))
            },
            FitFieldMesgNum::Goal => {
                let (val, o) = FitMessageGoal::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Goal(val), o))
            },
            FitFieldMesgNum::GpsMetadata => {
                let (val, o) = FitMessageGpsMetadata::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::GpsMetadata(val), o))
            },
            FitFieldMesgNum::GyroscopeData => {
                let (val, o) = FitMessageGyroscopeData::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::GyroscopeData(val), o))
            },
            FitFieldMesgNum::Hr => {
                let (val, o) = FitMessageHr::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Hr(val), o))
            },
            FitFieldMesgNum::HrZone => {
                let (val, o) = FitMessageHrZone::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::HrZone(val), o))
            },
            FitFieldMesgNum::HrmProfile => {
                let (val, o) = FitMessageHrmProfile::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::HrmProfile(val), o))
            },
            FitFieldMesgNum::Hrv => {
                let (val, o) = FitMessageHrv::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Hrv(val), o))
            },
            FitFieldMesgNum::Lap => {
                let (val, o) = FitMessageLap::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Lap(val), o))
            },
            FitFieldMesgNum::Length => {
                let (val, o) = FitMessageLength::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Length(val), o))
            },
            FitFieldMesgNum::MagnetometerData => {
                let (val, o) = FitMessageMagnetometerData::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::MagnetometerData(val), o))
            },
            FitFieldMesgNum::MemoGlob => {
                let (val, o) = FitMessageMemoGlob::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::MemoGlob(val), o))
            },
            FitFieldMesgNum::MesgCapabilities => {
                let (val, o) = FitMessageMesgCapabilities::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::MesgCapabilities(val), o))
            },
            FitFieldMesgNum::MetZone => {
                let (val, o) = FitMessageMetZone::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::MetZone(val), o))
            },
            FitFieldMesgNum::Monitoring => {
                let (val, o) = FitMessageMonitoring::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Monitoring(val), o))
            },
            FitFieldMesgNum::MonitoringInfo => {
                let (val, o) = FitMessageMonitoringInfo::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::MonitoringInfo(val), o))
            },
            FitFieldMesgNum::NmeaSentence => {
                let (val, o) = FitMessageNmeaSentence::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::NmeaSentence(val), o))
            },
            FitFieldMesgNum::ObdiiData => {
                let (val, o) = FitMessageObdiiData::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::ObdiiData(val), o))
            },
            FitFieldMesgNum::OhrSettings => {
                let (val, o) = FitMessageOhrSettings::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::OhrSettings(val), o))
            },
            FitFieldMesgNum::PowerZone => {
                let (val, o) = FitMessagePowerZone::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::PowerZone(val), o))
            },
            FitFieldMesgNum::Record => {
                let (val, o) = FitMessageRecord::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Record(val), o))
            },
            FitFieldMesgNum::Schedule => {
                let (val, o) = FitMessageSchedule::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Schedule(val), o))
            },
            FitFieldMesgNum::SdmProfile => {
                let (val, o) = FitMessageSdmProfile::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SdmProfile(val), o))
            },
            FitFieldMesgNum::SegmentFile => {
                let (val, o) = FitMessageSegmentFile::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SegmentFile(val), o))
            },
            FitFieldMesgNum::SegmentId => {
                let (val, o) = FitMessageSegmentId::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SegmentId(val), o))
            },
            FitFieldMesgNum::SegmentLap => {
                let (val, o) = FitMessageSegmentLap::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SegmentLap(val), o))
            },
            FitFieldMesgNum::SegmentLeaderboardEntry => {
                let (val, o) = FitMessageSegmentLeaderboardEntry::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SegmentLeaderboardEntry(val), o))
            },
            FitFieldMesgNum::SegmentPoint => {
                let (val, o) = FitMessageSegmentPoint::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SegmentPoint(val), o))
            },
            FitFieldMesgNum::Session => {
                let (val, o) = FitMessageSession::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Session(val), o))
            },
            FitFieldMesgNum::SlaveDevice => {
                let (val, o) = FitMessageSlaveDevice::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SlaveDevice(val), o))
            },
            FitFieldMesgNum::Software => {
                let (val, o) = FitMessageSoftware::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Software(val), o))
            },
            FitFieldMesgNum::SpeedZone => {
                let (val, o) = FitMessageSpeedZone::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::SpeedZone(val), o))
            },
            FitFieldMesgNum::Sport => {
                let (val, o) = FitMessageSport::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Sport(val), o))
            },
            FitFieldMesgNum::ThreeDSensorCalibration => {
                let (val, o) = FitMessageThreeDSensorCalibration::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::ThreeDSensorCalibration(val), o))
            },
            FitFieldMesgNum::TimestampCorrelation => {
                let (val, o) = FitMessageTimestampCorrelation::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::TimestampCorrelation(val), o))
            },
            FitFieldMesgNum::Totals => {
                let (val, o) = FitMessageTotals::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Totals(val), o))
            },
            FitFieldMesgNum::TrainingFile => {
                let (val, o) = FitMessageTrainingFile::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::TrainingFile(val), o))
            },
            FitFieldMesgNum::UserProfile => {
                let (val, o) = FitMessageUserProfile::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::UserProfile(val), o))
            },
            FitFieldMesgNum::Video => {
                let (val, o) = FitMessageVideo::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Video(val), o))
            },
            FitFieldMesgNum::VideoClip => {
                let (val, o) = FitMessageVideoClip::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::VideoClip(val), o))
            },
            FitFieldMesgNum::VideoDescription => {
                let (val, o) = FitMessageVideoDescription::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::VideoDescription(val), o))
            },
            FitFieldMesgNum::VideoFrame => {
                let (val, o) = FitMessageVideoFrame::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::VideoFrame(val), o))
            },
            FitFieldMesgNum::VideoTitle => {
                let (val, o) = FitMessageVideoTitle::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::VideoTitle(val), o))
            },
            FitFieldMesgNum::WatchfaceSettings => {
                let (val, o) = FitMessageWatchfaceSettings::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::WatchfaceSettings(val), o))
            },
            FitFieldMesgNum::WeatherAlert => {
                let (val, o) = FitMessageWeatherAlert::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::WeatherAlert(val), o))
            },
            FitFieldMesgNum::WeatherConditions => {
                let (val, o) = FitMessageWeatherConditions::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::WeatherConditions(val), o))
            },
            FitFieldMesgNum::WeightScale => {
                let (val, o) = FitMessageWeightScale::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::WeightScale(val), o))
            },
            FitFieldMesgNum::Workout => {
                let (val, o) = FitMessageWorkout::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::Workout(val), o))
            },
            FitFieldMesgNum::WorkoutStep => {
                let (val, o) = FitMessageWorkoutStep::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::WorkoutStep(val), o))
            },
            FitFieldMesgNum::ZonesTarget => {
                let (val, o) = FitMessageZonesTarget::parse(input, header, parsing_state, offset_secs)?;
                Ok((FitDataMessage::ZonesTarget(val), o))
            },
            _ => Err(Error::unknown_error())
        }
    }
}