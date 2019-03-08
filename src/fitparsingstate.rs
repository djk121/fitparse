use std::collections::HashMap;
use std::rc::Rc;

use FitDefinitionMessage;
use FitDeveloperDataDefinition;

use errors::{Error, Result};
use fittypes::{FitDataMessage, FitFieldDateTime};

pub struct FitParsingState {
    map: HashMap<u16, Rc<FitDefinitionMessage>>,
    last_timestamp: Option<FitFieldDateTime>,
    timezone_offset_secs: Option<f64>,
    developer_data_definitions: HashMap<u8, FitDeveloperDataDefinition>,
}

impl FitParsingState {
    pub fn new() -> FitParsingState {
        FitParsingState {
            map: HashMap::new(),
            last_timestamp: None,
            timezone_offset_secs: None,
            developer_data_definitions: HashMap::new(),
        }
    }

    pub fn add(&mut self, local_num: u16, def: Rc<FitDefinitionMessage>) {
        //println!("adding local_mesg_num {}", local_num);
        self.map.insert(local_num, def);
    }

    pub fn get(&self, local_num: u16) -> Result<Rc<FitDefinitionMessage>> {
        match self.map.get(&local_num) {
            Some(def) => Ok(Rc::clone(def)),
            None => Err(Error::invalid_local_mesg_num(local_num.to_string())),
        }
    }

    pub fn set_last_timestamp(&mut self, ts: FitFieldDateTime) {
        self.last_timestamp = Some(ts);
    }

    pub fn get_last_timestamp(&self) -> Result<FitFieldDateTime> {
        match self.last_timestamp {
            Some(ts) => Ok(ts),
            None => Err(Error::timestamp_base_not_set()),
        }
    }

    pub fn set_timezone_offset(&mut self, timezone_offset_secs: f64) {
        self.timezone_offset_secs = Some(timezone_offset_secs);
    }

    pub fn get_timezone_offset(&self) -> f64 {
        match self.timezone_offset_secs {
            Some(tzos) => tzos,
            //None => Err(Error::timezone_offset_not_set())
            None => 0.0,
        }
    }

    pub fn set_developer_data_definition(&mut self, developer_data_index: u8, dd: FitDataMessage) {
        let p = self
            .developer_data_definitions
            .entry(developer_data_index)
            .or_insert(FitDeveloperDataDefinition::new());
        p.add(dd);
    }

    pub fn get_developer_data_definition(
        &self,
        developer_data_index: u8,
    ) -> Result<&FitDeveloperDataDefinition> {
        match self.developer_data_definitions.get(&developer_data_index) {
            Some(ddd) => Ok(ddd),
            None => Err(Error::developer_data_definition_not_found(
                developer_data_index,
            )),
        }
    }
}
