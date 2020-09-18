use std::collections::HashMap;
use std::sync::Arc;

use FitDefinitionMessage;

use errors;
use errors::Result;
use fittypes::{FitMessageFieldDescription, FitMessageDeveloperDataId};
use fittypes_utils::FitFieldDateTime;

pub struct FitParsingState {
    map: HashMap<u16, Arc<FitDefinitionMessage>>,
    pub retain_bytes: bool,
    last_timestamp: Option<FitFieldDateTime>,
    timezone_offset_secs: Option<f64>,
    developer_data_ids: HashMap<u8, Arc<FitMessageDeveloperDataId>>,
    developer_field_descriptions: HashMap<u8, HashMap<u8, Arc<FitMessageFieldDescription>>>,
}

impl FitParsingState {
    pub fn new() -> FitParsingState {
        FitParsingState {
            map: HashMap::new(),
            retain_bytes: false,
            last_timestamp: None,
            timezone_offset_secs: None,
            developer_data_ids: HashMap::new(),
            developer_field_descriptions: HashMap::new(),
        }
    }

    pub fn add_definition(&mut self, local_num: u16, def: Arc<FitDefinitionMessage>) {
        self.map.insert(local_num, def);
    }

    pub fn get_definition(&self, local_num: u16) -> Result<Arc<FitDefinitionMessage>> {
        match self.map.get(&local_num) {
            Some(def) => Ok(Arc::clone(def)),
            None => Err(errors::invalid_local_mesg_num(local_num.to_string())),
        }
    }

    pub fn set_last_timestamp(&mut self, ts: FitFieldDateTime) {
        self.last_timestamp = Some(ts);
    }

    pub fn get_last_timestamp(&self) -> Result<FitFieldDateTime> {
        match self.last_timestamp {
            Some(ts) => Ok(ts),
            None => Err(errors::timestamp_base_not_set()),
        }
    }

    pub fn set_timezone_offset(&mut self, timezone_offset_secs: f64) {
        self.timezone_offset_secs = Some(timezone_offset_secs);
    }

    pub fn get_timezone_offset(&self) -> f64 {
        match self.timezone_offset_secs {
            Some(tzos) => tzos,
            //None => Err(errors::timezone_offset_not_set())
            None => 0.0,
        }
    }

    pub fn add_developer_data_id(&mut self, developer_data_index: u8, ddi: Arc<FitMessageDeveloperDataId>) {
        self.developer_data_ids.insert(developer_data_index, ddi);
    }

    pub fn get_developer_data_id(&self, developer_data_index: u8) -> Result<Arc<FitMessageDeveloperDataId>> {
        match self.developer_data_ids.get(&developer_data_index) {
            Some(ddi) => Ok(Arc::clone(ddi)),
            None => Err(errors::developer_data_definition_not_found(developer_data_index))
        }
    }

    pub fn add_developer_field_description(&mut self, developer_data_index: u8, field_number: u8, fd: Arc<FitMessageFieldDescription>) {
        let p = self
            .developer_field_descriptions
            .entry(developer_data_index)
            .or_insert(HashMap::new());
        p.insert(field_number, Arc::clone(&fd));
    }


    pub fn get_developer_field_description(&self, developer_data_index: u8, field_number: u8) -> Result<Arc<FitMessageFieldDescription>> {
        match self.developer_field_descriptions.get(&developer_data_index) {
            Some(fd_map) => {
                match fd_map.get(&field_number) {
                    Some(fd) => Ok(Arc::clone(fd)),
                    None => Err(errors::developer_data_definition_not_found(developer_data_index))
                }
            },
            None => Err(errors::developer_data_definition_not_found(developer_data_index))
        }
    }
}
