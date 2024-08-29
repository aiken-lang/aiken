use pallas_primitives::conway::Language;
use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Deserialize, Serialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum PlutusVersion {
    V1,
    V2,
    #[default]
    V3,
}

impl From<PlutusVersion> for Language {
    fn from(value: PlutusVersion) -> Self {
        match value {
            PlutusVersion::V1 => Language::PlutusV1,
            PlutusVersion::V2 => Language::PlutusV2,
            PlutusVersion::V3 => Language::PlutusV3,
        }
    }
}

impl From<&PlutusVersion> for Language {
    fn from(value: &PlutusVersion) -> Self {
        match value {
            PlutusVersion::V1 => Language::PlutusV1,
            PlutusVersion::V2 => Language::PlutusV2,
            PlutusVersion::V3 => Language::PlutusV3,
        }
    }
}

impl From<Language> for PlutusVersion {
    fn from(value: Language) -> Self {
        match value {
            Language::PlutusV1 => PlutusVersion::V2,
            Language::PlutusV2 => PlutusVersion::V2,
            Language::PlutusV3 => PlutusVersion::V3,
        }
    }
}

impl PlutusVersion {
    pub fn cardano_cli_type(&self) -> String {
        match self {
            PlutusVersion::V1 => "PlutusScriptV1".to_string(),
            PlutusVersion::V2 => "PlutusScriptV2".to_string(),
            PlutusVersion::V3 => "PlutusScriptV3".to_string(),
        }
    }
}
