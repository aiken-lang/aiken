use std::collections::HashMap;

use crate::{
    config::{PackageName, Platform},
    error::Error,
    telemetry::EventListener,
};

pub struct Manifest {
    pub requirements: HashMap<String, String>,
    pub packages: Vec<Package>,
}

impl Manifest {
    pub fn load<T>(event_listener: &T) -> Result<(Self, bool), Error>
    where
        T: EventListener,
    {
        todo!()
    }

    pub fn save(&self) -> Result<(), Error> {
        todo!()
    }
}

pub struct Package {
    pub name: PackageName,
    pub version: String,
    pub requirements: Vec<String>,
    pub source: PackageSource,
}

pub enum PackageSource {
    Github { url: String },
}

impl From<PackageSource> for Platform {
    fn from(value: PackageSource) -> Self {
        match value {
            PackageSource::Github { .. } => Self::Github,
        }
    }
}
