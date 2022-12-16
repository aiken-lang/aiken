use std::collections::HashMap;

pub struct Manifest {
    pub requirements: HashMap<String, String>,
    pub packages: Vec<Package>,
}

pub struct Package {
    pub name: String,
    pub version: String,
    pub requirements: Vec<String>,
    pub source: PackageSource,
}

pub enum PackageSource {
    GitHub { url: String },
}
