use crate::{
    Error, error::TomlLoadingContext, github::repo::LatestRelease, package_name::PackageName, paths,
};
use aiken_lang::{
    ast::{Annotation, ByteArrayFormatPreference, ModuleConstant, Span, UntypedDefinition},
    expr::UntypedExpr,
    parser::token::Base,
};
pub use aiken_lang::{plutus_version::PlutusVersion, version::compiler_version};
use glob::glob;
use miette::NamedSource;
use semver::Version;
use serde::{
    Deserialize, Serialize, de,
    ser::{self, SerializeSeq, SerializeStruct},
};
use std::{
    collections::BTreeMap,
    fmt::Display,
    fs, io,
    path::{Path, PathBuf},
};

#[derive(Deserialize, Serialize, Clone)]
pub struct ProjectConfig {
    pub name: PackageName,

    pub version: String,

    #[serde(
        deserialize_with = "deserialize_version",
        serialize_with = "serialize_version",
        default = "default_version"
    )]
    pub compiler: Version,

    #[serde(default, deserialize_with = "validate_v3_only")]
    pub plutus: PlutusVersion,

    pub license: Option<String>,

    #[serde(default)]
    pub description: String,

    pub repository: Option<Repository>,

    #[serde(default)]
    pub dependencies: Vec<Dependency>,

    #[serde(default)]
    pub config: BTreeMap<String, BTreeMap<String, SimpleExpr>>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub assurance: Option<AssuranceConfig>,
}

/// External assurance workspace settings; paths are resolved by the caller.
#[derive(Debug, Default, Deserialize, Serialize, Clone, PartialEq, Eq)]
#[serde(default)]
pub struct AssuranceConfig {
    /// CardanoLedgerApiBlaster directory, relative to the containing aiken.toml.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<PathBuf>,
}

#[derive(Default, Deserialize)]
#[serde(default)]
struct AssuranceEnvelope {
    assurance: AssuranceConfig,
}

impl AssuranceConfig {
    /// Read only assurance settings, including from a monorepo root manifest.
    /// A missing manifest or assurance section leaves all settings unspecified.
    #[allow(clippy::result_large_err)]
    pub fn load(dir: &Path) -> Result<Self, crate::Error> {
        let config_path = dir.join(paths::project_config());
        let raw_config = match fs::read_to_string(&config_path) {
            Ok(raw_config) => raw_config,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(Self::default()),
            Err(error) => {
                return Err(Error::FileIo {
                    error,
                    path: Box::new(config_path),
                });
            }
        };
        let envelope: AssuranceEnvelope = toml::from_str(&raw_config).map_err(|error| {
            from_toml_de_error(error, config_path, raw_config, TomlLoadingContext::Project)
        })?;
        Ok(envelope.assurance)
    }
}

#[derive(Deserialize, Serialize, Clone)]
struct RawWorkspaceConfig {
    members: Vec<String>,
}

impl RawWorkspaceConfig {
    pub fn expand_members(self, root: &Path) -> Vec<PathBuf> {
        let mut result = Vec::new();

        for member in self.members {
            let pattern = root.join(member);

            let glob_result: Vec<_> = pattern
                .to_str()
                .and_then(|s| glob(s).ok())
                .map_or(Vec::new(), |g| g.filter_map(Result::ok).collect());

            if glob_result.is_empty() {
                // No matches (or glob failed), treat as literal path
                result.push(pattern);
            } else {
                // Glob worked, add all matches
                result.extend(glob_result);
            }
        }

        result
    }
}

pub struct WorkspaceConfig {
    pub members: Vec<PathBuf>,
}

impl WorkspaceConfig {
    #[allow(clippy::result_large_err)]
    pub fn load(dir: &Path) -> Result<WorkspaceConfig, Error> {
        let config_path = dir.join(paths::project_config());
        let raw_config = fs::read_to_string(&config_path).map_err(|_| Error::MissingManifest {
            path: Box::new(dir.to_path_buf()),
        })?;

        let raw: RawWorkspaceConfig = toml::from_str(&raw_config).map_err(|e| {
            from_toml_de_error(e, config_path, raw_config, TomlLoadingContext::Workspace)
        })?;

        let members = raw.expand_members(dir);

        Ok(WorkspaceConfig { members })
    }
}

#[derive(Clone, Debug)]
pub enum SimpleExpr {
    Int(i64),
    Bool(bool),
    ByteArray(Vec<u8>, ByteArrayFormatPreference),
    List(Vec<SimpleExpr>),
}

impl SimpleExpr {
    pub fn as_untyped_expr(&self, annotation: &Annotation) -> UntypedExpr {
        match self {
            SimpleExpr::Bool(b) => UntypedExpr::Var {
                location: Span::empty(),
                name: if *b { "True" } else { "False" }.to_string(),
            },
            SimpleExpr::Int(i) => UntypedExpr::UInt {
                location: Span::empty(),
                value: format!("{i}"),
                base: Base::Decimal {
                    numeric_underscore: false,
                },
            },
            SimpleExpr::ByteArray(bs, preferred_format) => UntypedExpr::ByteArray {
                location: Span::empty(),
                bytes: bs.iter().map(|b| (*b, Span::empty())).collect(),
                preferred_format: *preferred_format,
            },
            SimpleExpr::List(es) => match annotation {
                Annotation::Tuple { elems, .. } => UntypedExpr::Tuple {
                    location: Span::empty(),
                    elems: es
                        .iter()
                        .zip(elems)
                        .map(|(e, ann)| e.as_untyped_expr(ann))
                        .collect(),
                },
                Annotation::Constructor {
                    module,
                    name,
                    arguments,
                    ..
                } if name == "List" && module.is_none() => UntypedExpr::List {
                    location: Span::empty(),
                    elements: es
                        .iter()
                        .map(|e| e.as_untyped_expr(arguments.first().unwrap()))
                        .collect(),
                    tail: None,
                },
                _ => unreachable!(
                    "unexpected annotation for simple list expression: {annotation:#?}"
                ),
            },
        }
    }

    pub fn as_annotation(&self) -> Annotation {
        let location = Span::empty();
        match self {
            SimpleExpr::Bool(..) => Annotation::boolean(location),
            SimpleExpr::Int(_) => Annotation::int(location),
            SimpleExpr::ByteArray(_, _) => Annotation::bytearray(location),
            SimpleExpr::List(elems) => {
                let elems = elems.iter().map(|e| e.as_annotation()).collect::<Vec<_>>();

                let (is_uniform, inner) =
                    elems
                        .iter()
                        .fold((true, None), |(matches, ann), a| match ann {
                            None => (matches, Some(a)),
                            Some(b) => (matches && a == b, ann),
                        });

                if is_uniform {
                    Annotation::list(
                        inner.cloned().unwrap_or_else(|| Annotation::data(location)),
                        location,
                    )
                } else {
                    Annotation::tuple(elems, location)
                }
            }
        }
    }

    pub fn as_definition(&self, identifier: &str) -> UntypedDefinition {
        let annotation = self.as_annotation();
        let value = self.as_untyped_expr(&annotation);
        UntypedDefinition::ModuleConstant(ModuleConstant {
            location: Span::empty(),
            doc: None,
            public: true,
            name: identifier.to_string(),
            annotation: Some(annotation),
            value,
        })
    }
}

impl Serialize for SimpleExpr {
    fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            SimpleExpr::Bool(b) => serializer.serialize_bool(*b),
            SimpleExpr::Int(i) => serializer.serialize_i64(*i),
            SimpleExpr::ByteArray(bs, preferred_format) => match preferred_format {
                ByteArrayFormatPreference::Utf8String => {
                    serializer.serialize_str(String::from_utf8(bs.to_vec()).unwrap().as_str())
                }
                ByteArrayFormatPreference::ArrayOfBytes(..)
                | ByteArrayFormatPreference::HexadecimalString => {
                    let mut s = serializer.serialize_struct("ByteArray", 2)?;
                    s.serialize_field("bytes", &hex::encode(bs))?;
                    s.serialize_field("encoding", "base16")?;
                    s.end()
                }
            },
            SimpleExpr::List(es) => {
                let mut seq = serializer.serialize_seq(Some(es.len()))?;
                for e in es {
                    seq.serialize_element(e)?;
                }
                seq.end()
            }
        }
    }
}

impl<'a> Deserialize<'a> for SimpleExpr {
    fn deserialize<D: de::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        struct SimpleExprVisitor;

        #[derive(Deserialize)]
        enum Encoding {
            #[serde(rename(deserialize = "utf8"))]
            Utf8,
            #[serde(rename(deserialize = "utf-8"))]
            Utf8Bis,
            #[serde(rename(deserialize = "hex"))]
            Hex,
            #[serde(rename(deserialize = "base16"))]
            Base16,
        }

        #[derive(Deserialize)]
        struct Bytes {
            bytes: String,
            encoding: Encoding,
        }

        impl<'a> de::Visitor<'a> for SimpleExprVisitor {
            type Value = SimpleExpr;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("Int | Bool | ByteArray | List<any_of_those>")
            }

            fn visit_bool<E>(self, b: bool) -> Result<Self::Value, E> {
                Ok(SimpleExpr::Bool(b))
            }

            fn visit_i64<E>(self, i: i64) -> Result<Self::Value, E> {
                Ok(SimpleExpr::Int(i))
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> {
                Ok(SimpleExpr::ByteArray(
                    s.as_bytes().to_vec(),
                    ByteArrayFormatPreference::Utf8String,
                ))
            }

            fn visit_map<V>(self, map: V) -> Result<Self::Value, V::Error>
            where
                V: de::MapAccess<'a>,
            {
                let Bytes { bytes, encoding } =
                    Bytes::deserialize(de::value::MapAccessDeserializer::new(map))?;

                match encoding {
                    Encoding::Hex | Encoding::Base16 => match hex::decode(&bytes) {
                        Err(e) => Err(de::Error::custom(format!("invalid base16 string: {e:?}"))),
                        Ok(bytes) => Ok(SimpleExpr::ByteArray(
                            bytes,
                            ByteArrayFormatPreference::HexadecimalString,
                        )),
                    },
                    Encoding::Utf8 | Encoding::Utf8Bis => Ok(SimpleExpr::ByteArray(
                        bytes.as_bytes().to_vec(),
                        ByteArrayFormatPreference::Utf8String,
                    )),
                }
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'a>,
            {
                let mut es = Vec::new();

                while let Some(e) = seq.next_element()? {
                    es.push(e);
                }

                Ok(SimpleExpr::List(es))
            }
        }

        deserializer.deserialize_any(SimpleExprVisitor)
    }
}

fn deserialize_version<'de, D>(deserializer: D) -> Result<Version, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?.replace('v', "");

    Version::parse(&buf).map_err(serde::de::Error::custom)
}

fn serialize_version<S>(version: &Version, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let version = format!("v{version}");

    serializer.serialize_str(&version)
}

fn default_version() -> Version {
    Version::parse(built_info::PKG_VERSION).unwrap()
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Repository {
    pub user: String,
    pub project: String,
    pub platform: Platform,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Copy, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    Github,
    Gitlab,
    Bitbucket,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Debug)]
pub struct Dependency {
    pub name: PackageName,
    pub version: String,
    pub source: Platform,
}

impl Display for Platform {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        match *self {
            Platform::Github => f.write_str("github"),
            Platform::Gitlab => f.write_str("gitlab"),
            Platform::Bitbucket => f.write_str("bitbucket"),
        }
    }
}

impl ProjectConfig {
    pub fn default(name: &PackageName) -> Self {
        ProjectConfig {
            name: name.clone(),
            version: "0.0.0".to_string(),
            compiler: default_version(),
            plutus: PlutusVersion::default(),
            license: Some("Apache-2.0".to_string()),
            description: format!("Aiken contracts for project '{name}'"),
            repository: Some(Repository {
                user: name.owner.clone(),
                project: name.repo.clone(),
                platform: Platform::Github,
            }),
            dependencies: vec![Dependency {
                name: PackageName {
                    owner: "aiken-lang".to_string(),
                    repo: "stdlib".to_string(),
                },
                version: match LatestRelease::of("aiken-lang/stdlib") {
                    Ok(stdlib) => stdlib.tag_name,
                    _ => "1.5.0".to_string(),
                },
                source: Platform::Github,
            }],
            config: BTreeMap::new(),
            assurance: None,
        }
    }

    pub fn save(&self, dir: &Path) -> Result<(), io::Error> {
        let aiken_toml_path = dir.join(paths::project_config());
        let aiken_toml = toml::to_string_pretty(self).unwrap();
        fs::write(aiken_toml_path, aiken_toml)
    }

    #[allow(clippy::result_large_err)]
    pub fn load(dir: &Path) -> Result<ProjectConfig, Error> {
        let config_path = dir.join(paths::project_config());
        let raw_config = fs::read_to_string(&config_path).map_err(|_| Error::MissingManifest {
            path: Box::new(dir.to_path_buf()),
        })?;

        let result: Self = toml::from_str(&raw_config).map_err(|e| {
            from_toml_de_error(e, config_path, raw_config, TomlLoadingContext::Project)
        })?;

        Ok(result)
    }

    pub fn insert(mut self, dependency: &Dependency, and_replace: bool) -> Option<Self> {
        for existing in self.dependencies.iter_mut() {
            if existing.name == dependency.name {
                return if and_replace {
                    existing.version.clone_from(&dependency.version);
                    Some(self)
                } else {
                    None
                };
            }
        }
        self.dependencies.push(dependency.clone());
        Some(self)
    }
}

fn validate_v3_only<'de, D>(deserializer: D) -> Result<PlutusVersion, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let version = PlutusVersion::deserialize(deserializer)?;

    match version {
        PlutusVersion::V3 => Ok(version),
        _ => Err(serde::de::Error::custom("Aiken only supports Plutus V3")),
    }
}

fn from_toml_de_error(
    e: toml::de::Error,
    config_path: PathBuf,
    raw_config: String,
    ctx: TomlLoadingContext,
) -> Error {
    Error::TomlLoading {
        ctx,
        path: Box::new(config_path.clone()),
        src: Box::new(raw_config.clone()),
        named: NamedSource::new(config_path.display().to_string(), raw_config).into(),
        // this isn't actually a legit way to get the span
        location: e.span().map(|range| Span {
            start: range.start,
            end: range.end,
        }),
        help: Box::new(e.message().to_string()),
    }
}

mod built_info {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

pub fn compiler_info() -> String {
    format!(
        r#"
Operating System: {}
Architecture:     {}
Version:          {}"#,
        built_info::CFG_OS,
        built_info::CFG_TARGET_ARCH,
        compiler_version(true),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    const MINIMAL_PROJECT: &str = "name = \"test/assurance\"\nversion = \"0.0.0\"\n";

    #[test]
    fn assurance_absent_is_optional() {
        let config: ProjectConfig = toml::from_str(MINIMAL_PROJECT).unwrap();
        assert!(config.assurance.is_none());
        assert_eq!(
            toml::from_str::<AssuranceConfig>("").unwrap(),
            AssuranceConfig::default()
        );
    }

    #[test]
    fn assurance_workspace_is_preserved() {
        let raw =
            format!("{MINIMAL_PROJECT}\n[assurance]\nworkspace = \"../CardanoLedgerApiBlaster\"\n");
        let config: ProjectConfig = toml::from_str(&raw).unwrap();
        assert_eq!(
            config.assurance.unwrap().workspace,
            Some(PathBuf::from("../CardanoLedgerApiBlaster"))
        );
    }

    #[test]
    fn assurance_rejects_non_string_workspace() {
        let raw = format!("{MINIMAL_PROJECT}\n[assurance]\nworkspace = 42\n");
        assert!(toml::from_str::<ProjectConfig>(&raw).is_err());
        assert!(toml::from_str::<AssuranceEnvelope>(&raw).is_err());
    }

    #[test]
    fn assurance_absent_stays_omitted_on_roundtrip() {
        let config: ProjectConfig = toml::from_str(MINIMAL_PROJECT).unwrap();
        let serialized = toml::to_string_pretty(&config).unwrap();
        let value: toml::Value = toml::from_str(&serialized).unwrap();
        assert!(value.get("assurance").is_none());
        let restored: ProjectConfig = toml::from_str(&serialized).unwrap();
        assert!(restored.assurance.is_none());
    }

    #[test]
    fn assurance_envelope_accepts_monorepo_manifest() {
        let raw = "members = [\"packages/*\"]\n[assurance]\nworkspace = \"ledger\"\n";
        let envelope: AssuranceEnvelope = toml::from_str(raw).unwrap();
        assert_eq!(envelope.assurance.workspace, Some(PathBuf::from("ledger")));
        let absent: AssuranceEnvelope = toml::from_str("members = []").unwrap();
        assert_eq!(absent.assurance, AssuranceConfig::default());
    }

    #[test]
    fn assurance_load_handles_manifest_and_errors() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        static NEXT: AtomicUsize = AtomicUsize::new(0);
        struct Fixture(PathBuf);
        impl Drop for Fixture {
            fn drop(&mut self) {
                let _ = fs::remove_dir_all(&self.0);
            }
        }
        let fixture = Fixture(std::env::temp_dir().join(format!(
            "aiken-assurance-config-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        )));
        fs::create_dir_all(&fixture.0).unwrap();
        assert_eq!(
            AssuranceConfig::load(&fixture.0).unwrap(),
            AssuranceConfig::default()
        );
        let manifest = fixture.0.join(paths::project_config());
        fs::write(&manifest, "members = []\n").unwrap();
        assert_eq!(
            AssuranceConfig::load(&fixture.0).unwrap(),
            AssuranceConfig::default()
        );
        fs::write(
            &manifest,
            "members = []\n[assurance]\nworkspace = \"../ledger\"\n",
        )
        .unwrap();
        assert_eq!(
            AssuranceConfig::load(&fixture.0).unwrap().workspace,
            Some(PathBuf::from("../ledger"))
        );
        for malformed in ["[assurance]\nworkspace = 42\n", "[assurance"] {
            fs::write(&manifest, malformed).unwrap();
            assert!(matches!(
                AssuranceConfig::load(&fixture.0),
                Err(Error::TomlLoading { .. })
            ));
        }
        fs::remove_file(&manifest).unwrap();
        fs::create_dir(&manifest).unwrap();
        assert!(matches!(
            AssuranceConfig::load(&fixture.0),
            Err(Error::FileIo { .. })
        ));
    }

    #[allow(clippy::arc_with_non_send_sync)]
    fn arbitrary_simple_expr() -> impl Strategy<Value = SimpleExpr> {
        let leaf = prop_oneof![
            (any::<i64>)().prop_map(SimpleExpr::Int),
            (any::<bool>)().prop_map(SimpleExpr::Bool),
            "[a-z0-9]*".prop_map(|bytes| SimpleExpr::ByteArray(
                bytes.as_bytes().to_vec(),
                ByteArrayFormatPreference::Utf8String
            )),
            "([0-9a-f][0-9a-f])*".prop_map(|bytes| SimpleExpr::ByteArray(
                bytes.as_bytes().to_vec(),
                ByteArrayFormatPreference::HexadecimalString
            ))
        ];

        leaf.prop_recursive(3, 8, 3, |inner| {
            prop_oneof![
                inner.clone(),
                prop::collection::vec(inner.clone(), 0..3).prop_map(SimpleExpr::List)
            ]
        })
    }

    #[derive(Deserialize, Serialize)]
    struct TestConfig {
        expr: SimpleExpr,
    }

    proptest! {
        #[test]
        fn round_trip_simple_expr(expr in arbitrary_simple_expr()) {
            let pretty = toml::to_string_pretty(&TestConfig { expr });
            assert!(
                matches!(
                    pretty.as_ref().map(|s| toml::from_str::<TestConfig>(s.as_str())),
                    Ok(Ok(..)),
                ),
                "\ncounterexample: {}\n",
                pretty.unwrap_or_default(),
            )

        }
    }
}
