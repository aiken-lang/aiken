use aiken_project::{
    blueprint::{
        self,
        definitions::Definitions,
        schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
    },
    error::Error,
    pretty::multiline,
    watch::with_project,
};
use inquire;
use num_bigint::BigInt;
use ordinal::Ordinal;
use owo_colors::{OwoColorize, Stream::Stderr};
use pallas_primitives::alonzo::PlutusData;
use std::{fs, path::PathBuf, process, str::FromStr};
use uplc::ast::Data as UplcData;

/// Apply a parameter to a parameterized validator.
#[derive(clap::Args)]
pub struct Args {
    /// The parameter, as a Plutus Data (CBOR, hex-encoded).
    ///
    /// For example, `182A` designates an integer of value 42. If you're unsure about the shape of
    /// the parameter, look at the schema specified in the project's blueprint (i.e.
    /// `plutus.json`), or use the `cbor.serialise` function from the Aiken standard library.
    #[clap(value_name = "CBOR")]
    parameter: Option<String>,

    /// Optional path to the blueprint file to be used as input.
    ///
    /// [default: plutus.json]
    #[clap(
        short,
        long = "in",
        value_parser,
        value_name = "FILEPATH",
        verbatim_doc_comment
    )]
    input: Option<PathBuf>,

    /// Optional relative filepath to the generated Plutus blueprint. Default to printing to stdout
    /// when omitted.
    #[clap(short, long("out"), value_parser, value_name = "FILEPATH")]
    output: Option<PathBuf>,

    /// Name of the validator's module within the project. Optional if there's only one validator.
    #[clap(short, long)]
    module: Option<String>,

    /// Name of the validator within the module. Optional if there's only one validator.
    #[clap(short, long)]
    validator: Option<String>,
}

pub fn exec(
    Args {
        parameter,
        input,
        output,
        module,
        validator,
    }: Args,
) -> miette::Result<()> {
    with_project(None, false, false, |p| {
        eprintln!(
            "{} blueprint",
            "    Analyzing"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
        );

        let blueprint_input_path = p.blueprint_path(input.as_deref());

        let data: PlutusData = match &parameter {
            Some(param) => {
                eprintln!(
                    "{} inputs",
                    "      Parsing"
                        .if_supports_color(Stderr, |s| s.purple())
                        .if_supports_color(Stderr, |s| s.bold()),
                );

                let bytes = hex::decode(param)
                    .map_err::<Error, _>(|e| {
                        blueprint::error::Error::MalformedParameter {
                            hint: format!("Invalid hex-encoded string: {e}"),
                        }
                        .into()
                    })
                    .unwrap_or_else(|e| {
                        println!();
                        e.report();
                        process::exit(1)
                    });

                uplc::plutus_data(&bytes)
                    .map_err::<Error, _>(|e| {
                        blueprint::error::Error::MalformedParameter {
                            hint: format!("Invalid Plutus data; malformed CBOR encoding: {e}"),
                        }
                        .into()
                    })
                    .unwrap_or_else(|e| {
                        println!();
                        e.report();
                        process::exit(1)
                    })
            }

            None => p.construct_parameter_incrementally(
                module.as_deref(),
                validator.as_deref(),
                &blueprint_input_path,
                ask_schema,
            )?,
        };

        eprintln!(
            "{} {}",
            "     Applying"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
            {
                let padding = "\n              ";
                multiline(48, UplcData::to_hex(data.clone())).join(padding)
            }
        );

        let blueprint = p.apply_parameter(
            module.as_deref(),
            validator.as_deref(),
            &blueprint_input_path,
            &data,
        )?;

        let json = serde_json::to_string_pretty(&blueprint).unwrap();

        match output {
            None => {
                println!("\n{}\n", json);
                Ok(())
            }
            Some(ref path) => {
                let blueprint_output_path = p.blueprint_path(Some(path));
                fs::write(&blueprint_output_path, json).map_err(|error| Error::FileIo {
                    error,
                    path: blueprint_output_path,
                })
            }
        }?;

        eprintln!(
            "{}",
            "         Done"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
        );

        Ok(())
    })
    .map_err(|_| std::process::exit(1))
}

fn ask_schema(
    schema: &Annotated<Schema>,
    definitions: &Definitions<Annotated<Schema>>,
) -> Result<PlutusData, blueprint::error::Error> {
    match schema.annotated {
        Schema::Data(Data::Integer) => {
            let input = prompt_primitive("an integer", schema)?;

            let n = BigInt::from_str(input.as_str()).map_err(|e| {
                blueprint::error::Error::MalformedParameter {
                    hint: format!("Unable to convert input to integer: {e}"),
                }
            })?;

            Ok(UplcData::integer(n))
        }

        Schema::Data(Data::Bytes) => {
            let input = prompt_primitive("a byte-array", schema)?;

            let bytes =
                hex::decode(input).map_err(|e| blueprint::error::Error::MalformedParameter {
                    hint: format!("Invalid hex-encoded string: {e}"),
                })?;

            Ok(UplcData::bytestring(bytes))
        }

        Schema::Data(Data::List(Items::Many(ref decls))) => {
            eprintln!(
                "        {}",
                asking(schema, "Found", &format!("a {}-tuple", decls.len()))
            );

            let mut elems = vec![];

            for (ix, decl) in decls.iter().enumerate() {
                eprintln!(
                    "       {} Tuple's {}{} element",
                    "Asking"
                        .if_supports_color(Stderr, |s| s.purple())
                        .if_supports_color(Stderr, |s| s.bold()),
                    ix + 1,
                    Ordinal::<usize>(ix + 1).suffix()
                );
                let inner_schema = lookup_declaration(&decl.clone().into(), definitions);
                elems.push(ask_schema(&inner_schema, definitions)?);
            }

            Ok(UplcData::list(elems))
        }

        Schema::Data(Data::List(Items::One(ref decl))) => {
            eprintln!("        {}", asking(schema, "Found", "a list"));

            let inner_schema = lookup_declaration(&decl.clone().into(), definitions);

            let mut elems = vec![];
            while prompt_iterable(schema, "item")? {
                elems.push(ask_schema(&inner_schema, definitions)?);
            }

            Ok(UplcData::list(elems))
        }

        Schema::Data(Data::Map(ref key_decl, ref value_decl)) => {
            eprintln!("        {}", asking(schema, "Found", "an associative map"));

            let key_schema = lookup_declaration(&key_decl.clone().into(), definitions);
            let value_schema = lookup_declaration(&value_decl.clone().into(), definitions);

            let mut elems = vec![];
            while prompt_iterable(schema, "key/value entry")? {
                elems.push((
                    ask_schema(&key_schema, definitions)?,
                    ask_schema(&value_schema, definitions)?,
                ));
            }

            Ok(UplcData::map(elems))
        }

        Schema::Data(Data::AnyOf(ref constructors)) => {
            eprintln!(
                "        {}",
                asking(
                    schema,
                    "Found",
                    if constructors.len() == 1 {
                        "a record"
                    } else {
                        "a data-type"
                    }
                )
            );

            let ix = prompt_constructor(constructors, schema)?;

            let mut fields = Vec::new();
            for field in &constructors[ix].annotated.fields {
                let inner_schema = lookup_declaration(field, definitions);
                fields.push(ask_schema(&inner_schema, definitions)?);
            }

            Ok(UplcData::constr(ix.try_into().unwrap(), fields))
        }

        _ => unimplemented!(
            "Hey! You've found a case that we haven't implemented yet. Yes, we've been a bit lazy on that one... If that use-case is important to you, please let us know on Discord or on Github."
        ),
    }
}

fn lookup_declaration(
    decl: &Annotated<Declaration<Data>>,
    definitions: &Definitions<Annotated<Schema>>,
) -> Annotated<Schema> {
    match decl.annotated {
        Declaration::Inline(ref data) => Annotated {
            title: decl.title.clone(),
            description: decl.description.clone(),
            annotated: Schema::Data(*(*data).clone()),
        },
        Declaration::Referenced(ref reference) => {
            let schema = definitions
                .lookup(reference)
                .expect("reference to unknown type in blueprint?");
            Annotated {
                title: decl.title.clone().or_else(|| schema.title.clone()),
                description: decl
                    .description
                    .clone()
                    .or_else(|| schema.description.clone()),
                annotated: schema.annotated.clone(),
            }
        }
    }
}

fn asking(schema: &Annotated<Schema>, verb: &str, type_name: &str) -> String {
    let subject = get_subject(schema, type_name);
    format!(
        "{} {subject}",
        verb.if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold()),
        subject = subject,
    )
}

fn prompt_primitive(
    type_name: &str,
    schema: &Annotated<Schema>,
) -> Result<String, blueprint::error::Error> {
    inquire::Text::new(&format!("     {}:", asking(schema, "Asking", type_name)))
        .with_description(schema.description.as_ref())
        .prompt()
        .map_err(|e| blueprint::error::Error::MalformedParameter {
            hint: format!("Invalid input received from prompt: {e}"),
        })
}

fn prompt_iterable(
    schema: &Annotated<Schema>,
    elem_name: &str,
) -> Result<bool, blueprint::error::Error> {
    inquire::Confirm::new(&format!(
        "     {} one more {elem_name}?",
        "Adding"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold())
    ))
    .with_description(schema.description.as_ref())
    .with_default(true)
    .prompt()
    .map_err(|e| blueprint::error::Error::MalformedParameter {
        hint: format!("Invalid input received from prompt: {e}"),
    })
}

fn prompt_constructor(
    constructors: &[Annotated<Constructor>],
    schema: &Annotated<Schema>,
) -> Result<usize, blueprint::error::Error> {
    let mut choices = Vec::new();
    for c in constructors {
        let name = c
            .title
            .as_ref()
            .cloned()
            .unwrap_or_else(|| format!("{}", c.annotated.index));
        choices.push(name);
    }

    let mut choice = choices
        .first()
        .expect("Data-type with no constructor?")
        .to_string();

    if choices.len() > 1 {
        choice = inquire::Select::new(
            &format!(
                "  {} constructor",
                "Selecting"
                    .if_supports_color(Stderr, |s| s.purple())
                    .if_supports_color(Stderr, |s| s.bold())
            ),
            choices.clone(),
        )
        .with_description(schema.description.as_ref())
        .prompt()
        .map_err(|e| blueprint::error::Error::MalformedParameter {
            hint: format!("Invalid input received from prompt: {e}"),
        })?;
    }

    Ok(choices.into_iter().position(|c| c == choice).unwrap())
}

fn get_subject<T>(schema: &Annotated<T>, type_name: &str) -> String {
    schema
        .title
        .as_ref()
        .map(|title| format!("{title} ({type_name})"))
        .unwrap_or_else(|| type_name.to_string())
}

trait WithDescription<'a> {
    fn with_description(self, opt: Option<&'a String>) -> Self;
}

impl<'a> WithDescription<'a> for inquire::Confirm<'a> {
    fn with_description(
        self: inquire::Confirm<'a>,
        opt: Option<&'a String>,
    ) -> inquire::Confirm<'a> {
        match opt {
            Some(description) => self.with_help_message(description),
            None => self,
        }
    }
}

impl<'a> WithDescription<'a> for inquire::Text<'a> {
    fn with_description(self: inquire::Text<'a>, opt: Option<&'a String>) -> inquire::Text<'a> {
        match opt {
            Some(description) => self.with_help_message(description),
            None => self,
        }
    }
}

impl<'a, T> WithDescription<'a> for inquire::Select<'a, T>
where
    T: std::fmt::Display,
{
    fn with_description(
        self: inquire::Select<'a, T>,
        opt: Option<&'a String>,
    ) -> inquire::Select<'a, T> {
        match opt {
            Some(description) => self.with_help_message(description),
            None => self,
        }
    }
}
