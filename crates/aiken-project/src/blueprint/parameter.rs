use super::{
    definitions::{Definitions, Reference},
    error::Error,
    schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
};
use std::{iter, ops::Deref};
use uplc::{
    ast::{Constant, Data as UplcData},
    PlutusData,
};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Parameter {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    pub schema: Declaration<Schema>,
}

impl From<Reference> for Parameter {
    fn from(schema: Reference) -> Parameter {
        Parameter {
            title: None,
            schema: Declaration::Referenced(schema),
        }
    }
}

impl Parameter {
    pub fn validate(
        &self,
        definitions: &Definitions<Annotated<Schema>>,
        constant: &Constant,
    ) -> Result<(), Error> {
        let schema = match &self.schema {
            Declaration::Inline(schema) => schema,
            Declaration::Referenced(ref link) => {
                &definitions
                    .lookup(link)
                    .map(Ok)
                    .unwrap_or_else(|| {
                        Err(Error::UnresolvedSchemaReference {
                            reference: link.clone(),
                        })
                    })?
                    .annotated
            }
        };

        validate_schema(schema, definitions, constant)
    }
}

fn mismatch(term: &Constant, schema: Schema) -> Error {
    Error::SchemaMismatch {
        schema,
        term: term.clone(),
    }
}

fn validate_schema(
    schema: &Schema,
    definitions: &Definitions<Annotated<Schema>>,
    term: &Constant,
) -> Result<(), Error> {
    match schema {
        Schema::Data(data) => validate_data(data, definitions, term),

        Schema::Unit => expect_unit(term),

        Schema::Integer => expect_integer(term),

        Schema::Bytes => expect_bytes(term),

        Schema::String => expect_string(term),

        Schema::Boolean => expect_boolean(term),

        Schema::Pair(left, right) => {
            let (term_left, term_right) = expect_pair(term)?;

            let left =
                left.schema(definitions)
                    .ok_or_else(|| Error::UnresolvedSchemaReference {
                        reference: left.reference().unwrap().clone(),
                    })?;
            validate_schema(left, definitions, &term_left)?;

            let right =
                right
                    .schema(definitions)
                    .ok_or_else(|| Error::UnresolvedSchemaReference {
                        reference: right.reference().unwrap().clone(),
                    })?;
            validate_schema(right, definitions, &term_right)?;

            Ok(())
        }

        Schema::List(Items::One(item)) => {
            let terms = expect_list(term)?;

            let item =
                item.schema(definitions)
                    .ok_or_else(|| Error::UnresolvedSchemaReference {
                        reference: item.reference().unwrap().clone(),
                    })?;

            for ref term in terms {
                validate_schema(item, definitions, term)?;
            }

            Ok(())
        }

        Schema::List(Items::Many(items)) => {
            let terms = expect_list(term)?;

            let items = items
                .iter()
                .map(|item| {
                    item.schema(definitions)
                        .ok_or_else(|| Error::UnresolvedSchemaReference {
                            reference: item.reference().unwrap().clone(),
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;

            if terms.len() != items.len() {
                return Err(Error::TupleItemsMismatch {
                    expected: items.len(),
                    found: terms.len(),
                });
            }

            for (item, ref term) in iter::zip(items, terms) {
                validate_schema(item, definitions, term)?;
            }

            Ok(())
        }
    }
}

fn validate_data(
    data: &Data,
    definitions: &Definitions<Annotated<Schema>>,
    term: &Constant,
) -> Result<(), Error> {
    match data {
        Data::Opaque => expect_data(term),

        Data::Integer => expect_data_integer(term),

        Data::Bytes => expect_data_bytes(term),

        Data::List(Items::One(item)) => {
            let terms = expect_data_list(term)?;

            let item =
                item.schema(definitions)
                    .ok_or_else(|| Error::UnresolvedSchemaReference {
                        reference: item.reference().unwrap().clone(),
                    })?;

            for ref term in terms {
                validate_data(item, definitions, term)?;
            }

            Ok(())
        }

        Data::List(Items::Many(items)) => {
            let terms = expect_data_list(term)?;

            let items = items
                .iter()
                .map(|item| {
                    item.schema(definitions)
                        .ok_or_else(|| Error::UnresolvedSchemaReference {
                            reference: item.reference().unwrap().clone(),
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;

            if terms.len() != items.len() {
                return Err(Error::TupleItemsMismatch {
                    expected: items.len(),
                    found: terms.len(),
                });
            }

            for (item, ref term) in iter::zip(items, terms) {
                validate_data(item, definitions, term)?;
            }

            Ok(())
        }

        Data::Map(keys, values) => {
            let terms = expect_data_map(term)?;

            let keys =
                keys.schema(definitions)
                    .ok_or_else(|| Error::UnresolvedSchemaReference {
                        reference: keys.reference().unwrap().clone(),
                    })?;

            let values =
                values
                    .schema(definitions)
                    .ok_or_else(|| Error::UnresolvedSchemaReference {
                        reference: values.reference().unwrap().clone(),
                    })?;

            for (ref k, ref v) in terms {
                validate_data(keys, definitions, k)?;
                validate_data(values, definitions, v)?;
            }

            Ok(())
        }

        Data::AnyOf(constructors) => {
            let constructors: Vec<(usize, Vec<&Data>)> = constructors
                .iter()
                .map(|constructor| {
                    constructor
                        .annotated
                        .fields
                        .iter()
                        .map(|field| {
                            field.annotated.schema(definitions).ok_or_else(|| {
                                Error::UnresolvedSchemaReference {
                                    reference: field.annotated.reference().unwrap().clone(),
                                }
                            })
                        })
                        .collect::<Result<_, _>>()
                        .map(|fields| (constructor.annotated.index, fields))
                })
                .collect::<Result<_, _>>()?;

            for (index, fields_schema) in constructors.iter() {
                if let Ok(fields) = expect_data_constr(term, *index) {
                    if fields_schema.len() != fields.len() {
                        panic!("fields length different");
                    }

                    for (instance, schema) in iter::zip(fields, fields_schema) {
                        validate_data(schema, definitions, &instance)?;
                    }

                    return Ok(());
                }
            }

            Err(mismatch(
                term,
                Schema::Data(Data::AnyOf(
                    constructors
                        .iter()
                        .map(|(index, fields)| {
                            Constructor {
                                index: *index,
                                fields: fields
                                    .iter()
                                    .map(|_| Declaration::Inline(Box::new(Data::Opaque)).into())
                                    .collect(),
                            }
                            .into()
                        })
                        .collect(),
                )),
            ))
        }
    }
}

fn expect_data(term: &Constant) -> Result<(), Error> {
    if matches!(term, Constant::Data(..)) {
        return Ok(());
    }

    Err(mismatch(term, Schema::Data(Data::Opaque)))
}

fn expect_data_integer(term: &Constant) -> Result<(), Error> {
    if let Constant::Data(data) = term {
        if matches!(data, PlutusData::BigInt(..)) {
            return Ok(());
        }
    }

    Err(mismatch(term, Schema::Data(Data::Integer)))
}

fn expect_data_bytes(term: &Constant) -> Result<(), Error> {
    if let Constant::Data(data) = term {
        if matches!(data, PlutusData::BoundedBytes(..)) {
            return Ok(());
        }
    }

    Err(mismatch(term, Schema::Data(Data::Bytes)))
}

fn expect_data_list(term: &Constant) -> Result<Vec<Constant>, Error> {
    if let Constant::Data(PlutusData::Array(elems)) = term {
        return Ok(elems
            .iter()
            .map(|elem| Constant::Data(elem.to_owned()))
            .collect());
    }

    Err(mismatch(
        term,
        Schema::Data(Data::List(Items::One(Declaration::Inline(Box::new(
            Data::Opaque,
        ))))),
    ))
}

fn expect_data_map(term: &Constant) -> Result<Vec<(Constant, Constant)>, Error> {
    if let Constant::Data(PlutusData::Map(pairs)) = term {
        return Ok(pairs
            .iter()
            .map(|(k, v)| (Constant::Data(k.to_owned()), Constant::Data(v.to_owned())))
            .collect());
    }

    Err(mismatch(
        term,
        Schema::Data(Data::Map(
            Declaration::Inline(Box::new(Data::Opaque)),
            Declaration::Inline(Box::new(Data::Opaque)),
        )),
    ))
}

fn expect_data_constr(term: &Constant, index: usize) -> Result<Vec<Constant>, Error> {
    if let Constant::Data(PlutusData::Constr(constr)) = term {
        if let PlutusData::Constr(expected) = UplcData::constr(index as u64, vec![]) {
            if expected.tag == constr.tag && expected.any_constructor == constr.any_constructor {
                return Ok(constr
                    .fields
                    .iter()
                    .map(|field| Constant::Data(field.to_owned()))
                    .collect());
            }
        }
    }

    Err(mismatch(
        term,
        Schema::Data(Data::AnyOf(vec![Constructor {
            index,
            fields: vec![],
        }
        .into()])),
    ))
}

fn expect_unit(term: &Constant) -> Result<(), Error> {
    if matches!(term, Constant::Unit) {
        return Ok(());
    }

    Err(mismatch(term, Schema::Unit))
}

fn expect_integer(term: &Constant) -> Result<(), Error> {
    if matches!(term, Constant::Integer(..)) {
        return Ok(());
    }

    Err(mismatch(term, Schema::Integer))
}

fn expect_bytes(term: &Constant) -> Result<(), Error> {
    if matches!(term, Constant::ByteString(..)) {
        return Ok(());
    }

    Err(mismatch(term, Schema::Bytes))
}

fn expect_string(term: &Constant) -> Result<(), Error> {
    if matches!(term, Constant::String(..)) {
        return Ok(());
    }

    Err(mismatch(term, Schema::String))
}

fn expect_boolean(term: &Constant) -> Result<(), Error> {
    if matches!(term, Constant::Bool(..)) {
        return Ok(());
    }

    Err(mismatch(term, Schema::Boolean))
}

fn expect_pair(term: &Constant) -> Result<(Constant, Constant), Error> {
    if let Constant::ProtoPair(_, _, left, right) = term {
        return Ok((left.deref().clone(), right.deref().clone()));
    }

    Err(mismatch(
        term,
        Schema::Pair(
            Declaration::Inline(Box::new(Schema::Data(Data::Opaque))),
            Declaration::Inline(Box::new(Schema::Data(Data::Opaque))),
        ),
    ))
}

fn expect_list(term: &Constant) -> Result<Vec<Constant>, Error> {
    if let Constant::ProtoList(_, elems) = term {
        return Ok(elems.to_owned());
    }

    Err(mismatch(
        term,
        Schema::List(Items::One(Declaration::Inline(Box::new(Schema::Data(
            Data::Opaque,
        ))))),
    ))
}
