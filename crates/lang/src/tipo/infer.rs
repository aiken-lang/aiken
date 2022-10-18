use std::collections::HashMap;

use crate::{
    ast::{ModuleKind, TypedModule, UntypedModule},
    token::Token,
    IdGenerator,
};

use super::{
    environment::Environment,
    error::{Error, Warning},
    Module,
};

pub fn module(
    id_gen: &IdGenerator,
    mut module: UntypedModule,
    kind: ModuleKind,
    package: &str,
    modules: &HashMap<String, Module>,
    warnings: &mut Vec<Warning>,
) -> Result<TypedModule, Error> {
    let name = module.name.clone();
    let docs = std::mem::take(&mut module.docs);
    let mut environment = Environment::new(id_gen.clone(), &name, modules, warnings);

    validate_module_name(&name)?;

    let mut type_names = HashMap::with_capacity(module.definitions.len());
    let mut value_names = HashMap::with_capacity(module.definitions.len());
    let mut hydrators = HashMap::with_capacity(module.definitions.len());

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    for def in module.definitions() {
        environment.register_import(def)?;
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for def in module.definitions() {
        environment.register_types(def, &name, &mut hydrators, &mut type_names)?;
    }

    // Register values so they can be used in functions earlier in the module.
    for def in module.definitions() {
        environment.register_values(def, &name, &mut hydrators, &mut value_names)?;
    }

    todo!()
}

fn validate_module_name(name: &str) -> Result<(), Error> {
    if name == "aiken" {
        return Err(Error::ReservedModuleName {
            name: name.to_string(),
        });
    };

    for segment in name.split('/') {
        if str_to_keyword(segment).is_some() {
            return Err(Error::KeywordInModuleName {
                name: name.to_string(),
                keyword: segment.to_string(),
            });
        }
    }

    Ok(())
}

pub fn str_to_keyword(word: &str) -> Option<Token> {
    // Alphabetical keywords:
    match word {
        "as" => Some(Token::As),
        "assert" => Some(Token::Assert),
        "when" => Some(Token::When),
        "const" => Some(Token::Const),
        "fn" => Some(Token::Fn),
        "if" => Some(Token::If),
        "use" => Some(Token::Use),
        "let" => Some(Token::Let),
        "opaque" => Some(Token::Opaque),
        "pub" => Some(Token::Pub),
        "todo" => Some(Token::Todo),
        "try" => Some(Token::Try),
        "type" => Some(Token::Type),
        _ => None,
    }
}
