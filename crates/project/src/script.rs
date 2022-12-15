use crate::{ExBudget, Term};
use aiken_lang::ast::BinOp;
use std::path::PathBuf;
use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug)]
pub struct Script {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub program: Program<NamedDeBruijn>,
    pub evaluation_hint: Option<EvalHint>,
}

impl Script {
    pub fn new(
        input_path: PathBuf,
        module: String,
        name: String,
        program: Program<NamedDeBruijn>,
        evaluation_hint: Option<EvalHint>,
    ) -> Script {
        Script {
            input_path,
            module,
            name,
            program,
            evaluation_hint,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvalHint {
    pub bin_op: BinOp,
    pub left: Program<NamedDeBruijn>,
    pub right: Program<NamedDeBruijn>,
}

#[derive(Debug)]
pub struct EvalInfo {
    pub success: bool,
    pub script: Script,
    pub spent_budget: ExBudget,
    pub output: Option<Term<NamedDeBruijn>>,
}
