use std::sync::Arc;

use uplc::builtins::DefaultFunction;

use crate::{
    ast::{AssignmentKind, BinOp, TypedRecordUpdateArg},
    tipo::{Type, ValueConstructor},
};

// []

#[derive(Debug, Clone)]
pub enum IR {
    Int {
        scope: Vec<u64>,
        value: String,
    },

    String {
        scope: Vec<u64>,
        value: String,
    },

    ByteArray {
        scope: Vec<u64>,
        bytes: Vec<u8>,
    },

    Var {
        scope: Vec<u64>,
        constructor: ValueConstructor,
        name: String,
    },

    // Fn {
    //  scope: Vec<u64>,
    //     tipo: Arc<Type>,
    //     is_capture: bool,
    //     args: Vec<Arg<Arc<Type>>>,
    //     body: Box<Self>,
    //     return_annotation: Option<Annotation>,
    // },
    List {
        scope: Vec<u64>,
        count: usize,
        tipo: Arc<Type>,
        tail: bool,
    },

    Tail {
        scope: Vec<u64>,
        count: usize,
    },

    ListAccessor {
        scope: Vec<u64>,
        names: Vec<String>,
        tail: bool,
    },

    // func(x, other(y))
    //[ Define(3) x  definition *lam_body* -> [ (x [ (func [ func x [ (y [ other y ]) *definition* ] ]) *definition* ]) *definition* ], Define func -> [ (func [ func x [ (y [ other y ]) *definition* ] ]) *definition* ] , Call func -> [ func x [ (y [ other y ]) *definition* ] ], Var x -> x, Define y -> [ (y [ other y ]) *definition* ], Call other -> [ other y ], Var y -> y]

    // [y_varCall other_var Call Call x_defCall func_var ]
    Call {
        scope: Vec<u64>,
        count: usize,
    },

    Builtin {
        scope: Vec<u64>,
        func: DefaultFunction,
    },

    BinOp {
        scope: Vec<u64>,
        name: BinOp,
        count: usize,
        tipo: Arc<Type>,
    },

    Assignment {
        scope: Vec<u64>,
        name: String,
        kind: AssignmentKind,
    },

    DefineFunc {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        params: Vec<String>,
        recursive: bool,
    },

    DefineConst {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        count: usize,
    },

    DefineConstrFields {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        count: usize,
    },

    DefineConstrFieldAccess {
        scope: Vec<u64>,
        func_name: String,
        module_name: String,
        count: usize,
    },

    Lam {
        scope: Vec<u64>,
        name: String,
    },

    // Try {
    //  scope: Vec<u64>,
    //     tipo: Arc<Type>,
    //     value: Box<Self>,
    //     then: Box<Self>,
    //     pattern: Pattern<PatternConstructor, Arc<Type>>,
    // },
    When {
        scope: Vec<u64>,
        count: usize,
        tipo: Arc<Type>,
        subject_name: String,
    },

    Clause {
        scope: Vec<u64>,
        count: usize,
        tipo: Arc<Type>,
        subject_name: String,
    },

    ClauseGuard {
        scope: Vec<u64>,
    },

    Discard {
        scope: Vec<u64>,
    },

    Finally {
        scope: Vec<u64>,
    },

    If {
        scope: Vec<u64>,
        count: usize,
    },

    Constr {
        scope: Vec<u64>,
        count: usize,
    },

    Fields {
        scope: Vec<u64>,
        count: usize,
    },

    RecordAccess {
        scope: Vec<u64>,
        index: u64,
        tipo: Arc<Type>,
    },

    FieldsExpose {
        scope: Vec<u64>,
        count: usize,
        indices: Vec<(usize, String, Arc<Type>)>,
    },

    // ModuleSelect {
    //  scope: Vec<u64>,
    //     tipo: Arc<Type>,
    //     label: String,
    //     module_name: String,
    //     module_alias: String,
    //     constructor: ModuleValueConstructor,
    // },

    // Tuple {
    //  scope: Vec<u64>,
    //
    // tipo: Arc<Type>,
    // elems: Vec<Self>,
    // },

    // TupleIndex {
    //  scope: Vec<u64>,
    //
    // tipo: Arc<Type>,
    // index: u64,
    // tuple: Box<Self>,
    // },
    Todo {
        scope: Vec<u64>,
        label: Option<String>,
        tipo: Arc<Type>,
    },

    RecordUpdate {
        scope: Vec<u64>,
        tipo: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },

    Negate {
        scope: Vec<u64>,
        value: Box<Self>,
    },
}

impl IR {
    pub fn scope(&self) -> Vec<u64> {
        match self {
            IR::Int { scope, .. }
            | IR::String { scope, .. }
            | IR::ByteArray { scope, .. }
            | IR::Var { scope, .. }
            | IR::List { scope, .. }
            | IR::Tail { scope, .. }
            | IR::ListAccessor { scope, .. }
            | IR::Call { scope, .. }
            | IR::Builtin { scope, .. }
            | IR::BinOp { scope, .. }
            | IR::Assignment { scope, .. }
            | IR::DefineFunc { scope, .. }
            | IR::DefineConst { scope, .. }
            | IR::DefineConstrFields { scope, .. }
            | IR::DefineConstrFieldAccess { scope, .. }
            | IR::Lam { scope, .. }
            | IR::When { scope, .. }
            | IR::Clause { scope, .. }
            | IR::ClauseGuard { scope }
            | IR::Discard { scope }
            | IR::Finally { scope }
            | IR::If { scope, .. }
            | IR::Constr { scope, .. }
            | IR::Fields { scope, .. }
            | IR::RecordAccess { scope, .. }
            | IR::FieldsExpose { scope, .. }
            | IR::Todo { scope, .. }
            | IR::RecordUpdate { scope, .. }
            | IR::Negate { scope, .. } => scope.to_vec(),
        }
    }
}
// pub fn get(r: Int) -> This{g: Int}{
// This{ g: r }
// }
//let x = get(rdmr)       when datum is { Hold(h, thing: f) -> h > x.g, Sell{price} -> x.g == price }

// [ When(3) datum_var Clause(2) FieldsExpose(3) datum_var h_field_var  f_field_var Binop(Greater, 2) h_var FieldAccess(2) x_var g_field Finally(2) Binop(2, EqualsInteger)  FieldAccess x_var g_field_var ]

// [ Assignment(2) ]

// [ Assignment(2) x_var ]

// [ Assignment(2) x_var Call(2) ]

// [ Assignment(2) x_var Call(2) func_get_var ]
//                                    ^Mark this index function insertion

// [ Assignment(2) x_var Call(2) func_get_var rdmr_var When(4)]

// [ Assignment(2) x_var Call(2) func_get_var ]

//  (var rdmr)

// [ Assignment(2) x_var Call(2) ]

// (var func_get)

// arg stack [ (var rdmr)]

// [ Assignment(2) x_var ]

// [(var func_get) (var rdmr)]

// [ Assignment(2)]

// (var x)

// arg stack [ [(var func_get) (var rdmr)] ]
