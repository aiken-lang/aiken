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
        value: String,
    },

    String {
        value: String,
    },

    ByteArray {
        bytes: Vec<u8>,
    },

    Var {
        constructor: ValueConstructor,
        name: String,
    },

    // Fn {
    //     tipo: Arc<Type>,
    //     is_capture: bool,
    //     args: Vec<Arg<Arc<Type>>>,
    //     body: Box<Self>,
    //     return_annotation: Option<Annotation>,
    // },
    List {
        count: usize,
        tipo: Arc<Type>,
        tail: bool,
    },

    Tail {
        count: usize,
    },

    ListAccessor {
        names: Vec<String>,
        tail: bool,
    },

    // func(x, other(y))
    //[ Define(3) x  definition *lam_body* -> [ (x [ (func [ func x [ (y [ other y ]) *definition* ] ]) *definition* ]) *definition* ], Define func -> [ (func [ func x [ (y [ other y ]) *definition* ] ]) *definition* ] , Call func -> [ func x [ (y [ other y ]) *definition* ] ], Var x -> x, Define y -> [ (y [ other y ]) *definition* ], Call other -> [ other y ], Var y -> y]

    // [y_varCall other_var Call Call x_defCall func_var ]
    Call {
        count: usize,
    },

    Builtin {
        func: DefaultFunction,
    },

    BinOp {
        name: BinOp,
        count: usize,
        tipo: Arc<Type>,
    },

    Assignment {
        name: String,
        kind: AssignmentKind,
    },

    DefineFunc {
        func_name: String,
        module_name: String,
    },

    DefineConst {
        func_name: String,
        module_name: String,
        count: usize,
    },

    DefineConstrFields {
        func_name: String,
        module_name: String,
        count: usize,
    },

    DefineConstrFieldAccess {
        func_name: String,
        module_name: String,
        count: usize,
    },

    // Try {
    //     tipo: Arc<Type>,
    //     value: Box<Self>,
    //     then: Box<Self>,
    //     pattern: Pattern<PatternConstructor, Arc<Type>>,
    // },
    When {
        count: usize,
    },

    Clause {
        count: usize,
    },

    Finally {
        count: usize,
    },

    If {
        count: usize,
    },

    Constr {
        count: usize,
    },

    Fields {
        count: usize,
    },

    RecordAccess {
        index: u64,
        tipo: Arc<Type>,
    },

    FieldsExpose {
        count: usize,
    },

    // ModuleSelect {
    //     tipo: Arc<Type>,
    //     label: String,
    //     module_name: String,
    //     module_alias: String,
    //     constructor: ModuleValueConstructor,
    // },

    // Tuple {
    //
    // tipo: Arc<Type>,
    // elems: Vec<Self>,
    // },

    // TupleIndex {
    //
    // tipo: Arc<Type>,
    // index: u64,
    // tuple: Box<Self>,
    // },
    Todo {
        label: Option<String>,
        tipo: Arc<Type>,
    },

    RecordUpdate {
        tipo: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },

    Negate {
        value: Box<Self>,
    },
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
