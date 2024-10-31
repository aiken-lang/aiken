use super::air::{Air, ExpectLevel, FunctionVariants};
use crate::{
    ast::{BinOp, Curve, Span, UnOp},
    tipo::{Type, ValueConstructor, ValueConstructorVariant},
};

use itertools::Itertools;
use std::{borrow::BorrowMut, rc::Rc, slice::Iter};
use uplc::{builder::INNER_EXPECT_ON_LIST, builtins::DefaultFunction};

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Fields {
    FirstField,
    SecondField,
    ThirdField,
    FourthField,
    FifthField,
    SixthField,
    SeventhField,
    EighthField,
    ArgsField(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TreePath {
    path: Vec<(usize, Fields)>,
}

impl TreePath {
    pub fn new() -> Self {
        TreePath { path: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    pub fn push(&mut self, depth: usize, index: Fields) {
        self.path.push((depth, index));
    }

    pub fn pop(&mut self) -> Option<(usize, Fields)> {
        self.path.pop()
    }

    pub fn common_ancestor(&self, other: &Self) -> Self {
        let mut common_ancestor = TreePath::new();

        let mut self_iter = self.path.iter();
        let mut other_iter = other.path.iter();

        let mut self_next = self_iter.next();
        let mut other_next = other_iter.next();

        while self_next.is_some() && other_next.is_some() {
            let self_next_level = self_next.unwrap();
            let other_next_level = other_next.unwrap();

            if self_next_level == other_next_level {
                common_ancestor.push(self_next_level.0, self_next_level.1);
            } else {
                break;
            }

            self_next = self_iter.next();
            other_next = other_iter.next();
        }

        common_ancestor
    }
}

impl Default for TreePath {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexCounter {
    current_index: usize,
}

impl IndexCounter {
    pub fn new() -> Self {
        IndexCounter { current_index: 0 }
    }

    /// Returns the next of this [`IndexCounter`].
    pub fn next_number(&mut self) -> usize {
        let current_index = self.current_index;
        self.current_index += 1;
        current_index
    }
}

impl Default for IndexCounter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirMsg {
    LocalVar(String),
    Msg(String),
}

impl AirMsg {
    pub fn to_air_tree(&self) -> AirTree {
        match self {
            AirMsg::LocalVar(name) => AirTree::local_var(name, Type::string()),
            AirMsg::Msg(msg) => AirTree::string(msg),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AirTree {
    // Statements
    Let {
        name: String,
        value: Box<AirTree>,
        then: Box<AirTree>,
    },
    SoftCastLet {
        name: String,
        tipo: Rc<Type>,
        value: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    DefineFunc {
        func_name: String,
        module_name: String,
        variant_name: String,
        //params and other parts of a function
        params: Vec<String>,
        recursive: bool,
        recursive_nonstatic_params: Vec<String>,
        func_body: Box<AirTree>,
        then: Box<AirTree>,
    },
    DefineCyclicFuncs {
        func_name: String,
        module_name: String,
        variant_name: String,
        // params and body
        contained_functions: Vec<(Vec<String>, AirTree)>,
        then: Box<AirTree>,
    },
    AssertBool {
        is_true: bool,
        value: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // Field Access
    FieldsExpose {
        indices: Vec<(usize, String, Rc<Type>)>,
        record: Box<AirTree>,
        is_expect: bool,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // List Access
    ListAccessor {
        tipo: Rc<Type>,
        names: Vec<String>,
        tail: bool,
        list: Box<AirTree>,
        expect_level: ExpectLevel,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // Tuple Access
    TupleAccessor {
        names: Vec<String>,
        tipo: Rc<Type>,
        tuple: Box<AirTree>,
        is_expect: bool,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // Pair Access
    PairAccessor {
        fst: Option<String>,
        snd: Option<String>,
        tipo: Rc<Type>,
        is_expect: bool,
        pair: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    ExtractField {
        tipo: Rc<Type>,
        arg: Box<AirTree>,
    },
    // Misc.
    FieldsEmpty {
        constr: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    ListEmpty {
        list: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    NoOp {
        then: Box<AirTree>,
    },
    // End Statements

    // Expressions
    // Primitives
    Int {
        value: String,
    },
    String {
        value: String,
    },
    ByteArray {
        bytes: Vec<u8>,
    },
    CurvePoint {
        point: Curve,
    },
    Bool {
        value: bool,
    },
    List {
        tipo: Rc<Type>,
        tail: bool,
        items: Vec<AirTree>,
    },
    Tuple {
        tipo: Rc<Type>,
        items: Vec<AirTree>,
    },
    Pair {
        tipo: Rc<Type>,
        fst: Box<AirTree>,
        snd: Box<AirTree>,
    },
    Void,
    Var {
        constructor: ValueConstructor,
        name: String,
        variant_name: String,
    },
    // Functions
    Call {
        tipo: Rc<Type>,
        func: Box<AirTree>,
        args: Vec<AirTree>,
    },

    Fn {
        params: Vec<String>,
        func_body: Box<AirTree>,
        allow_inline: bool,
    },
    Builtin {
        func: DefaultFunction,
        tipo: Rc<Type>,
        args: Vec<AirTree>,
    },
    // Operators
    BinOp {
        name: BinOp,
        tipo: Rc<Type>,
        left: Box<AirTree>,
        right: Box<AirTree>,
        argument_tipo: Rc<Type>,
    },
    UnOp {
        op: UnOp,
        arg: Box<AirTree>,
    },

    CastFromData {
        tipo: Rc<Type>,
        value: Box<AirTree>,
        full_cast: bool,
    },
    CastToData {
        tipo: Rc<Type>,
        value: Box<AirTree>,
    },

    // When
    When {
        tipo: Rc<Type>,
        subject_name: String,
        subject: Box<AirTree>,
        subject_tipo: Rc<Type>,
        clauses: Box<AirTree>,
    },
    Clause {
        subject_tipo: Rc<Type>,
        subject_name: String,
        pattern: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    ListClause {
        subject_tipo: Rc<Type>,
        tail_name: String,
        next_tail_name: Option<(String, String)>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // If
    If {
        tipo: Rc<Type>,
        condition: Box<AirTree>,
        then: Box<AirTree>,
        otherwise: Box<AirTree>,
    },
    // Record Creation
    Constr {
        tag: usize,
        tipo: Rc<Type>,
        args: Vec<AirTree>,
    },
    RecordUpdate {
        highest_index: usize,
        indices: Vec<(usize, Rc<Type>)>,
        tipo: Rc<Type>,
        record: Box<AirTree>,
        args: Vec<AirTree>,
    },
    // Misc.
    ErrorTerm {
        tipo: Rc<Type>,
        validator: bool,
    },
    Trace {
        tipo: Rc<Type>,
        msg: Box<AirTree>,
        then: Box<AirTree>,
    },
}

impl AirTree {
    pub fn is_error(&self) -> bool {
        matches!(self, AirTree::ErrorTerm { .. })
    }

    pub fn int(value: impl ToString) -> AirTree {
        AirTree::Int {
            value: value.to_string(),
        }
    }

    pub fn string(value: impl ToString) -> AirTree {
        AirTree::String {
            value: value.to_string(),
        }
    }

    pub fn byte_array(bytes: Vec<u8>) -> AirTree {
        AirTree::ByteArray { bytes }
    }

    pub fn curve(point: Curve) -> AirTree {
        AirTree::CurvePoint { point }
    }

    pub fn bool(value: bool) -> AirTree {
        AirTree::Bool { value }
    }

    pub fn list(mut items: Vec<AirTree>, tipo: Rc<Type>, tail: Option<AirTree>) -> AirTree {
        if let Some(tail) = tail {
            items.push(tail);

            AirTree::List {
                tipo,
                tail: true,
                items,
            }
        } else {
            AirTree::List {
                tipo,
                tail: false,
                items,
            }
        }
    }

    pub fn tuple(items: Vec<AirTree>, tipo: Rc<Type>) -> AirTree {
        AirTree::Tuple { tipo, items }
    }

    pub fn pair(fst: AirTree, snd: AirTree, tipo: Rc<Type>) -> AirTree {
        AirTree::Pair {
            tipo,
            fst: fst.into(),
            snd: snd.into(),
        }
    }

    pub fn void() -> AirTree {
        AirTree::Void
    }

    pub fn var(
        constructor: ValueConstructor,
        name: impl ToString,
        variant_name: impl ToString,
    ) -> AirTree {
        AirTree::Var {
            constructor,
            name: name.to_string(),
            variant_name: variant_name.to_string(),
        }
    }

    pub fn local_var(name: impl ToString, tipo: Rc<Type>) -> AirTree {
        AirTree::Var {
            constructor: ValueConstructor::public(
                tipo,
                ValueConstructorVariant::LocalVariable {
                    location: Span::empty(),
                },
            ),
            name: name.to_string(),
            variant_name: "".to_string(),
        }
    }

    pub fn call(func: AirTree, tipo: Rc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Call {
            tipo,
            func: func.into(),
            args,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn define_func(
        func_name: impl ToString,
        module_name: impl ToString,
        variant_name: impl ToString,
        params: Vec<String>,
        recursive: bool,
        recursive_nonstatic_params: Vec<String>,
        func_body: AirTree,
        then: AirTree,
    ) -> AirTree {
        AirTree::DefineFunc {
            func_name: func_name.to_string(),
            module_name: module_name.to_string(),
            params,
            recursive,
            recursive_nonstatic_params,

            variant_name: variant_name.to_string(),
            func_body: func_body.into(),
            then: then.into(),
        }
    }

    pub fn define_cyclic_func(
        func_name: impl ToString,
        module_name: impl ToString,
        variant_name: impl ToString,
        contained_functions: Vec<(Vec<String>, AirTree)>,
        then: AirTree,
    ) -> AirTree {
        AirTree::DefineCyclicFuncs {
            func_name: func_name.to_string(),
            module_name: module_name.to_string(),
            variant_name: variant_name.to_string(),
            contained_functions,
            then: then.into(),
        }
    }

    pub fn anon_func(params: Vec<String>, func_body: AirTree, allow_inline: bool) -> AirTree {
        AirTree::Fn {
            params,
            func_body: func_body.into(),
            allow_inline,
        }
    }

    pub fn builtin(func: DefaultFunction, tipo: Rc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Builtin { func, tipo, args }
    }

    pub fn binop(
        op: BinOp,
        tipo: Rc<Type>,
        left: AirTree,
        right: AirTree,
        argument_tipo: Rc<Type>,
    ) -> AirTree {
        AirTree::BinOp {
            name: op,
            tipo,
            left: left.into(),
            right: right.into(),
            argument_tipo,
        }
    }

    pub fn unop(op: UnOp, arg: AirTree) -> AirTree {
        AirTree::UnOp {
            op,
            arg: arg.into(),
        }
    }

    pub fn let_assignment(name: impl ToString, value: AirTree, then: AirTree) -> AirTree {
        AirTree::Let {
            name: name.to_string(),
            value: value.into(),
            then: then.into(),
        }
    }

    pub fn soft_cast_assignment(
        name: impl ToString,
        tipo: Rc<Type>,
        value: AirTree,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::SoftCastLet {
            name: name.to_string(),
            tipo,
            value: value.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn cast_from_data(value: AirTree, tipo: Rc<Type>, full_cast: bool) -> AirTree {
        AirTree::CastFromData {
            tipo,
            value: value.into(),
            full_cast,
        }
    }

    pub fn cast_to_data(value: AirTree, tipo: Rc<Type>) -> AirTree {
        AirTree::CastToData {
            tipo,
            value: value.into(),
        }
    }

    pub fn assert_bool(
        is_true: bool,
        value: AirTree,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::AssertBool {
            is_true,
            value: value.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn when(
        subject_name: impl ToString,
        tipo: Rc<Type>,
        subject_tipo: Rc<Type>,
        subject: AirTree,
        clauses: AirTree,
    ) -> AirTree {
        AirTree::When {
            tipo,
            subject_name: subject_name.to_string(),
            subject: subject.into(),
            subject_tipo,
            clauses: clauses.into(),
        }
    }

    pub fn clause(
        subject_name: impl ToString,
        pattern: AirTree,
        subject_tipo: Rc<Type>,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::Clause {
            subject_tipo,
            subject_name: subject_name.to_string(),
            pattern: pattern.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn list_clause(
        tail_name: impl ToString,
        subject_tipo: Rc<Type>,
        then: AirTree,
        otherwise: AirTree,
        next_tail_name: Option<(String, String)>,
    ) -> AirTree {
        AirTree::ListClause {
            subject_tipo,
            tail_name: tail_name.to_string(),
            next_tail_name,
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn if_branch(
        tipo: Rc<Type>,
        condition: AirTree,
        branch: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::If {
            tipo,
            condition: condition.into(),
            then: branch.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn create_constr(tag: usize, tipo: Rc<Type>, args: Vec<AirTree>) -> AirTree {
        AirTree::Constr { tag, tipo, args }
    }

    pub fn record_update(
        indices: Vec<(usize, Rc<Type>)>,
        highest_index: usize,
        tipo: Rc<Type>,
        record: AirTree,
        args: Vec<AirTree>,
    ) -> AirTree {
        AirTree::RecordUpdate {
            highest_index,
            indices,
            tipo,
            record: record.into(),
            args,
        }
    }

    pub fn index_access(function_name: String, tipo: Rc<Type>, list_of_fields: AirTree) -> AirTree {
        AirTree::cast_from_data(
            AirTree::call(
                AirTree::var(
                    ValueConstructor::public(
                        Type::Fn {
                            args: vec![Type::list(Type::data())],
                            ret: Type::data(),
                            alias: None,
                        }
                        .into(),
                        ValueConstructorVariant::ModuleFn {
                            name: function_name.clone(),
                            field_map: None,
                            module: "".to_string(),
                            arity: 1,
                            location: Span::empty(),
                            builtin: None,
                        },
                    ),
                    function_name,
                    "",
                ),
                Type::data(),
                vec![list_of_fields],
            ),
            tipo.clone(),
            false,
        )
    }

    pub fn fields_expose(
        indices: Vec<(usize, String, Rc<Type>)>,
        record: AirTree,
        is_expect: bool,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::FieldsExpose {
            indices,
            record: record.into(),
            is_expect,
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn list_access(
        names: Vec<String>,
        tipo: Rc<Type>,
        tail: bool,
        list: AirTree,

        expect_level: ExpectLevel,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::ListAccessor {
            tipo,
            names,
            tail,
            list: list.into(),
            expect_level,
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn tuple_access(
        names: Vec<String>,
        tipo: Rc<Type>,
        tuple: AirTree,
        is_expect: bool,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::TupleAccessor {
            names,
            tipo,
            tuple: tuple.into(),
            is_expect,
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn pair_access(
        fst: Option<String>,
        snd: Option<String>,
        tipo: Rc<Type>,
        pair: AirTree,
        is_expect: bool,
        then: AirTree,
        otherwise: AirTree,
    ) -> AirTree {
        AirTree::PairAccessor {
            fst,
            snd,
            tipo,
            is_expect,
            pair: pair.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn extract_field(tipo: Rc<Type>, arg: AirTree) -> AirTree {
        AirTree::ExtractField {
            tipo,
            arg: arg.into(),
        }
    }

    pub fn pair_index(index: usize, tipo: Rc<Type>, tuple: AirTree) -> AirTree {
        AirTree::cast_from_data(
            AirTree::builtin(
                if index == 0 {
                    DefaultFunction::FstPair
                } else {
                    DefaultFunction::SndPair
                },
                Type::data(),
                vec![tuple],
            ),
            tipo.clone(),
            false,
        )
    }

    pub fn error(tipo: Rc<Type>, validator: bool) -> AirTree {
        AirTree::ErrorTerm { tipo, validator }
    }

    pub fn trace(msg: AirTree, tipo: Rc<Type>, then: AirTree) -> AirTree {
        AirTree::Trace {
            tipo,
            msg: msg.into(),
            then: then.into(),
        }
    }
    pub fn no_op(then: AirTree) -> AirTree {
        AirTree::NoOp { then: then.into() }
    }

    pub fn fields_empty(constr: AirTree, then: AirTree, otherwise: AirTree) -> AirTree {
        AirTree::FieldsEmpty {
            constr: constr.into(),
            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn list_empty(list: AirTree, then: AirTree, otherwise: AirTree) -> AirTree {
        AirTree::ListEmpty {
            list: list.into(),

            then: then.into(),
            otherwise: otherwise.into(),
        }
    }

    pub fn expect_on_list2() -> AirTree {
        let inner_expect_on_list = AirTree::local_var(INNER_EXPECT_ON_LIST, Type::void());

        let list_var = AirTree::local_var("__list_to_check", Type::list(Type::data()));

        AirTree::let_assignment(
            INNER_EXPECT_ON_LIST,
            AirTree::anon_func(
                vec![
                    INNER_EXPECT_ON_LIST.to_string(),
                    "__list_to_check".to_string(),
                ],
                AirTree::call(
                    AirTree::local_var("__check_with", Type::void()),
                    Type::void(),
                    vec![
                        list_var.clone(),
                        AirTree::call(
                            inner_expect_on_list.clone(),
                            Type::void(),
                            vec![inner_expect_on_list.clone()],
                        ),
                    ],
                ),
                false,
            ),
            AirTree::call(
                inner_expect_on_list.clone(),
                Type::void(),
                vec![inner_expect_on_list, list_var],
            ),
        )
    }

    pub fn to_vec(&self) -> Vec<Air> {
        let mut air_vec = vec![];
        self.create_air_vec(&mut air_vec);
        air_vec
    }

    fn create_air_vec(&self, air_vec: &mut Vec<Air>) {
        match self {
            AirTree::Let { name, value, then } => {
                air_vec.push(Air::Let { name: name.clone() });
                value.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
            }
            AirTree::SoftCastLet {
                name,
                tipo,
                value,
                then,
                otherwise,
            } => {
                air_vec.push(Air::SoftCastLet {
                    name: name.clone(),
                    tipo: tipo.clone(),
                });
                value.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::DefineFunc {
                func_name,
                module_name,
                params,
                recursive,
                recursive_nonstatic_params,
                variant_name,
                func_body,
                then,
            } => {
                let variant = if *recursive {
                    FunctionVariants::Recursive {
                        params: params.clone(),
                        recursive_nonstatic_params: recursive_nonstatic_params.clone(),
                    }
                } else {
                    assert_eq!(params, recursive_nonstatic_params);
                    FunctionVariants::Standard(params.clone())
                };

                air_vec.push(Air::DefineFunc {
                    func_name: func_name.clone(),
                    module_name: module_name.clone(),
                    variant_name: variant_name.clone(),
                    variant,
                });
                func_body.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
            }
            AirTree::DefineCyclicFuncs {
                func_name,
                module_name,
                variant_name,
                contained_functions,
                then,
            } => {
                let variant = FunctionVariants::Cyclic(
                    contained_functions
                        .iter()
                        .map(|(params, _)| params.clone())
                        .collect_vec(),
                );

                air_vec.push(Air::DefineFunc {
                    func_name: func_name.clone(),
                    module_name: module_name.clone(),
                    variant_name: variant_name.clone(),
                    variant,
                });

                for (_, func_body) in contained_functions {
                    func_body.create_air_vec(air_vec);
                }
                then.create_air_vec(air_vec);
            }
            AirTree::AssertBool {
                is_true,
                value,
                then,
                otherwise,
            } => {
                air_vec.push(Air::AssertBool { is_true: *is_true });

                value.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::FieldsExpose {
                indices,
                record,
                is_expect,
                then,
                otherwise,
            } => {
                air_vec.push(Air::FieldsExpose {
                    indices: indices.clone(),
                    is_expect: *is_expect,
                });

                record.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                if *is_expect {
                    otherwise.create_air_vec(air_vec);
                }
            }
            AirTree::ListAccessor {
                tipo,
                names,
                tail,
                list,
                expect_level,
                then,
                otherwise,
            } => {
                air_vec.push(Air::ListAccessor {
                    tipo: tipo.clone(),
                    names: names.clone(),
                    tail: *tail,
                    expect_level: *expect_level,
                });

                list.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                if matches!(expect_level, ExpectLevel::Full | ExpectLevel::Items) {
                    otherwise.create_air_vec(air_vec);
                }
            }
            AirTree::TupleAccessor {
                names,
                tipo,
                tuple,
                is_expect,
                then,
                otherwise,
            } => {
                air_vec.push(Air::TupleAccessor {
                    names: names.clone(),
                    tipo: tipo.clone(),
                    is_expect: *is_expect,
                });

                tuple.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                if *is_expect {
                    otherwise.create_air_vec(air_vec);
                }
            }
            AirTree::PairAccessor {
                fst,
                snd,
                tipo,
                is_expect,
                pair,
                then,
                otherwise,
            } => {
                air_vec.push(Air::PairAccessor {
                    fst: fst.clone(),
                    snd: snd.clone(),
                    tipo: tipo.clone(),
                    is_expect: *is_expect,
                });

                pair.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                if *is_expect {
                    otherwise.create_air_vec(air_vec);
                }
            }
            AirTree::FieldsEmpty {
                constr,
                then,
                otherwise,
            } => {
                air_vec.push(Air::FieldsEmpty);

                constr.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::ListEmpty {
                list,
                then,
                otherwise,
            } => {
                air_vec.push(Air::ListEmpty);

                list.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::NoOp { then } => {
                air_vec.push(Air::NoOp);
                then.create_air_vec(air_vec);
            }
            AirTree::Int { value } => air_vec.push(Air::Int {
                value: value.clone(),
            }),

            AirTree::String { value } => air_vec.push(Air::String {
                value: value.clone(),
            }),
            AirTree::ByteArray { bytes } => air_vec.push(Air::ByteArray {
                bytes: bytes.clone(),
            }),
            AirTree::CurvePoint { point } => air_vec.push(Air::CurvePoint { point: *point }),
            AirTree::Bool { value } => air_vec.push(Air::Bool { value: *value }),
            AirTree::List { tipo, tail, items } => {
                air_vec.push(Air::List {
                    count: items.len(),
                    tipo: tipo.clone(),
                    tail: *tail,
                });
                for item in items {
                    item.create_air_vec(air_vec);
                }
            }
            AirTree::Tuple { tipo, items } => {
                air_vec.push(Air::Tuple {
                    tipo: tipo.clone(),
                    count: items.len(),
                });
                for item in items {
                    item.create_air_vec(air_vec);
                }
            }
            AirTree::Pair { tipo, fst, snd } => {
                air_vec.push(Air::Pair { tipo: tipo.clone() });
                fst.create_air_vec(air_vec);
                snd.create_air_vec(air_vec);
            }
            AirTree::Void => air_vec.push(Air::Void),
            AirTree::Var {
                constructor,
                name,
                variant_name,
            } => air_vec.push(Air::Var {
                constructor: constructor.clone(),
                name: name.clone(),
                variant_name: variant_name.clone(),
            }),
            AirTree::Call { tipo, func, args } => {
                air_vec.push(Air::Call {
                    count: args.len(),
                    tipo: tipo.clone(),
                });
                func.create_air_vec(air_vec);
                for arg in args {
                    arg.create_air_vec(air_vec);
                }
            }
            AirTree::Fn {
                params,
                func_body,
                allow_inline,
            } => {
                air_vec.push(Air::Fn {
                    params: params.clone(),
                    allow_inline: *allow_inline,
                });
                func_body.create_air_vec(air_vec);
            }
            AirTree::Builtin { func, tipo, args } => {
                air_vec.push(Air::Builtin {
                    count: args.len(),
                    func: *func,
                    tipo: tipo.clone(),
                });

                for arg in args {
                    arg.create_air_vec(air_vec);
                }
            }
            AirTree::BinOp {
                name,
                tipo,
                left,
                right,
                argument_tipo,
            } => {
                air_vec.push(Air::BinOp {
                    name: *name,
                    tipo: tipo.clone(),
                    argument_tipo: argument_tipo.clone(),
                });
                left.create_air_vec(air_vec);
                right.create_air_vec(air_vec);
            }
            AirTree::UnOp { op, arg } => {
                air_vec.push(Air::UnOp { op: *op });
                arg.create_air_vec(air_vec);
            }
            AirTree::CastFromData {
                tipo,
                value,
                full_cast,
            } => {
                air_vec.push(Air::CastFromData {
                    tipo: tipo.clone(),
                    full_cast: *full_cast,
                });

                value.create_air_vec(air_vec);
            }
            AirTree::CastToData { tipo, value } => {
                air_vec.push(Air::CastToData { tipo: tipo.clone() });
                value.create_air_vec(air_vec);
            }
            AirTree::When {
                tipo,
                subject_name,
                subject,
                subject_tipo,
                clauses,
            } => {
                air_vec.push(Air::When {
                    tipo: tipo.clone(),
                    subject_name: subject_name.clone(),
                    subject_tipo: subject_tipo.clone(),
                });
                subject.create_air_vec(air_vec);
                clauses.create_air_vec(air_vec);
            }
            AirTree::Clause {
                subject_tipo,
                subject_name,
                pattern,
                then,
                otherwise,
            } => {
                air_vec.push(Air::Clause {
                    subject_tipo: subject_tipo.clone(),
                    subject_name: subject_name.clone(),
                });
                pattern.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::ListClause {
                subject_tipo,
                tail_name,
                next_tail_name,

                then,
                otherwise,
            } => {
                air_vec.push(Air::ListClause {
                    subject_tipo: subject_tipo.clone(),
                    tail_name: tail_name.clone(),
                    next_tail_name: next_tail_name.clone(),
                });
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::If {
                tipo,
                condition: pattern,
                then,
                otherwise,
            } => {
                air_vec.push(Air::If { tipo: tipo.clone() });
                pattern.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
                otherwise.create_air_vec(air_vec);
            }
            AirTree::Constr { tag, tipo, args } => {
                air_vec.push(Air::Constr {
                    tag: *tag,
                    tipo: tipo.clone(),
                    count: args.len(),
                });
                for arg in args {
                    arg.create_air_vec(air_vec);
                }
            }
            AirTree::RecordUpdate {
                highest_index,
                indices,
                tipo,
                record,
                args,
            } => {
                air_vec.push(Air::RecordUpdate {
                    highest_index: *highest_index,
                    indices: indices.clone(),
                    tipo: tipo.clone(),
                });
                record.create_air_vec(air_vec);
                for arg in args {
                    arg.create_air_vec(air_vec);
                }
            }
            AirTree::ErrorTerm { tipo, validator } => air_vec.push(Air::ErrorTerm {
                tipo: tipo.clone(),
                validator: *validator,
            }),
            AirTree::Trace { tipo, msg, then } => {
                air_vec.push(Air::Trace { tipo: tipo.clone() });
                msg.create_air_vec(air_vec);
                then.create_air_vec(air_vec);
            }
            AirTree::ExtractField {
                tipo,
                arg: args_list,
            } => {
                air_vec.push(Air::ExtractField { tipo: tipo.clone() });
                args_list.create_air_vec(air_vec);
            }
        }
    }

    pub fn return_type(&self) -> Rc<Type> {
        match self {
            AirTree::Int { .. } => Type::int(),
            AirTree::String { .. } => Type::string(),
            AirTree::ByteArray { .. } => Type::byte_array(),
            AirTree::Bool { .. } => Type::bool(),
            AirTree::CurvePoint { point } => point.tipo(),
            AirTree::List { tipo, .. }
            | AirTree::Tuple { tipo, .. }
            | AirTree::Pair { tipo, .. }
            | AirTree::Call { tipo, .. }
            | AirTree::Builtin { tipo, .. }
            | AirTree::ExtractField { tipo, .. }
            | AirTree::BinOp { tipo, .. }
            | AirTree::CastFromData { tipo, .. }
            | AirTree::When { tipo, .. }
            | AirTree::If { tipo, .. }
            | AirTree::Constr { tipo, .. }
            | AirTree::RecordUpdate { tipo, .. }
            | AirTree::ErrorTerm { tipo, .. }
            | AirTree::Trace { tipo, .. } => tipo.clone(),
            AirTree::Void => Type::void(),
            AirTree::Var { constructor, .. } => constructor.tipo.clone(),
            AirTree::Fn { func_body, .. } => func_body.return_type(),
            AirTree::UnOp { op, .. } => match op {
                UnOp::Not => Type::bool(),
                UnOp::Negate => Type::int(),
            },
            AirTree::CastToData { .. } => Type::data(),
            AirTree::Clause { then, .. }
            | AirTree::ListClause { then, .. }
            | AirTree::Let { then, .. }
            | AirTree::SoftCastLet { then, .. }
            | AirTree::DefineFunc { then, .. }
            | AirTree::DefineCyclicFuncs { then, .. }
            | AirTree::AssertBool { then, .. }
            | AirTree::FieldsExpose { then, .. }
            | AirTree::ListAccessor { then, .. }
            | AirTree::TupleAccessor { then, .. }
            | AirTree::PairAccessor { then, .. }
            | AirTree::FieldsEmpty { then, .. }
            | AirTree::ListEmpty { then, .. }
            | AirTree::NoOp { then } => then.return_type(),
        }
    }

    pub fn mut_held_types(&mut self) -> Vec<&mut Rc<Type>> {
        match self {
            AirTree::Clause { subject_tipo, .. } | AirTree::ListClause { subject_tipo, .. } => {
                vec![subject_tipo]
            }

            AirTree::ListAccessor { tipo, .. }
            | AirTree::TupleAccessor { tipo, .. }
            | AirTree::PairAccessor { tipo, .. }
            | AirTree::List { tipo, .. }
            | AirTree::Tuple { tipo, .. }
            | AirTree::Call { tipo, .. }
            | AirTree::Builtin { tipo, .. }
            | AirTree::ExtractField { tipo, .. }
            | AirTree::CastFromData { tipo, .. }
            | AirTree::CastToData { tipo, .. }
            | AirTree::If { tipo, .. }
            | AirTree::Constr { tipo, .. }
            | AirTree::ErrorTerm { tipo, .. }
            | AirTree::Trace { tipo, .. }
            | AirTree::Pair { tipo, .. }
            | AirTree::SoftCastLet { tipo, .. } => vec![tipo],

            AirTree::FieldsExpose { indices, .. } => {
                let mut types = vec![];
                for (_, _, tipo) in indices {
                    types.push(tipo);
                }
                types
            }

            AirTree::Var { constructor, .. } => {
                vec![constructor.tipo.borrow_mut()]
            }
            AirTree::BinOp {
                tipo,
                argument_tipo,
                ..
            } => {
                vec![tipo, argument_tipo]
            }
            AirTree::When {
                tipo, subject_tipo, ..
            } => vec![tipo, subject_tipo],

            AirTree::RecordUpdate { tipo, indices, .. } => {
                let mut types = vec![tipo];
                for (_, tipo) in indices {
                    types.push(tipo);
                }
                types
            }
            AirTree::Let { .. }
            | AirTree::DefineFunc { .. }
            | AirTree::DefineCyclicFuncs { .. }
            | AirTree::AssertBool { .. }
            | AirTree::FieldsEmpty { .. }
            | AirTree::ListEmpty { .. }
            | AirTree::NoOp { .. }
            | AirTree::Int { .. }
            | AirTree::String { .. }
            | AirTree::ByteArray { .. }
            | AirTree::CurvePoint { .. }
            | AirTree::Bool { .. }
            | AirTree::Void
            | AirTree::Fn { .. }
            | AirTree::UnOp { .. } => vec![],
        }
    }

    pub fn traverse_tree_with(&mut self, with: &mut impl FnMut(&mut AirTree, &TreePath)) {
        let mut tree_path = TreePath::new();
        self.do_traverse_tree_with(&mut tree_path, 0, Fields::FirstField, with);
    }

    pub fn traverse_tree_with_path(
        &mut self,
        path: &mut TreePath,
        current_depth: usize,
        depth_index: Fields,
        with: &mut impl FnMut(&mut AirTree, &TreePath),
    ) {
        self.do_traverse_tree_with(path, current_depth, depth_index, with);
    }

    fn do_traverse_tree_with(
        &mut self,
        tree_path: &mut TreePath,
        current_depth: usize,
        field_index: Fields,
        with: &mut impl FnMut(&mut AirTree, &TreePath),
    ) {
        tree_path.push(current_depth, field_index);

        // TODO: Merge together the 2 match statements

        match self {
            AirTree::Let {
                name: _,
                value,
                then: _,
            } => {
                value.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );
            }

            AirTree::SoftCastLet {
                name: _,
                tipo: _,
                value,
                then: _,
                otherwise,
            } => {
                value.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);

                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FifthField,
                    with,
                );
            }

            AirTree::AssertBool {
                is_true: _,
                value,
                then: _,
                otherwise,
            } => {
                value.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );
                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FourthField,
                    with,
                )
            }
            AirTree::FieldsExpose {
                indices: _,
                record,
                is_expect: _,
                then: _,
                otherwise,
            } => {
                record.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );
                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FifthField,
                    with,
                )
            }
            AirTree::ListAccessor {
                tipo: _,
                names: _,
                tail: _,
                list,
                expect_level: _,
                then: _,
                otherwise,
            } => {
                list.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FourthField, with);
                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SeventhField,
                    with,
                )
            }
            AirTree::TupleAccessor {
                names: _,
                tipo: _,
                tuple,
                is_expect: _,
                then: _,
                otherwise,
            } => {
                tuple.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);
                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SixthField,
                    with,
                )
            }
            AirTree::PairAccessor {
                fst: _,
                snd: _,
                tipo: _,
                is_expect: _,
                pair,
                then: _,
                otherwise,
            } => {
                pair.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FifthField, with);
                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SeventhField,
                    with,
                )
            }
            AirTree::FieldsEmpty {
                constr,
                then: _,
                otherwise,
            } => {
                constr.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FirstField,
                    with,
                );

                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::ThirdField,
                    with,
                );
            }
            AirTree::ListEmpty {
                list,
                then: _,
                otherwise,
            } => {
                list.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FirstField, with);
                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::ThirdField,
                    with,
                )
            }

            AirTree::When {
                tipo: _,
                subject_name: _,
                subject,
                subject_tipo: _,
                clauses: _,
            } => subject.do_traverse_tree_with(
                tree_path,
                current_depth + 1,
                Fields::ThirdField,
                with,
            ),
            AirTree::DefineFunc { .. }
            | AirTree::DefineCyclicFuncs { .. }
            | AirTree::NoOp { .. }
            | AirTree::Int { .. }
            | AirTree::String { .. }
            | AirTree::ByteArray { .. }
            | AirTree::CurvePoint { .. }
            | AirTree::Bool { .. }
            | AirTree::List { .. }
            | AirTree::Tuple { .. }
            | AirTree::Pair { .. }
            | AirTree::Void
            | AirTree::Var { .. }
            | AirTree::Call { .. }
            | AirTree::Fn { .. }
            | AirTree::Builtin { .. }
            | AirTree::BinOp { .. }
            | AirTree::UnOp { .. }
            | AirTree::CastFromData { .. }
            | AirTree::CastToData { .. }
            | AirTree::Clause { .. }
            | AirTree::ListClause { .. }
            | AirTree::If { .. }
            | AirTree::Constr { .. }
            | AirTree::RecordUpdate { .. }
            | AirTree::ErrorTerm { .. }
            | AirTree::Trace { .. }
            | AirTree::ExtractField { .. } => {}
        }

        match self {
            AirTree::NoOp { then } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FirstField, with);
            }
            AirTree::When {
                tipo: _,
                subject_name: _,
                subject: _,
                subject_tipo: _,
                clauses,
            } => {
                clauses.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FifthField,
                    with,
                );
            }
            AirTree::List {
                tipo: _,
                tail: _,
                items,
            } => {
                for (index, item) in items.iter_mut().enumerate() {
                    item.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
            }
            AirTree::Tuple { tipo: _, items } => {
                for (index, item) in items.iter_mut().enumerate() {
                    item.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
            }
            AirTree::Pair { tipo: _, fst, snd } => {
                fst.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);

                snd.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);
            }
            AirTree::Call {
                tipo: _,
                func,
                args,
            } => {
                func.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);

                for (index, arg) in args.iter_mut().enumerate() {
                    arg.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
            }
            AirTree::Fn {
                params: _,
                func_body,
                allow_inline: _,
            } => {
                func_body.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );
            }
            AirTree::Builtin {
                func: _,
                tipo: _,
                args,
            } => {
                for (index, arg) in args.iter_mut().enumerate() {
                    arg.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
            }
            AirTree::ExtractField { tipo: _, arg } => {
                arg.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);
            }
            AirTree::BinOp {
                name: _,
                tipo: _,
                left,
                right,
                argument_tipo: _,
            } => {
                left.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);

                right.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FourthField,
                    with,
                );
            }
            AirTree::UnOp { op: _, arg } => {
                arg.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);
            }
            AirTree::CastFromData {
                tipo: _,
                value,
                full_cast: _,
            } => {
                value.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );
            }
            AirTree::CastToData { tipo: _, value } => {
                value.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );
            }

            AirTree::Clause {
                subject_tipo: _,
                subject_name: _,
                pattern,
                then,
                otherwise,
            } => {
                pattern.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::ThirdField,
                    with,
                );

                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FourthField, with);

                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FifthField,
                    with,
                );
            }
            AirTree::ListClause {
                subject_tipo: _,
                tail_name: _,
                next_tail_name: _,
                then,
                otherwise,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FourthField, with);

                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FifthField,
                    with,
                );
            }
            AirTree::If {
                tipo: _,
                condition: pattern,
                then,
                otherwise,
            } => {
                pattern.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SecondField,
                    with,
                );

                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);

                otherwise.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FourthField,
                    with,
                );
            }
            AirTree::Constr {
                tag: _,
                tipo: _,
                args,
            } => {
                for (index, arg) in args.iter_mut().enumerate() {
                    arg.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
            }
            AirTree::RecordUpdate {
                highest_index: _,
                indices: _,
                tipo: _,
                record,
                args,
            } => {
                record.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::FourthField,
                    with,
                );

                for (index, arg) in args.iter_mut().enumerate() {
                    arg.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
            }
            AirTree::Trace { tipo: _, msg, then } => {
                msg.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);

                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);
            }
            AirTree::DefineFunc {
                func_name: _,
                module_name: _,
                params: _,
                recursive: _,
                recursive_nonstatic_params: _,
                variant_name: _,
                func_body,
                then,
            } => {
                func_body.do_traverse_tree_with(
                    tree_path,
                    current_depth + 1,
                    Fields::SeventhField,
                    with,
                );
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::EighthField, with)
            }
            AirTree::DefineCyclicFuncs {
                func_name: _,
                module_name: _,
                variant_name: _,
                contained_functions,
                then,
            } => {
                for (index, (_, func_body)) in contained_functions.iter_mut().enumerate() {
                    func_body.do_traverse_tree_with(
                        tree_path,
                        current_depth + 1,
                        Fields::ArgsField(index),
                        with,
                    );
                }
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FifthField, with);
            }
            AirTree::Int { .. }
            | AirTree::String { .. }
            | AirTree::ByteArray { .. }
            | AirTree::CurvePoint { .. }
            | AirTree::Bool { .. }
            | AirTree::Void
            | AirTree::Var { .. }
            | AirTree::ErrorTerm { .. } => {}
            AirTree::Let {
                name: _,
                value: _,
                then,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);
            }
            AirTree::SoftCastLet {
                name: _,
                tipo: _,
                value: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FourthField, with);
            }
            AirTree::AssertBool {
                is_true: _,
                value: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::ThirdField, with);
            }
            AirTree::FieldsExpose {
                indices: _,
                record: _,
                is_expect: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FourthField, with);
            }
            AirTree::ListAccessor {
                tipo: _,
                names: _,
                tail: _,
                list: _,
                expect_level: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SixthField, with);
            }
            AirTree::TupleAccessor {
                names: _,
                tipo: _,
                tuple: _,
                is_expect: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::FifthField, with);
            }
            AirTree::PairAccessor {
                fst: _,
                snd: _,
                tipo: _,
                is_expect: _,
                pair: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SixthField, with);
            }
            AirTree::FieldsEmpty {
                constr: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);
            }
            AirTree::ListEmpty {
                list: _,
                then,
                otherwise: _,
            } => {
                then.do_traverse_tree_with(tree_path, current_depth + 1, Fields::SecondField, with);
            }
        }

        with(self, tree_path);

        tree_path.pop();
    }

    /// Used in function hoisting to find the exact node to hoist over
    pub fn find_air_tree_node<'a>(&'a mut self, tree_path: &TreePath) -> &'a mut AirTree {
        let mut path_iter = tree_path.path.iter();
        path_iter.next();
        self.do_find_air_tree_node(&mut path_iter)
    }

    fn do_find_air_tree_node<'a>(
        &'a mut self,
        tree_path_iter: &mut Iter<(usize, Fields)>,
    ) -> &'a mut AirTree {
        // For finding the air node we skip over the define func ops since those are added later on.
        if let AirTree::DefineFunc { then, .. } | AirTree::DefineCyclicFuncs { then, .. } = self {
            then.as_mut().do_find_air_tree_node(tree_path_iter)
        } else if let Some((_depth, field)) = tree_path_iter.next() {
            match self {
                AirTree::Let {
                    name: _,
                    value,
                    then,
                } => match field {
                    Fields::SecondField => value.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::SoftCastLet {
                    name: _,
                    tipo: _,
                    value,
                    then,
                    otherwise,
                } => match field {
                    Fields::ThirdField => value.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FourthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FifthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::AssertBool {
                    is_true: _,
                    value,
                    then,
                    otherwise,
                } => match field {
                    Fields::SecondField => value.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FourthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::FieldsExpose {
                    indices: _,
                    record,
                    is_expect: _,
                    then,
                    otherwise,
                } => match field {
                    Fields::SecondField => record.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FourthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FifthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::ListAccessor {
                    tipo: _,
                    names: _,
                    tail: _,
                    list,
                    expect_level: _,
                    then,
                    otherwise,
                } => match field {
                    Fields::FourthField => list.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SixthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SeventhField => {
                        otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                    }
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::TupleAccessor {
                    names: _,
                    tipo: _,
                    tuple,
                    is_expect: _,
                    then,
                    otherwise,
                } => match field {
                    Fields::ThirdField => tuple.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FifthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SixthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::PairAccessor {
                    fst: _,
                    snd: _,
                    tipo: _,
                    is_expect: _,
                    pair,
                    then,
                    otherwise,
                } => match field {
                    Fields::FifthField => pair.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SixthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SeventhField => {
                        otherwise.as_mut().do_find_air_tree_node(tree_path_iter)
                    }
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::NoOp { then } => match field {
                    Fields::FirstField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::DefineFunc { .. } | AirTree::DefineCyclicFuncs { .. } => unreachable!(),
                AirTree::FieldsEmpty {
                    constr,
                    then,
                    otherwise,
                } => match field {
                    Fields::FirstField => constr.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SecondField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::ListEmpty {
                    list,
                    then,
                    otherwise,
                } => match field {
                    Fields::FirstField => list.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::SecondField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::List { items, .. }
                | AirTree::Tuple { items, .. }
                | AirTree::Builtin { args: items, .. }
                | AirTree::Constr { args: items, .. } => match field {
                    Fields::ArgsField(index) => items
                        .get_mut(*index)
                        .expect("Tree Path index outside tree children nodes")
                        .do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::ExtractField { tipo: _, arg } => match field {
                    Fields::SecondField => arg.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::Pair { tipo: _, fst, snd } => match field {
                    Fields::SecondField => fst.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => snd.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::Call {
                    tipo: _,
                    func,
                    args,
                } => match field {
                    Fields::SecondField => func.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ArgsField(index) => args
                        .get_mut(*index)
                        .expect("Tree Path index outside tree children nodes")
                        .do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::Fn {
                    params: _,
                    func_body,
                    allow_inline: _,
                } => match field {
                    Fields::SecondField => func_body.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::BinOp {
                    name: _,
                    tipo: _,
                    left,
                    right,
                    argument_tipo: _,
                } => match field {
                    Fields::ThirdField => left.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FourthField => right.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::UnOp { op: _, arg } => match field {
                    Fields::SecondField => arg.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::CastFromData {
                    tipo: _,
                    value,
                    full_cast: _,
                } => match field {
                    Fields::SecondField => value.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::CastToData { tipo: _, value } => match field {
                    Fields::SecondField => value.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::When {
                    tipo: _,
                    subject_name: _,
                    subject,
                    subject_tipo: _,
                    clauses,
                } => match field {
                    Fields::ThirdField => subject.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FifthField => clauses.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::Clause {
                    subject_tipo: _,
                    subject_name: _,
                    pattern,
                    then,
                    otherwise,
                } => match field {
                    Fields::ThirdField => pattern.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FourthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FifthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::ListClause {
                    subject_tipo: _,
                    tail_name: _,
                    next_tail_name: _,
                    then,
                    otherwise,
                } => match field {
                    Fields::FourthField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FifthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::If {
                    tipo: _,
                    condition: pattern,
                    then,
                    otherwise,
                } => match field {
                    Fields::SecondField => pattern.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::FourthField => otherwise.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::RecordUpdate {
                    highest_index: _,
                    indices: _,
                    tipo: _,
                    record,
                    args,
                } => match field {
                    Fields::FourthField => record.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ArgsField(index) => args
                        .get_mut(*index)
                        .expect("Tree Path index outside tree children nodes")
                        .do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::Trace { tipo: _, msg, then } => match field {
                    Fields::SecondField => msg.as_mut().do_find_air_tree_node(tree_path_iter),
                    Fields::ThirdField => then.as_mut().do_find_air_tree_node(tree_path_iter),
                    _ => panic!("Tree Path index outside tree children nodes"),
                },
                AirTree::Int { .. }
                | AirTree::String { .. }
                | AirTree::ByteArray { .. }
                | AirTree::CurvePoint { .. }
                | AirTree::Bool { .. }
                | AirTree::Void
                | AirTree::Var { .. }
                | AirTree::ErrorTerm { .. } => {
                    panic!("A tree node with no children was encountered with a longer tree path.")
                }
            }
        } else {
            self
        }
    }
}
