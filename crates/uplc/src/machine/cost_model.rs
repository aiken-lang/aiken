use std::collections::HashMap;

use pallas_primitives::babbage::Language;

use crate::builtins::DefaultFunction;

use super::Value;

macro_rules! hashmap {
    // map-like
    ($($k:expr => $v:expr),* $(,)?) => {{
        core::convert::From::from([$(($k, $v),)*])
    }};
    // set-like
    ($($v:expr),* $(,)?) => {{
        core::convert::From::from([$($v,)*])
    }};
}

/// Can be negative
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ExBudget {
    pub mem: i64,
    pub cpu: i64,
}

impl ExBudget {
    pub fn occurences(&mut self, n: i64) {
        self.mem *= n;
        self.cpu *= n;
    }

    pub fn v1() -> Self {
        ExBudget {
            mem: 14000000,
            cpu: 10000000000,
        }
    }
}

impl Default for ExBudget {
    fn default() -> Self {
        ExBudget {
            mem: 14000000,
            cpu: 10000000000,
        }
    }
}

impl std::ops::Sub for ExBudget {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        ExBudget {
            mem: self.mem - rhs.mem,
            cpu: self.cpu - rhs.cpu,
        }
    }
}

#[derive(Default)]
pub struct CostModel {
    pub machine_costs: MachineCosts,
    pub builtin_costs: BuiltinCosts,
}

impl CostModel {
    pub fn v1() -> Self {
        Self {
            machine_costs: MachineCosts::v1(),
            builtin_costs: BuiltinCosts::v1(),
        }
    }
}

/// There's no entry for Error since we'll be exiting anyway; also, what would
/// happen if calling 'Error' caused the budget to be exceeded?
pub struct MachineCosts {
    startup: ExBudget,
    var: ExBudget,
    constant: ExBudget,
    lambda: ExBudget,
    delay: ExBudget,
    force: ExBudget,
    apply: ExBudget,
    /// Just the cost of evaluating a Builtin node, not the builtin itself.
    builtin: ExBudget,
}

impl MachineCosts {
    /// Get the cost of a step
    pub fn get(&self, step: StepKind) -> ExBudget {
        match step {
            StepKind::Constant => self.constant,
            StepKind::Var => self.var,
            StepKind::Lambda => self.lambda,
            StepKind::Apply => self.apply,
            StepKind::Delay => self.delay,
            StepKind::Force => self.force,
            StepKind::Builtin => self.builtin,
            StepKind::StartUp => self.startup,
        }
    }

    pub fn v1() -> Self {
        Self {
            startup: ExBudget { mem: 100, cpu: 100 },
            var: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            constant: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            lambda: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            delay: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            force: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            apply: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            builtin: ExBudget {
                mem: 100,
                cpu: 23000,
            },
        }
    }
}

impl Default for MachineCosts {
    /// Default is V2
    fn default() -> Self {
        Self {
            startup: ExBudget { mem: 100, cpu: 100 },
            var: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            constant: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            lambda: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            delay: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            force: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            apply: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            builtin: ExBudget {
                mem: 100,
                cpu: 23000,
            },
        }
    }
}

pub struct BuiltinCosts {
    pub add_integer: CostingFun<TwoArguments>,
    pub subtract_integer: CostingFun<TwoArguments>,
    pub multiply_integer: CostingFun<TwoArguments>,
    pub divide_integer: CostingFun<TwoArguments>,
    pub quotient_integer: CostingFun<TwoArguments>,
    pub remainder_integer: CostingFun<TwoArguments>,
    pub mod_integer: CostingFun<TwoArguments>,
    pub equals_integer: CostingFun<TwoArguments>,
    pub less_than_integer: CostingFun<TwoArguments>,
    pub less_than_equals_integer: CostingFun<TwoArguments>,
    // Bytestrings
    pub append_byte_string: CostingFun<TwoArguments>,
    pub cons_byte_string: CostingFun<TwoArguments>,
    pub slice_byte_string: CostingFun<ThreeArguments>,
    pub length_of_byte_string: CostingFun<OneArgument>,
    pub index_byte_string: CostingFun<TwoArguments>,
    pub equals_byte_string: CostingFun<TwoArguments>,
    pub less_than_byte_string: CostingFun<TwoArguments>,
    pub less_than_equals_byte_string: CostingFun<TwoArguments>,
    // Cryptography and hashes
    pub sha2_256: CostingFun<OneArgument>,
    pub sha3_256: CostingFun<OneArgument>,
    pub blake2b_256: CostingFun<OneArgument>,
    pub verify_ed25519_signature: CostingFun<ThreeArguments>,
    pub verify_ecdsa_secp256k1_signature: CostingFun<ThreeArguments>,
    pub verify_schnorr_secp256k1_signature: CostingFun<ThreeArguments>,
    // Strings
    pub append_string: CostingFun<TwoArguments>,
    pub equals_string: CostingFun<TwoArguments>,
    pub encode_utf8: CostingFun<OneArgument>,
    pub decode_utf8: CostingFun<OneArgument>,
    // Bool
    pub if_then_else: CostingFun<ThreeArguments>,
    // Unit
    pub choose_unit: CostingFun<TwoArguments>,
    // Tracing
    pub trace: CostingFun<TwoArguments>,
    // Pairs
    pub fst_pair: CostingFun<OneArgument>,
    pub snd_pair: CostingFun<OneArgument>,
    // Lists
    pub choose_list: CostingFun<ThreeArguments>,
    pub mk_cons: CostingFun<TwoArguments>,
    pub head_list: CostingFun<OneArgument>,
    pub tail_list: CostingFun<OneArgument>,
    pub null_list: CostingFun<OneArgument>,
    // Data
    pub choose_data: CostingFun<SixArguments>,
    pub constr_data: CostingFun<TwoArguments>,
    pub map_data: CostingFun<OneArgument>,
    pub list_data: CostingFun<OneArgument>,
    pub i_data: CostingFun<OneArgument>,
    pub b_data: CostingFun<OneArgument>,
    pub un_constr_data: CostingFun<OneArgument>,
    pub un_map_data: CostingFun<OneArgument>,
    pub un_list_data: CostingFun<OneArgument>,
    pub un_i_data: CostingFun<OneArgument>,
    pub un_b_data: CostingFun<OneArgument>,
    pub equals_data: CostingFun<TwoArguments>,
    // Misc constructors
    pub mk_pair_data: CostingFun<TwoArguments>,
    pub mk_nil_data: CostingFun<OneArgument>,
    pub mk_nil_pair_data: CostingFun<OneArgument>,
    pub serialise_data: CostingFun<OneArgument>,
}

impl BuiltinCosts {
    pub fn v1() -> Self {
        Self {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),

                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 69522,
                    slope: 11687,
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            remainder_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            mod_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208512,
                    slope: 421,
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208896,
                    slope: 511,
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 204924,
                    slope: 473,
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 571,
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 221973,
                    slope: 511,
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 265318,
                    slope: 0,
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(1000),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(57667),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 245000,
                    intercept: 216773,
                    slope: 62,
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 806990,
                    slope: 30482,
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1927926,
                    slope: 82523,
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 117366,
                    slope: 10475,
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 57996947,
                    slope: 18975,
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(20000000000),
                cpu: ThreeArguments::ConstantCost(20000000000),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(20000000000),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 20000000000,
                    slope: 0,
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 4,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 24177,
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 187000,
                    intercept: 1000,
                    slope: 52998,
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1000,
                    slope: 28662,
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 497525,
                    slope: 14068,
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(1),
                cpu: ThreeArguments::ConstantCost(80556),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(46417),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(212342),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(80436),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(85931),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(32),
                cpu: ThreeArguments::ConstantCost(175354),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(65493),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(43249),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(41182),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(60091),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(32),
                cpu: SixArguments::ConstantCost(19537),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(89141),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(64832),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(52467),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(1000),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(1000),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(32696),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(38314),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(32247),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(43357),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(31220),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 1060367,
                    slope: 12586,
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(76511),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(22558),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(16563),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 30000000000,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1159724,
                    slope: 392670,
                }),
            },
        }
    }
}

impl Default for BuiltinCosts {
    /// Default is V2
    fn default() -> Self {
        Self {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),

                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 69522,
                    slope: 11687,
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            remainder_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            mod_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208512,
                    slope: 421,
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208896,
                    slope: 511,
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 204924,
                    slope: 473,
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 571,
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 221973,
                    slope: 511,
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 265318,
                    slope: 0,
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(1000),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(57667),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 245000,
                    intercept: 216773,
                    slope: 62,
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 806990,
                    slope: 30482,
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1927926,
                    slope: 82523,
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 117366,
                    slope: 10475,
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 57996947,
                    slope: 18975,
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(20000000000),
                cpu: ThreeArguments::ConstantCost(20000000000),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(20000000000),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 20000000000,
                    slope: 0,
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 4,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 24177,
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 187000,
                    intercept: 1000,
                    slope: 52998,
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1000,
                    slope: 28662,
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 497525,
                    slope: 14068,
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(1),
                cpu: ThreeArguments::ConstantCost(80556),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(46417),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(212342),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(80436),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(85931),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(32),
                cpu: ThreeArguments::ConstantCost(175354),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(65493),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(43249),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(41182),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(60091),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(32),
                cpu: SixArguments::ConstantCost(19537),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(89141),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(64832),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(52467),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(1000),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(1000),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(32696),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(38314),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(32247),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(43357),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(31220),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 1060367,
                    slope: 12586,
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(76511),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(22558),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(16563),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 0,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1159724,
                    slope: 392670,
                }),
            },
        }
    }
}

impl BuiltinCosts {
    pub fn to_ex_budget_v2(&self, fun: DefaultFunction, args: &[Value]) -> ExBudget {
        match fun {
            DefaultFunction::AddInteger => ExBudget {
                mem: self
                    .add_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .add_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SubtractInteger => ExBudget {
                mem: self
                    .subtract_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .subtract_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MultiplyInteger => ExBudget {
                mem: self
                    .multiply_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .multiply_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::DivideInteger => ExBudget {
                mem: self
                    .divide_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .divide_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::QuotientInteger => ExBudget {
                mem: self
                    .quotient_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .quotient_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::RemainderInteger => ExBudget {
                mem: self
                    .remainder_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .remainder_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::ModInteger => ExBudget {
                mem: self
                    .mod_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mod_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsInteger => ExBudget {
                mem: self
                    .equals_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanInteger => ExBudget {
                mem: self
                    .less_than_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanEqualsInteger => ExBudget {
                mem: self
                    .less_than_equals_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_equals_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::AppendByteString => ExBudget {
                mem: self
                    .append_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .append_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::ConsByteString => ExBudget {
                mem: self
                    .cons_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .cons_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SliceByteString => ExBudget {
                mem: self.slice_byte_string.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.slice_byte_string.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::LengthOfByteString => ExBudget {
                mem: self.length_of_byte_string.mem.cost(args[0].to_ex_mem()),
                cpu: self.length_of_byte_string.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IndexByteString => ExBudget {
                mem: self
                    .index_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .index_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsByteString => ExBudget {
                mem: self
                    .equals_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanByteString => ExBudget {
                mem: self
                    .less_than_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanEqualsByteString => ExBudget {
                mem: self
                    .less_than_equals_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_equals_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Sha2_256 => ExBudget {
                mem: self.sha2_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.sha2_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Sha3_256 => ExBudget {
                mem: self.sha3_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.sha3_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Blake2b_256 => ExBudget {
                mem: self.blake2b_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.blake2b_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::VerifyEd25519Signature => ExBudget {
                mem: self.verify_ed25519_signature.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.verify_ed25519_signature.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
            DefaultFunction::AppendString => ExBudget {
                mem: self
                    .append_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .append_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsString => ExBudget {
                mem: self
                    .equals_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EncodeUtf8 => ExBudget {
                mem: self.encode_utf8.mem.cost(args[0].to_ex_mem()),
                cpu: self.encode_utf8.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::DecodeUtf8 => ExBudget {
                mem: self.decode_utf8.mem.cost(args[0].to_ex_mem()),
                cpu: self.decode_utf8.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IfThenElse => ExBudget {
                mem: self.if_then_else.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.if_then_else.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::ChooseUnit => ExBudget {
                mem: self
                    .choose_unit
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .choose_unit
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Trace => ExBudget {
                mem: self
                    .trace
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .trace
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::FstPair => ExBudget {
                mem: self.fst_pair.mem.cost(args[0].to_ex_mem()),
                cpu: self.fst_pair.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::SndPair => ExBudget {
                mem: self.snd_pair.mem.cost(args[0].to_ex_mem()),
                cpu: self.snd_pair.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ChooseList => ExBudget {
                mem: self.choose_list.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.choose_list.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::MkCons => ExBudget {
                mem: self
                    .mk_cons
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mk_cons
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::HeadList => ExBudget {
                mem: self.head_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.head_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::TailList => ExBudget {
                mem: self.tail_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.tail_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::NullList => ExBudget {
                mem: self.null_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.null_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ChooseData => ExBudget {
                mem: self.choose_data.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                    args[3].to_ex_mem(),
                    args[4].to_ex_mem(),
                    args[5].to_ex_mem(),
                ),
                cpu: self.choose_data.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                    args[3].to_ex_mem(),
                    args[4].to_ex_mem(),
                    args[5].to_ex_mem(),
                ),
            },
            DefaultFunction::ConstrData => ExBudget {
                mem: self
                    .constr_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .constr_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MapData => ExBudget {
                mem: self.map_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.map_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ListData => ExBudget {
                mem: self.list_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.list_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IData => ExBudget {
                mem: self.i_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.i_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::BData => ExBudget {
                mem: self.b_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.b_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnConstrData => ExBudget {
                mem: self.un_constr_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_constr_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnMapData => ExBudget {
                mem: self.un_map_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_map_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnListData => ExBudget {
                mem: self.un_list_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_list_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnIData => ExBudget {
                mem: self.un_i_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_i_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnBData => ExBudget {
                mem: self.un_b_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_b_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::EqualsData => ExBudget {
                mem: self
                    .equals_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SerialiseData => ExBudget {
                mem: self.serialise_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.serialise_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::MkPairData => ExBudget {
                mem: self
                    .mk_pair_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mk_pair_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MkNilData => ExBudget {
                mem: self.mk_nil_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.mk_nil_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::MkNilPairData => ExBudget {
                mem: self.mk_nil_pair_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.mk_nil_pair_data.cpu.cost(args[0].to_ex_mem()),
            },
        }
    }

    pub fn to_ex_budget_v1(&self, fun: DefaultFunction, args: &[Value]) -> ExBudget {
        match fun {
            DefaultFunction::AddInteger => ExBudget {
                mem: self
                    .add_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .add_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SubtractInteger => ExBudget {
                mem: self
                    .subtract_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .subtract_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MultiplyInteger => ExBudget {
                mem: self
                    .multiply_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .multiply_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::DivideInteger => ExBudget {
                mem: self
                    .divide_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .divide_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::QuotientInteger => ExBudget {
                mem: self
                    .quotient_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .quotient_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::RemainderInteger => ExBudget {
                mem: self
                    .remainder_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .remainder_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::ModInteger => ExBudget {
                mem: self
                    .mod_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mod_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsInteger => ExBudget {
                mem: self
                    .equals_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanInteger => ExBudget {
                mem: self
                    .less_than_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanEqualsInteger => ExBudget {
                mem: self
                    .less_than_equals_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_equals_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::AppendByteString => ExBudget {
                mem: self
                    .append_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .append_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::ConsByteString => ExBudget {
                mem: self
                    .cons_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .cons_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SliceByteString => ExBudget {
                mem: self.slice_byte_string.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.slice_byte_string.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::LengthOfByteString => ExBudget {
                mem: self.length_of_byte_string.mem.cost(args[0].to_ex_mem()),
                cpu: self.length_of_byte_string.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IndexByteString => ExBudget {
                mem: self
                    .index_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .index_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsByteString => ExBudget {
                mem: self
                    .equals_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanByteString => ExBudget {
                mem: self
                    .less_than_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanEqualsByteString => ExBudget {
                mem: self
                    .less_than_equals_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_equals_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Sha2_256 => ExBudget {
                mem: self.sha2_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.sha2_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Sha3_256 => ExBudget {
                mem: self.sha3_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.sha3_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Blake2b_256 => ExBudget {
                mem: self.blake2b_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.blake2b_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::VerifyEd25519Signature => ExBudget {
                mem: self.verify_ed25519_signature.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.verify_ed25519_signature.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::VerifyEcdsaSecp256k1Signature => unreachable!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => unreachable!(),
            DefaultFunction::AppendString => ExBudget {
                mem: self
                    .append_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .append_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsString => ExBudget {
                mem: self
                    .equals_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EncodeUtf8 => ExBudget {
                mem: self.encode_utf8.mem.cost(args[0].to_ex_mem()),
                cpu: self.encode_utf8.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::DecodeUtf8 => ExBudget {
                mem: self.decode_utf8.mem.cost(args[0].to_ex_mem()),
                cpu: self.decode_utf8.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IfThenElse => ExBudget {
                mem: self.if_then_else.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.if_then_else.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::ChooseUnit => ExBudget {
                mem: self
                    .choose_unit
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .choose_unit
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Trace => ExBudget {
                mem: self
                    .trace
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .trace
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::FstPair => ExBudget {
                mem: self.fst_pair.mem.cost(args[0].to_ex_mem()),
                cpu: self.fst_pair.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::SndPair => ExBudget {
                mem: self.snd_pair.mem.cost(args[0].to_ex_mem()),
                cpu: self.snd_pair.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ChooseList => ExBudget {
                mem: self.choose_list.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.choose_list.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::MkCons => ExBudget {
                mem: self
                    .mk_cons
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mk_cons
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::HeadList => ExBudget {
                mem: self.head_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.head_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::TailList => ExBudget {
                mem: self.tail_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.tail_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::NullList => ExBudget {
                mem: self.null_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.null_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ChooseData => ExBudget {
                mem: self.choose_data.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                    args[3].to_ex_mem(),
                    args[4].to_ex_mem(),
                    args[5].to_ex_mem(),
                ),
                cpu: self.choose_data.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                    args[3].to_ex_mem(),
                    args[4].to_ex_mem(),
                    args[5].to_ex_mem(),
                ),
            },
            DefaultFunction::ConstrData => ExBudget {
                mem: self
                    .constr_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .constr_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MapData => ExBudget {
                mem: self.map_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.map_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ListData => ExBudget {
                mem: self.list_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.list_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IData => ExBudget {
                mem: self.i_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.i_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::BData => ExBudget {
                mem: self.b_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.b_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnConstrData => ExBudget {
                mem: self.un_constr_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_constr_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnMapData => ExBudget {
                mem: self.un_map_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_map_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnListData => ExBudget {
                mem: self.un_list_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_list_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnIData => ExBudget {
                mem: self.un_i_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_i_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnBData => ExBudget {
                mem: self.un_b_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_b_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::EqualsData => ExBudget {
                mem: self
                    .equals_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SerialiseData => unreachable!(),
            DefaultFunction::MkPairData => ExBudget {
                mem: self
                    .mk_pair_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mk_pair_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MkNilData => ExBudget {
                mem: self.mk_nil_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.mk_nil_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::MkNilPairData => ExBudget {
                mem: self.mk_nil_pair_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.mk_nil_pair_data.cpu.cost(args[0].to_ex_mem()),
            },
        }
    }
}

pub fn initialize_cost_model(version: &Language, costs: &[i64]) -> CostModel {
    let cost_map: HashMap<&str, i64> = match version {
        Language::PlutusV1 => {
            hashmap! {
                "add_integer-cpu-arguments-intercept" => costs[0],
                "add_integer-cpu-arguments-slope" => costs[1],
                "add_integer-mem-arguments-intercept" => costs[2],
                "add_integer-mem-arguments-slope" => costs[3],
                "append_byte_string-cpu-arguments-intercept" => costs[4],
                "append_byte_string-cpu-arguments-slope" => costs[5],
                "append_byte_string-mem-arguments-intercept" => costs[6],
                "append_byte_string-mem-arguments-slope" => costs[7],
                "append_string-cpu-arguments-intercept" => costs[8],
                "append_string-cpu-arguments-slope" => costs[9],
                "append_string-mem-arguments-intercept" => costs[10],
                "append_string-mem-arguments-slope" => costs[11],
                "b_data-cpu-arguments" => costs[12],
                "b_data-mem-arguments" => costs[13],
                "blake2b_256-cpu-arguments-intercept" => costs[14],
                "blake2b_256-cpu-arguments-slope" => costs[15],
                "blake2b_256-mem-arguments" => costs[16],
                "cek_apply_cost-exBudgetCPU" => costs[17],
                "cek_apply_cost-exBudgetmem" => costs[18],
                "cek_builtin_cost-exBudgetCPU" => costs[19],
                "cek_builtin_cost-exBudgetmem" => costs[20],
                "cek_const_cost-exBudgetCPU" => costs[21],
                "cek_const_cost-exBudgetmem" => costs[22],
                "cek_delay_cost-exBudgetCPU" => costs[23],
                "cek_delay_cost-exBudgetmem" => costs[24],
                "cek_force_cost-exBudgetCPU" => costs[25],
                "cek_force_cost-exBudgetmem" => costs[26],
                "cek_lam_cost-exBudgetCPU" => costs[27],
                "cek_lam_cost-exBudgetmem" => costs[28],
                "cek_startup_cost-exBudgetCPU" => costs[29],
                "cek_startup_cost-exBudgetmem" => costs[30],
                "cek_var_cost-exBudgetCPU" => costs[31],
                "cek_var_cost-exBudgetmem" => costs[32],
                "choose_data-cpu-arguments" => costs[33],
                "choose_data-mem-arguments" => costs[34],
                "choose_list-cpu-arguments" => costs[35],
                "choose_list-mem-arguments" => costs[36],
                "choose_unit-cpu-arguments" => costs[37],
                "choose_unit-mem-arguments" => costs[38],
                "cons_byte_string-cpu-arguments-intercept" => costs[39],
                "cons_byte_string-cpu-arguments-slope" => costs[40],
                "cons_byte_string-mem-arguments-intercept" => costs[41],
                "cons_byte_string-mem-arguments-slope" => costs[42],
                "constr_data-cpu-arguments" => costs[43],
                "constr_data-mem-arguments" => costs[44],
                "decode_utf8-cpu-arguments-intercept" => costs[45],
                "decode_utf8-cpu-arguments-slope" => costs[46],
                "decode_utf8-mem-arguments-intercept" => costs[47],
                "decode_utf8-mem-arguments-slope" => costs[48],
                "divide_integer-cpu-arguments-constant" => costs[49],
                "divide_integer-cpu-arguments-model-arguments-intercept" => costs[50],
                "divide_integer-cpu-arguments-model-arguments-slope" => costs[51],
                "divide_integer-mem-arguments-intercept" => costs[52],
                "divide_integer-mem-arguments-minimum" => costs[53],
                "divide_integer-mem-arguments-slope" => costs[54],
                "encode_utf8-cpu-arguments-intercept" => costs[55],
                "encode_utf8-cpu-arguments-slope" => costs[56],
                "encode_utf8-mem-arguments-intercept" => costs[57],
                "encode_utf8-mem-arguments-slope" => costs[58],
                "equals_byte_string-cpu-arguments-constant" => costs[59],
                "equals_byte_string-cpu-arguments-intercept" => costs[60],
                "equals_byte_string-cpu-arguments-slope" => costs[61],
                "equals_byte_string-mem-arguments" => costs[62],
                "equals_data-cpu-arguments-intercept" => costs[63],
                "equals_data-cpu-arguments-slope" => costs[64],
                "equals_data-mem-arguments" => costs[65],
                "equals_integer-cpu-arguments-intercept" => costs[66],
                "equals_integer-cpu-arguments-slope" => costs[67],
                "equals_integer-mem-arguments" => costs[68],
                "equals_string-cpu-arguments-constant" => costs[69],
                "equals_string-cpu-arguments-intercept" => costs[70],
                "equals_string-cpu-arguments-slope" => costs[71],
                "equals_string-mem-arguments" => costs[72],
                "fst_pair-cpu-arguments" => costs[73],
                "fst_pair-mem-arguments" => costs[74],
                "head_list-cpu-arguments" => costs[75],
                "head_list-mem-arguments" => costs[76],
                "i_data-cpu-arguments" => costs[77],
                "i_data-mem-arguments" => costs[78],
                "if_then_else-cpu-arguments" => costs[79],
                "if_then_else-mem-arguments" => costs[80],
                "index_byte_string-cpu-arguments" => costs[81],
                "index_byte_string-mem-arguments" => costs[82],
                "length_of_byte_string-cpu-arguments" => costs[83],
                "length_of_byte_string-mem-arguments" => costs[84],
                "less_than_byte_string-cpu-arguments-intercept" => costs[85],
                "less_than_byte_string-cpu-arguments-slope" => costs[86],
                "less_than_byte_string-mem-arguments" => costs[87],
                "less_than_equals_byte_string-cpu-arguments-intercept" => costs[88],
                "less_than_equals_byte_string-cpu-arguments-slope" => costs[89],
                "less_than_equals_byte_string-mem-arguments" => costs[90],
                "less_than_equals_integer-cpu-arguments-intercept" => costs[91],
                "less_than_equals_integer-cpu-arguments-slope" => costs[92],
                "less_than_equals_integer-mem-arguments" => costs[93],
                "less_than_integer-cpu-arguments-intercept" => costs[94],
                "less_than_integer-cpu-arguments-slope" => costs[95],
                "less_than_integer-mem-arguments" => costs[96],
                "list_data-cpu-arguments" => costs[97],
                "list_data-mem-arguments" => costs[98],
                "map_data-cpu-arguments" => costs[99],
                "map_data-mem-arguments" => costs[100],
                "mk_cons-cpu-arguments" => costs[101],
                "mk_cons-mem-arguments" => costs[102],
                "mk_nil_data-cpu-arguments" => costs[103],
                "mk_nil_data-mem-arguments" => costs[104],
                "mk_nil_pair_data-cpu-arguments" => costs[105],
                "mk_nil_pair_data-mem-arguments" => costs[106],
                "mk_pair_data-cpu-arguments" => costs[107],
                "mk_pair_data-mem-arguments" => costs[108],
                "mod_integer-cpu-arguments-constant" => costs[109],
                "mod_integer-cpu-arguments-model-arguments-intercept" => costs[110],
                "mod_integer-cpu-arguments-model-arguments-slope" => costs[111],
                "mod_integer-mem-arguments-intercept" => costs[112],
                "mod_integer-mem-arguments-minimum" => costs[113],
                "mod_integer-mem-arguments-slope" => costs[114],
                "multiply_integer-cpu-arguments-intercept" => costs[115],
                "multiply_integer-cpu-arguments-slope" => costs[116],
                "multiply_integer-mem-arguments-intercept" => costs[117],
                "multiply_integer-mem-arguments-slope" => costs[118],
                "null_list-cpu-arguments" => costs[119],
                "null_list-mem-arguments" => costs[120],
                "quotient_integer-cpu-arguments-constant" => costs[121],
                "quotient_integer-cpu-arguments-model-arguments-intercept" => costs[122],
                "quotient_integer-cpu-arguments-model-arguments-slope" => costs[123],
                "quotient_integer-mem-arguments-intercept" => costs[124],
                "quotient_integer-mem-arguments-minimum" => costs[125],
                "quotient_integer-mem-arguments-slope" => costs[126],
                "remainder_integer-cpu-arguments-constant" => costs[127],
                "remainder_integer-cpu-arguments-model-arguments-intercept" => costs[128],
                "remainder_integer-cpu-arguments-model-arguments-slope" => costs[129],
                "remainder_integer-mem-arguments-intercept" => costs[130],
                "remainder_integer-mem-arguments-minimum" => costs[131],
                "remainder_integer-mem-arguments-slope" => costs[132],
                "sha2_256-cpu-arguments-intercept" => costs[133],
                "sha2_256-cpu-arguments-slope" => costs[134],
                "sha2_256-mem-arguments" => costs[135],
                "sha3_256-cpu-arguments-intercept" => costs[136],
                "sha3_256-cpu-arguments-slope" => costs[137],
                "sha3_256-mem-arguments" => costs[138],
                "slice_byte_string-cpu-arguments-intercept" => costs[139],
                "slice_byte_string-cpu-arguments-slope" => costs[140],
                "slice_byte_string-mem-arguments-intercept" => costs[141],
                "slice_byte_string-mem-arguments-slope" => costs[142],
                "snd_pair-cpu-arguments" => costs[143],
                "snd_pair-mem-arguments" => costs[144],
                "subtract_integer-cpu-arguments-intercept" => costs[145],
                "subtract_integer-cpu-arguments-slope" => costs[146],
                "subtract_integer-mem-arguments-intercept" => costs[147],
                "subtract_integer-mem-arguments-slope" => costs[148],
                "tail_list-cpu-arguments" => costs[149],
                "tail_list-mem-arguments" => costs[150],
                "trace-cpu-arguments" => costs[151],
                "trace-mem-arguments" => costs[152],
                "un_b_data-cpu-arguments" => costs[153],
                "un_b_data-mem-arguments" => costs[154],
                "un_constr_data-cpu-arguments" => costs[155],
                "un_constr_data-mem-arguments" => costs[156],
                "un_i_data-cpu-arguments" => costs[157],
                "un_i_data-mem-arguments" => costs[158],
                "un_list_data-cpu-arguments" => costs[159],
                "un_list_data-mem-arguments" => costs[160],
                "un_map_data-cpu-arguments" => costs[161],
                "un_map_data-mem-arguments" => costs[162],
                "verify_ed25519_signature-cpu-arguments-intercept" => costs[163],
                "verify_ed25519_signature-cpu-arguments-slope" => costs[164],
                "verify_ed25519_signature-mem-arguments" => costs[165]
            }
        }
        Language::PlutusV2 => {
            hashmap! {
                "add_integer-cpu-arguments-intercept"=> costs[0],
                "add_integer-cpu-arguments-slope"=> costs[1],
                "add_integer-mem-arguments-intercept"=> costs[2],
                "add_integer-mem-arguments-slope"=> costs[3],
                "append_byte_string-cpu-arguments-intercept"=> costs[4],
                "append_byte_string-cpu-arguments-slope"=> costs[5],
                "append_byte_string-mem-arguments-intercept"=> costs[6],
                "append_byte_string-mem-arguments-slope"=> costs[7],
                "append_string-cpu-arguments-intercept"=> costs[8],
                "append_string-cpu-arguments-slope"=> costs[9],
                "append_string-mem-arguments-intercept"=> costs[10],
                "append_string-mem-arguments-slope"=> costs[11],
                "b_data-cpu-arguments"=> costs[12],
                "b_data-mem-arguments"=> costs[13],
                "blake2b_256-cpu-arguments-intercept"=> costs[14],
                "blake2b_256-cpu-arguments-slope"=> costs[15],
                "blake2b_256-mem-arguments"=> costs[16],
                "cek_apply_cost-exBudgetCPU"=> costs[17],
                "cek_apply_cost-exBudgetmem"=> costs[18],
                "cek_builtin_cost-exBudgetCPU"=> costs[19],
                "cek_builtin_cost-exBudgetmem"=> costs[20],
                "cek_const_cost-exBudgetCPU"=> costs[21],
                "cek_const_cost-exBudgetmem"=> costs[22],
                "cek_delay_cost-exBudgetCPU"=> costs[23],
                "cek_delay_cost-exBudgetmem"=> costs[24],
                "cek_force_cost-exBudgetCPU"=> costs[25],
                "cek_force_cost-exBudgetmem"=> costs[26],
                "cek_lam_cost-exBudgetCPU"=> costs[27],
                "cek_lam_cost-exBudgetmem"=> costs[28],
                "cek_startup_cost-exBudgetCPU"=> costs[29],
                "cek_startup_cost-exBudgetmem"=> costs[30],
                "cek_var_cost-exBudgetCPU"=> costs[31],
                "cek_var_cost-exBudgetmem"=> costs[32],
                "choose_data-cpu-arguments"=> costs[33],
                "choose_data-mem-arguments"=> costs[34],
                "choose_list-cpu-arguments"=> costs[35],
                "choose_list-mem-arguments"=> costs[36],
                "choose_unit-cpu-arguments"=> costs[37],
                "choose_unit-mem-arguments"=> costs[38],
                "cons_byte_string-cpu-arguments-intercept"=> costs[39],
                "cons_byte_string-cpu-arguments-slope"=> costs[40],
                "cons_byte_string-mem-arguments-intercept"=> costs[41],
                "cons_byte_string-mem-arguments-slope"=> costs[42],
                "constr_data-cpu-arguments"=> costs[43],
                "constr_data-mem-arguments"=> costs[44],
                "decode_utf8-cpu-arguments-intercept"=> costs[45],
                "decode_utf8-cpu-arguments-slope"=> costs[46],
                "decode_utf8-mem-arguments-intercept"=> costs[47],
                "decode_utf8-mem-arguments-slope"=> costs[48],
                "divide_integer-cpu-arguments-constant"=> costs[49],
                "divide_integer-cpu-arguments-model-arguments-intercept"=> costs[50],
                "divide_integer-cpu-arguments-model-arguments-slope"=> costs[51],
                "divide_integer-mem-arguments-intercept"=> costs[52],
                "divide_integer-mem-arguments-minimum"=> costs[53],
                "divide_integer-mem-arguments-slope"=> costs[54],
                "encode_utf8-cpu-arguments-intercept"=> costs[55],
                "encode_utf8-cpu-arguments-slope"=> costs[56],
                "encode_utf8-mem-arguments-intercept"=> costs[57],
                "encode_utf8-mem-arguments-slope"=> costs[58],
                "equals_byte_string-cpu-arguments-constant"=> costs[59],
                "equals_byte_string-cpu-arguments-intercept"=> costs[60],
                "equals_byte_string-cpu-arguments-slope"=> costs[61],
                "equals_byte_string-mem-arguments"=> costs[62],
                "equals_data-cpu-arguments-intercept"=> costs[63],
                "equals_data-cpu-arguments-slope"=> costs[64],
                "equals_data-mem-arguments"=> costs[65],
                "equals_integer-cpu-arguments-intercept"=> costs[66],
                "equals_integer-cpu-arguments-slope"=> costs[67],
                "equals_integer-mem-arguments"=> costs[68],
                "equals_string-cpu-arguments-constant"=> costs[69],
                "equals_string-cpu-arguments-intercept"=> costs[70],
                "equals_string-cpu-arguments-slope"=> costs[71],
                "equals_string-mem-arguments"=> costs[72],
                "fst_pair-cpu-arguments"=> costs[73],
                "fst_pair-mem-arguments"=> costs[74],
                "head_list-cpu-arguments"=> costs[75],
                "head_list-mem-arguments"=> costs[76],
                "i_data-cpu-arguments"=> costs[77],
                "i_data-mem-arguments"=> costs[78],
                "if_then_else-cpu-arguments"=> costs[79],
                "if_then_else-mem-arguments"=> costs[80],
                "index_byte_string-cpu-arguments"=> costs[81],
                "index_byte_string-mem-arguments"=> costs[82],
                "length_of_byte_string-cpu-arguments"=> costs[83],
                "length_of_byte_string-mem-arguments"=> costs[84],
                "less_than_byte_string-cpu-arguments-intercept"=> costs[85],
                "less_than_byte_string-cpu-arguments-slope"=> costs[86],
                "less_than_byte_string-mem-arguments"=> costs[87],
                "less_than_equals_byte_string-cpu-arguments-intercept"=> costs[88],
                "less_than_equals_byte_string-cpu-arguments-slope"=> costs[89],
                "less_than_equals_byte_string-mem-arguments"=> costs[90],
                "less_than_equals_integer-cpu-arguments-intercept"=> costs[91],
                "less_than_equals_integer-cpu-arguments-slope"=> costs[92],
                "less_than_equals_integer-mem-arguments"=> costs[93],
                "less_than_integer-cpu-arguments-intercept"=> costs[94],
                "less_than_integer-cpu-arguments-slope"=> costs[95],
                "less_than_integer-mem-arguments"=> costs[96],
                "list_data-cpu-arguments"=> costs[97],
                "list_data-mem-arguments"=> costs[98],
                "map_data-cpu-arguments"=> costs[99],
                "map_data-mem-arguments"=> costs[100],
                "mk_cons-cpu-arguments"=> costs[101],
                "mk_cons-mem-arguments"=> costs[102],
                "mk_nil_data-cpu-arguments"=> costs[103],
                "mk_nil_data-mem-arguments"=> costs[104],
                "mk_nil_pair_data-cpu-arguments"=> costs[105],
                "mk_nil_pair_data-mem-arguments"=> costs[106],
                "mk_pair_data-cpu-arguments"=> costs[107],
                "mk_pair_data-mem-arguments"=> costs[108],
                "mod_integer-cpu-arguments-constant"=> costs[109],
                "mod_integer-cpu-arguments-model-arguments-intercept"=> costs[110],
                "mod_integer-cpu-arguments-model-arguments-slope"=> costs[111],
                "mod_integer-mem-arguments-intercept"=> costs[112],
                "mod_integer-mem-arguments-minimum"=> costs[113],
                "mod_integer-mem-arguments-slope"=> costs[114],
                "multiply_integer-cpu-arguments-intercept"=> costs[115],
                "multiply_integer-cpu-arguments-slope"=> costs[116],
                "multiply_integer-mem-arguments-intercept"=> costs[117],
                "multiply_integer-mem-arguments-slope"=> costs[118],
                "null_list-cpu-arguments"=> costs[119],
                "null_list-mem-arguments"=> costs[120],
                "quotient_integer-cpu-arguments-constant"=> costs[121],
                "quotient_integer-cpu-arguments-model-arguments-intercept"=> costs[122],
                "quotient_integer-cpu-arguments-model-arguments-slope"=> costs[123],
                "quotient_integer-mem-arguments-intercept"=> costs[124],
                "quotient_integer-mem-arguments-minimum"=> costs[125],
                "quotient_integer-mem-arguments-slope"=> costs[126],
                "remainder_integer-cpu-arguments-constant"=> costs[127],
                "remainder_integer-cpu-arguments-model-arguments-intercept"=> costs[128],
                "remainder_integer-cpu-arguments-model-arguments-slope"=> costs[129],
                "remainder_integer-mem-arguments-intercept"=> costs[130],
                "remainder_integer-mem-arguments-minimum"=> costs[131],
                "remainder_integer-mem-arguments-slope"=> costs[132],
                "serialise_data-cpu-arguments-intercept"=> costs[133],
                "serialise_data-cpu-arguments-slope"=> costs[134],
                "serialise_data-mem-arguments-intercept"=> costs[135],
                "serialise_data-mem-arguments-slope"=> costs[136],
                "sha2_256-cpu-arguments-intercept"=> costs[137],
                "sha2_256-cpu-arguments-slope"=> costs[138],
                "sha2_256-mem-arguments"=> costs[139],
                "sha3_256-cpu-arguments-intercept"=> costs[140],
                "sha3_256-cpu-arguments-slope"=> costs[141],
                "sha3_256-mem-arguments"=> costs[142],
                "slice_byte_string-cpu-arguments-intercept"=> costs[143],
                "slice_byte_string-cpu-arguments-slope"=> costs[144],
                "slice_byte_string-mem-arguments-intercept"=> costs[145],
                "slice_byte_string-mem-arguments-slope"=> costs[146],
                "snd_pair-cpu-arguments"=> costs[147],
                "snd_pair-mem-arguments"=> costs[148],
                "subtract_integer-cpu-arguments-intercept"=> costs[149],
                "subtract_integer-cpu-arguments-slope"=> costs[150],
                "subtract_integer-mem-arguments-intercept"=> costs[151],
                "subtract_integer-mem-arguments-slope"=> costs[152],
                "tail_list-cpu-arguments"=> costs[153],
                "tail_list-mem-arguments"=> costs[154],
                "trace-cpu-arguments"=> costs[155],
                "trace-mem-arguments"=> costs[156],
                "un_b_data-cpu-arguments"=> costs[157],
                "un_b_data-mem-arguments"=> costs[158],
                "un_constr_data-cpu-arguments"=> costs[159],
                "un_constr_data-mem-arguments"=> costs[160],
                "un_i_data-cpu-arguments"=> costs[161],
                "un_i_data-mem-arguments"=> costs[162],
                "un_list_data-cpu-arguments"=> costs[163],
                "un_list_data-mem-arguments"=> costs[164],
                "un_map_data-cpu-arguments"=> costs[165],
                "un_map_data-mem-arguments"=> costs[166],
                "verify_ecdsa_secp256k1_signature-cpu-arguments"=> costs[167],
                "verify_ecdsa_secp256k1_signature-mem-arguments"=> costs[168],
                "verify_ed25519_signature-cpu-arguments-intercept"=> costs[169],
                "verify_ed25519_signature-cpu-arguments-slope"=> costs[170],
                "verify_ed25519_signature-mem-arguments"=> costs[171],
                "verify_schnorr_secp256k1_signature-cpu-arguments-intercept"=> costs[172],
                "verify_schnorr_secp256k1_signature-cpu-arguments-slope"=> costs[173],
                "verify_schnorr_secp256k1_signature-mem-arguments"=> costs[174]
            }
        }
    };
    CostModel {
        machine_costs: MachineCosts {
            startup: ExBudget {
                mem: *cost_map
                    .get("cek_startup_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_startup_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            var: ExBudget {
                mem: *cost_map
                    .get("cek_var_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_var_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            constant: ExBudget {
                mem: *cost_map
                    .get("cek_const_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_const_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            lambda: ExBudget {
                mem: *cost_map
                    .get("cek_lam_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_lam_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            delay: ExBudget {
                mem: *cost_map
                    .get("cek_delay_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_delay_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            force: ExBudget {
                mem: *cost_map
                    .get("cek_force_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_force_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            apply: ExBudget {
                mem: *cost_map
                    .get("cek_apply_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_apply_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            builtin: ExBudget {
                mem: *cost_map
                    .get("cek_builtin_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_builtin_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
        },
        builtin_costs: BuiltinCosts {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("add_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("add_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("add_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("add_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("subtract_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("subtract_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("subtract_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("subtract_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("multiply_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("multiply_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("multiply_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("multiply_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: *cost_map
                        .get("divide_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("divide_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                    minimum: *cost_map
                        .get("divide_integer-mem-arguments-minimum")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: *cost_map
                        .get("divide_integer-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: *cost_map
                            .get("divide_integer-cpu-arguments-model-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("divide_integer-cpu-arguments-model-arguments-slope")
                            .unwrap_or(&30000000000),
                    })),
                }),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: *cost_map
                        .get("quotient_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("quotient_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                    minimum: *cost_map
                        .get("quotient_integer-mem-arguments-minimum")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: *cost_map
                        .get("quotient_integer-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: *cost_map
                            .get("quotient_integer-cpu-arguments-model-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("quotient_integer-cpu-arguments-model-arguments-slope")
                            .unwrap_or(&30000000000),
                    })),
                }),
            },
            remainder_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: *cost_map
                        .get("remainder_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("remainder_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                    minimum: *cost_map
                        .get("remainder_integer-mem-arguments-minimum")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: *cost_map
                        .get("remainder_integer-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: *cost_map
                            .get("remainder_integer-cpu-arguments-model-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("remainder_integer-cpu-arguments-model-arguments-slope")
                            .unwrap_or(&30000000000),
                    })),
                }),
            },
            mod_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: *cost_map
                        .get("mod_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("mod_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                    minimum: *cost_map
                        .get("mod_integer-mem-arguments-minimum")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: *cost_map
                        .get("mod_integer-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: *cost_map
                            .get("mod_integer-cpu-arguments-model-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("mod_integer-cpu-arguments-model-arguments-slope")
                            .unwrap_or(&30000000000),
                    })),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_integer-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("equals_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_integer-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_equals_integer-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_equals_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_equals_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_byte_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_byte_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("cons_byte_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("cons_byte_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: *cost_map
                        .get("cons_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("cons_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: *cost_map
                        .get("slice_byte_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("slice_byte_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: *cost_map
                        .get("slice_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("slice_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("length_of_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("length_of_byte_string-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("index_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("index_byte_string-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: *cost_map
                        .get("equals_byte_string-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    intercept: *cost_map
                        .get("equals_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_equals_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_equals_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_equals_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("sha2_256-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("sha2_256-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("sha2_256-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("sha3_256-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("sha3_256-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("sha3_256-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("blake2b_256-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("blake2b_256-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("blake2b_256-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_ed25519_signature-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: *cost_map
                        .get("verify_ed25519_signature-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("verify_ed25519_signature-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_ecdsa_secp256k1_signature-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_ecdsa_secp256k1_signature-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_schnorr_secp256k1_signature-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: *cost_map
                        .get("verify_schnorr_secp256k1_signature-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("verify_schnorr_secp256k1_signature-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: *cost_map
                        .get("equals_string-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    intercept: *cost_map
                        .get("equals_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("encode_utf8-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("encode_utf8-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("encode_utf8-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("encode_utf8-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("decode_utf8-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("decode_utf8-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("decode_utf8-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("decode_utf8-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("if_then_else-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("if_then_else-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("choose_unit-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("choose_unit-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map.get("trace-mem-arguments").unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map.get("trace-cpu-arguments").unwrap_or(&30000000000),
                ),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("fst_pair-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("fst_pair-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("snd_pair-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("snd_pair-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("choose_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("choose_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_cons-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_cons-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("head_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("head_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("tail_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("tail_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("null_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("null_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(
                    *cost_map
                        .get("choose_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: SixArguments::ConstantCost(
                    *cost_map
                        .get("choose_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("constr_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("constr_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("map_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("map_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("list_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("list_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map.get("i_data-mem-arguments").unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map.get("i_data-cpu-arguments").unwrap_or(&30000000000),
                ),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map.get("b_data-mem-arguments").unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map.get("b_data-cpu-arguments").unwrap_or(&30000000000),
                ),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_constr_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_constr_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_map_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_map_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_list_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_list_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_i_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_i_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_b_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_b_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("equals_data-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_data-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_pair_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_pair_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_pair_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_pair_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("serialise_data-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("serialise_data-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("serialise_data-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("serialise_data-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
        },
    }
}

pub struct CostingFun<T> {
    pub mem: T,
    pub cpu: T,
}

pub enum OneArgument {
    ConstantCost(i64),
    LinearCost(LinearSize),
}

impl OneArgument {
    pub fn cost(&self, x: i64) -> i64 {
        match self {
            OneArgument::ConstantCost(c) => *c,
            OneArgument::LinearCost(m) => m.slope * x + m.intercept,
        }
    }
}
#[derive(Clone)]
pub enum TwoArguments {
    ConstantCost(i64),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    AddedSizes(AddedSizes),
    SubtractedSizes(SubtractedSizes),
    MultipliedSizes(MultipliedSizes),
    MinSize(MinSize),
    MaxSize(MaxSize),
    LinearOnDiagonal(ConstantOrLinear),
    ConstAboveDiagonal(ConstantOrTwoArguments),
    ConstBelowDiagonal(ConstantOrTwoArguments),
}
impl TwoArguments {
    pub fn cost(&self, x: i64, y: i64) -> i64 {
        match self {
            TwoArguments::ConstantCost(c) => *c,
            TwoArguments::LinearInX(l) => l.slope * x + l.intercept,
            TwoArguments::LinearInY(l) => l.slope * y + l.intercept,
            TwoArguments::AddedSizes(s) => s.slope * (x + y) + s.intercept,
            TwoArguments::SubtractedSizes(s) => s.slope * s.minimum.max(x - y) + s.intercept,
            TwoArguments::MultipliedSizes(s) => s.slope * (x * y) + s.intercept,
            TwoArguments::MinSize(s) => s.slope * x.min(y) + s.intercept,
            TwoArguments::MaxSize(s) => s.slope * x.max(y) + s.intercept,
            TwoArguments::LinearOnDiagonal(l) => {
                if x == y {
                    x * l.slope + l.intercept
                } else {
                    l.constant
                }
            }
            TwoArguments::ConstAboveDiagonal(l) => {
                if x > y {
                    l.constant
                } else {
                    let p = *l.model.clone();
                    p.cost(x, y)
                }
            }
            TwoArguments::ConstBelowDiagonal(l) => {
                if x < y {
                    l.constant
                } else {
                    let p = *l.model.clone();
                    p.cost(x, y)
                }
            }
        }
    }
}

pub enum ThreeArguments {
    ConstantCost(i64),
    AddedSizes(AddedSizes),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    LinearInZ(LinearSize),
}

impl ThreeArguments {
    pub fn cost(&self, x: i64, y: i64, z: i64) -> i64 {
        match self {
            ThreeArguments::ConstantCost(c) => *c,
            ThreeArguments::AddedSizes(s) => (x + y + z) * s.slope + s.intercept,
            ThreeArguments::LinearInX(l) => x * l.slope + l.intercept,
            ThreeArguments::LinearInY(l) => y * l.slope + l.intercept,
            ThreeArguments::LinearInZ(l) => z * l.slope + l.intercept,
        }
    }
}

pub enum SixArguments {
    ConstantCost(i64),
}

impl SixArguments {
    pub fn cost(&self, _: i64, _: i64, _: i64, _: i64, _: i64, _: i64) -> i64 {
        match self {
            SixArguments::ConstantCost(c) => *c,
        }
    }
}

#[derive(Clone)]
pub struct LinearSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct AddedSizes {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct SubtractedSizes {
    pub intercept: i64,
    pub slope: i64,
    pub minimum: i64,
}

#[derive(Clone)]
pub struct MultipliedSizes {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct MinSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct MaxSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct ConstantOrLinear {
    pub constant: i64,
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct ConstantOrTwoArguments {
    pub constant: i64,
    pub model: Box<TwoArguments>,
}

#[repr(u8)]
pub enum StepKind {
    Constant = 0,
    Var = 1,
    Lambda = 2,
    Apply = 3,
    Delay = 4,
    Force = 5,
    Builtin = 6,
    // DO NOT USE THIS IN `step_and_maybe_spend`
    StartUp = 7,
}

impl TryFrom<u8> for StepKind {
    type Error = super::error::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(StepKind::Constant),
            1 => Ok(StepKind::Var),
            2 => Ok(StepKind::Lambda),
            3 => Ok(StepKind::Apply),
            4 => Ok(StepKind::Delay),
            5 => Ok(StepKind::Force),
            6 => Ok(StepKind::Builtin),
            v => Err(super::error::Error::InvalidStepKind(v)),
        }
    }
}
