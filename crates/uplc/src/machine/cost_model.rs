/// Can be negative
#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub struct ExBudget {
    pub mem: i32,
    pub cpu: i32,
}

impl ExBudget {
    pub fn occurences(&mut self, n: i32) {
        self.mem *= n;
        self.cpu *= n;
    }
}
#[derive(Default)]
pub struct CostModel {
    pub machine_costs: MachineCosts,
    pub builtin_costs: BuiltinCosts,
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
}

impl Default for MachineCosts {
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

impl Default for BuiltinCosts {
    fn default() -> Self {
        Self {
            add_integer: CostingFun {
                memory: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            subtract_integer: CostingFun {
                memory: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            multiply_integer: CostingFun {
                memory: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),

                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 69522,
                    slope: 11687,
                }),
            },
            divide_integer: CostingFun {
                memory: TwoArguments::SubtractedSizes(SubtractedSizes {
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
                memory: TwoArguments::SubtractedSizes(SubtractedSizes {
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
                memory: TwoArguments::SubtractedSizes(SubtractedSizes {
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
                memory: TwoArguments::SubtractedSizes(SubtractedSizes {
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
                memory: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208512,
                    slope: 421,
                }),
            },
            less_than_integer: CostingFun {
                memory: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208896,
                    slope: 511,
                }),
            },
            less_than_equals_integer: CostingFun {
                memory: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 204924,
                    slope: 473,
                }),
            },
            append_byte_string: CostingFun {
                memory: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 571,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
            },
            cons_byte_string: CostingFun {
                memory: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 221973,
                    slope: 511,
                }),
            },
            slice_byte_string: CostingFun {
                memory: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 265318,
                    slope: 0,
                }),
            },
            length_of_byte_string: CostingFun {
                memory: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(1000),
            },
            index_byte_string: CostingFun {
                memory: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(57667),
            },
            equals_byte_string: CostingFun {
                memory: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 245000,
                    intercept: 216773,
                    slope: 62,
                }),
            },
            less_than_byte_string: CostingFun {
                memory: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            sha2_256: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            sha3_256: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            blake2b_256: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            verify_ed25519_signature: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            append_string: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            equals_string: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            encode_utf8: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            decode_utf8: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            if_then_else: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            choose_unit: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            trace: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            fst_pair: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            snd_pair: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            choose_list: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            mk_cons: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            head_list: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            tail_list: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            null_list: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            choose_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            constr_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            map_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            list_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            i_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            b_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            un_constr_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            un_map_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            un_list_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            un_i_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            un_b_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            equals_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            mk_pair_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            mk_nil_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            mk_nil_pair_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
            serialise_data: CostingFun {
                memory: todo!(),
                cpu: todo!(),
            },
        }
    }
}

pub struct CostingFun<T> {
    pub memory: T,
    pub cpu: T,
}

pub enum OneArgument {
    ConstantCost(isize),
    LinearCost(LinearSize),
}

pub enum TwoArguments {
    ConstantCost(isize),
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

pub enum ThreeArguments {
    ConstantCost(isize),
    AddedSizes(AddedSizes),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    LinearInZ(LinearSize),
}

pub enum SixArguments {
    ConstantCost(isize),
}

pub struct LinearSize {
    pub intercept: isize,
    pub slope: isize,
}

pub struct AddedSizes {
    pub intercept: isize,
    pub slope: isize,
}

pub struct SubtractedSizes {
    pub intercept: isize,
    pub slope: isize,
    pub minimum: isize,
}

pub struct MultipliedSizes {
    pub intercept: isize,
    pub slope: isize,
}

pub struct MinSize {
    pub intercept: isize,
    pub slope: isize,
}

pub struct MaxSize {
    pub intercept: isize,
    pub slope: isize,
}

pub struct ConstantOrLinear {
    pub constant: isize,
    pub intercept: isize,
    pub slope: isize,
}

pub struct ConstantOrTwoArguments {
    pub constant: isize,
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
