use std::{fmt::Display, rc::Rc, str::FromStr};

use strum_macros::EnumIter;

use flat_rs::de;

use crate::ast::Term;

/// All the possible builtin functions in Untyped Plutus Core.
#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq, Copy, EnumIter)]
pub enum DefaultFunction {
    // Integer functions
    AddInteger = 0,
    SubtractInteger = 1,
    MultiplyInteger = 2,
    DivideInteger = 3,
    QuotientInteger = 4,
    RemainderInteger = 5,
    ModInteger = 6,
    EqualsInteger = 7,
    LessThanInteger = 8,
    LessThanEqualsInteger = 9,
    // ByteString functions
    AppendByteString = 10,
    ConsByteString = 11,
    SliceByteString = 12,
    LengthOfByteString = 13,
    IndexByteString = 14,
    EqualsByteString = 15,
    LessThanByteString = 16,
    LessThanEqualsByteString = 17,
    // Cryptography and hash functions
    Sha2_256 = 18,
    Sha3_256 = 19,
    Blake2b_256 = 20,
    VerifyEd25519Signature = 21,
    VerifyEcdsaSecp256k1Signature = 52,
    VerifySchnorrSecp256k1Signature = 53,
    // String functions
    AppendString = 22,
    EqualsString = 23,
    EncodeUtf8 = 24,
    DecodeUtf8 = 25,
    // Bool function
    IfThenElse = 26,
    // Unit function
    ChooseUnit = 27,
    // Tracing function
    Trace = 28,
    // Pairs functions
    FstPair = 29,
    SndPair = 30,
    // List functions
    ChooseList = 31,
    MkCons = 32,
    HeadList = 33,
    TailList = 34,
    NullList = 35,
    // Data functions
    // It is convenient to have a "choosing" function for a data type that has more than two
    // constructors to get pattern matching over it and we may end up having multiple such data
    // types, hence we include the name of the data type as a suffix.
    ChooseData = 36,
    ConstrData = 37,
    MapData = 38,
    ListData = 39,
    IData = 40,
    BData = 41,
    UnConstrData = 42,
    UnMapData = 43,
    UnListData = 44,
    UnIData = 45,
    UnBData = 46,
    EqualsData = 47,
    SerialiseData = 51,
    // Misc constructors
    // Constructors that we need for constructing e.g. Data. Polymorphic builtin
    // constructors are often problematic (See note [Representable built-in
    // functions over polymorphic built-in types])
    MkPairData = 48,
    MkNilData = 49,
    MkNilPairData = 50,
}

impl TryFrom<u8> for DefaultFunction {
    type Error = de::Error;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            v if v == DefaultFunction::AddInteger as u8 => Ok(DefaultFunction::AddInteger),
            v if v == DefaultFunction::SubtractInteger as u8 => {
                Ok(DefaultFunction::SubtractInteger)
            }
            v if v == DefaultFunction::MultiplyInteger as u8 => {
                Ok(DefaultFunction::MultiplyInteger)
            }
            v if v == DefaultFunction::DivideInteger as u8 => Ok(DefaultFunction::DivideInteger),
            v if v == DefaultFunction::QuotientInteger as u8 => {
                Ok(DefaultFunction::QuotientInteger)
            }
            v if v == DefaultFunction::RemainderInteger as u8 => {
                Ok(DefaultFunction::RemainderInteger)
            }
            v if v == DefaultFunction::ModInteger as u8 => Ok(DefaultFunction::ModInteger),
            v if v == DefaultFunction::EqualsInteger as u8 => Ok(DefaultFunction::EqualsInteger),
            v if v == DefaultFunction::LessThanInteger as u8 => {
                Ok(DefaultFunction::LessThanInteger)
            }
            v if v == DefaultFunction::LessThanEqualsInteger as u8 => {
                Ok(DefaultFunction::LessThanEqualsInteger)
            }
            // ByteString functions
            v if v == DefaultFunction::AppendByteString as u8 => {
                Ok(DefaultFunction::AppendByteString)
            }
            v if v == DefaultFunction::ConsByteString as u8 => Ok(DefaultFunction::ConsByteString),
            v if v == DefaultFunction::SliceByteString as u8 => {
                Ok(DefaultFunction::SliceByteString)
            }
            v if v == DefaultFunction::LengthOfByteString as u8 => {
                Ok(DefaultFunction::LengthOfByteString)
            }
            v if v == DefaultFunction::IndexByteString as u8 => {
                Ok(DefaultFunction::IndexByteString)
            }
            v if v == DefaultFunction::EqualsByteString as u8 => {
                Ok(DefaultFunction::EqualsByteString)
            }
            v if v == DefaultFunction::LessThanByteString as u8 => {
                Ok(DefaultFunction::LessThanByteString)
            }
            v if v == DefaultFunction::LessThanEqualsByteString as u8 => {
                Ok(DefaultFunction::LessThanEqualsByteString)
            }
            // Cryptography and hash functions
            v if v == DefaultFunction::Sha2_256 as u8 => Ok(DefaultFunction::Sha2_256),
            v if v == DefaultFunction::Sha3_256 as u8 => Ok(DefaultFunction::Sha3_256),
            v if v == DefaultFunction::Blake2b_256 as u8 => Ok(DefaultFunction::Blake2b_256),
            v if v == DefaultFunction::VerifyEd25519Signature as u8 => {
                Ok(DefaultFunction::VerifyEd25519Signature)
            }
            v if v == DefaultFunction::VerifyEcdsaSecp256k1Signature as u8 => {
                Ok(DefaultFunction::VerifyEcdsaSecp256k1Signature)
            }
            v if v == DefaultFunction::VerifySchnorrSecp256k1Signature as u8 => {
                Ok(DefaultFunction::VerifySchnorrSecp256k1Signature)
            }
            // String functions
            v if v == DefaultFunction::AppendString as u8 => Ok(DefaultFunction::AppendString),
            v if v == DefaultFunction::EqualsString as u8 => Ok(DefaultFunction::EqualsString),
            v if v == DefaultFunction::EncodeUtf8 as u8 => Ok(DefaultFunction::EncodeUtf8),
            v if v == DefaultFunction::DecodeUtf8 as u8 => Ok(DefaultFunction::DecodeUtf8),
            // Bool function
            v if v == DefaultFunction::IfThenElse as u8 => Ok(DefaultFunction::IfThenElse),
            // Unit function
            v if v == DefaultFunction::ChooseUnit as u8 => Ok(DefaultFunction::ChooseUnit),
            // Tracing function
            v if v == DefaultFunction::Trace as u8 => Ok(DefaultFunction::Trace),
            // Pairs functions
            v if v == DefaultFunction::FstPair as u8 => Ok(DefaultFunction::FstPair),
            v if v == DefaultFunction::SndPair as u8 => Ok(DefaultFunction::SndPair),
            // List functions
            v if v == DefaultFunction::ChooseList as u8 => Ok(DefaultFunction::ChooseList),
            v if v == DefaultFunction::MkCons as u8 => Ok(DefaultFunction::MkCons),
            v if v == DefaultFunction::HeadList as u8 => Ok(DefaultFunction::HeadList),
            v if v == DefaultFunction::TailList as u8 => Ok(DefaultFunction::TailList),
            v if v == DefaultFunction::NullList as u8 => Ok(DefaultFunction::NullList),
            // Data functions
            // It is convenient to have a "choosing" function for a data type that has more than two
            // constructors to get pattern matching over it and we may end up having multiple such data
            // types, hence we include the name of the data type as a suffix.
            v if v == DefaultFunction::ChooseData as u8 => Ok(DefaultFunction::ChooseData),
            v if v == DefaultFunction::ConstrData as u8 => Ok(DefaultFunction::ConstrData),
            v if v == DefaultFunction::MapData as u8 => Ok(DefaultFunction::MapData),
            v if v == DefaultFunction::ListData as u8 => Ok(DefaultFunction::ListData),
            v if v == DefaultFunction::IData as u8 => Ok(DefaultFunction::IData),
            v if v == DefaultFunction::BData as u8 => Ok(DefaultFunction::BData),
            v if v == DefaultFunction::UnConstrData as u8 => Ok(DefaultFunction::UnConstrData),
            v if v == DefaultFunction::UnMapData as u8 => Ok(DefaultFunction::UnMapData),
            v if v == DefaultFunction::UnListData as u8 => Ok(DefaultFunction::UnListData),
            v if v == DefaultFunction::UnIData as u8 => Ok(DefaultFunction::UnIData),
            v if v == DefaultFunction::UnBData as u8 => Ok(DefaultFunction::UnBData),
            v if v == DefaultFunction::EqualsData as u8 => Ok(DefaultFunction::EqualsData),
            v if v == DefaultFunction::SerialiseData as u8 => Ok(DefaultFunction::SerialiseData),
            // Misc constructors
            // Constructors that we need for constructing e.g. Data. Polymorphic builtin
            // constructors are often problematic (See note [Representable built-in
            // functions over polymorphic built-in types])
            v if v == DefaultFunction::MkPairData as u8 => Ok(DefaultFunction::MkPairData),
            v if v == DefaultFunction::MkNilData as u8 => Ok(DefaultFunction::MkNilData),
            v if v == DefaultFunction::MkNilPairData as u8 => Ok(DefaultFunction::MkNilPairData),
            _ => Err(de::Error::Message(format!(
                "Default Function not found - {}",
                v
            ))),
        }
    }
}

impl FromStr for DefaultFunction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use DefaultFunction::*;

        match s {
            "addInteger" => Ok(AddInteger),
            "subtractInteger" => Ok(SubtractInteger),
            "multiplyInteger" => Ok(MultiplyInteger),
            "divideInteger" => Ok(DivideInteger),
            "quotientInteger" => Ok(QuotientInteger),
            "remainderInteger" => Ok(RemainderInteger),
            "modInteger" => Ok(ModInteger),
            "equalsInteger" => Ok(EqualsInteger),
            "lessThanInteger" => Ok(LessThanInteger),
            "lessThanEqualsInteger" => Ok(LessThanEqualsInteger),
            "appendByteString" => Ok(AppendByteString),
            "consByteString" => Ok(ConsByteString),
            "sliceByteString" => Ok(SliceByteString),
            "lengthOfByteString" => Ok(LengthOfByteString),
            "indexByteString" => Ok(IndexByteString),
            "equalsByteString" => Ok(EqualsByteString),
            "lessThanByteString" => Ok(LessThanByteString),
            "lessThanEqualsByteString" => Ok(LessThanEqualsByteString),
            "sha2_256" => Ok(Sha2_256),
            "sha3_256" => Ok(Sha3_256),
            "blake2b_256" => Ok(Blake2b_256),
            "verifyEd25519Signature" => Ok(VerifyEd25519Signature),
            "verifyEcdsaSecp256k1Signature" => Ok(VerifyEcdsaSecp256k1Signature),
            "verifySchnorrSecp256k1Signature" => Ok(VerifySchnorrSecp256k1Signature),
            "appendString" => Ok(AppendString),
            "equalsString" => Ok(EqualsString),
            "encodeUtf8" => Ok(EncodeUtf8),
            "decodeUtf8" => Ok(DecodeUtf8),
            "ifThenElse" => Ok(IfThenElse),
            "chooseUnit" => Ok(ChooseUnit),
            "trace" => Ok(Trace),
            "fstPair" => Ok(FstPair),
            "sndPair" => Ok(SndPair),
            "chooseList" => Ok(ChooseList),
            "mkCons" => Ok(MkCons),
            "headList" => Ok(HeadList),
            "tailList" => Ok(TailList),
            "nullList" => Ok(NullList),
            "chooseData" => Ok(ChooseData),
            "constrData" => Ok(ConstrData),
            "mapData" => Ok(MapData),
            "listData" => Ok(ListData),
            "iData" => Ok(IData),
            "bData" => Ok(BData),
            "unConstrData" => Ok(UnConstrData),
            "unMapData" => Ok(UnMapData),
            "unListData" => Ok(UnListData),
            "unIData" => Ok(UnIData),
            "unBData" => Ok(UnBData),
            "equalsData" => Ok(EqualsData),
            "serialiseData" => Ok(SerialiseData),
            "mkPairData" => Ok(MkPairData),
            "mkNilData" => Ok(MkNilData),
            "mkNilPairData" => Ok(MkNilPairData),
            rest => Err(format!("Default Function not found - {}", rest)),
        }
    }
}

impl Display for DefaultFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DefaultFunction::*;

        match self {
            AddInteger => write!(f, "addInteger"),
            SubtractInteger => write!(f, "subtractInteger"),
            MultiplyInteger => write!(f, "multiplyInteger"),
            DivideInteger => write!(f, "divideInteger"),
            QuotientInteger => write!(f, "quotientInteger"),
            RemainderInteger => write!(f, "remainderInteger"),
            ModInteger => write!(f, "modInteger"),
            EqualsInteger => write!(f, "equalsInteger"),
            LessThanInteger => write!(f, "lessThanInteger"),
            LessThanEqualsInteger => write!(f, "lessThanEqualsInteger"),
            AppendByteString => write!(f, "appendByteString"),
            ConsByteString => write!(f, "consByteString"),
            SliceByteString => write!(f, "sliceByteString"),
            LengthOfByteString => write!(f, "lengthOfByteString"),
            IndexByteString => write!(f, "indexByteString"),
            EqualsByteString => write!(f, "equalsByteString"),
            LessThanByteString => write!(f, "lessThanByteString"),
            LessThanEqualsByteString => write!(f, "lessThanEqualsByteString"),
            Sha2_256 => write!(f, "sha2_256"),
            Sha3_256 => write!(f, "sha3_256"),
            Blake2b_256 => write!(f, "blake2b_256"),
            VerifyEd25519Signature => write!(f, "verifySignature"),
            VerifyEcdsaSecp256k1Signature => write!(f, "verifyEcdsaSecp256k1Signature"),
            VerifySchnorrSecp256k1Signature => write!(f, "verifySchnorrSecp256k1Signature"),
            AppendString => write!(f, "appendString"),
            EqualsString => write!(f, "equalsString"),
            EncodeUtf8 => write!(f, "encodeUtf8"),
            DecodeUtf8 => write!(f, "decodeUtf8"),
            IfThenElse => write!(f, "ifThenElse"),
            ChooseUnit => write!(f, "chooseUnit"),
            Trace => write!(f, "trace"),
            FstPair => write!(f, "fstPair"),
            SndPair => write!(f, "sndPair"),
            ChooseList => write!(f, "chooseList"),
            MkCons => write!(f, "mkCons"),
            HeadList => write!(f, "headList"),
            TailList => write!(f, "tailList"),
            NullList => write!(f, "nullList"),
            ChooseData => write!(f, "chooseData"),
            ConstrData => write!(f, "constrData"),
            MapData => write!(f, "mapData"),
            ListData => write!(f, "listData"),
            IData => write!(f, "iData"),
            BData => write!(f, "bData"),
            UnConstrData => write!(f, "unConstrData"),
            UnMapData => write!(f, "unMapData"),
            UnListData => write!(f, "unListData"),
            UnIData => write!(f, "unIData"),
            UnBData => write!(f, "unBData"),
            EqualsData => write!(f, "equalsData"),
            SerialiseData => write!(f, "serialiseData"),
            MkPairData => write!(f, "mkPairData"),
            MkNilData => write!(f, "mkNilData"),
            MkNilPairData => write!(f, "mkNilPairData"),
        }
    }
}

impl DefaultFunction {
    pub fn aiken_name(&self) -> String {
        use DefaultFunction::*;

        match self {
            AddInteger => "add_integer",
            SubtractInteger => "subtract_integer",
            MultiplyInteger => "multiply_integer",
            DivideInteger => "divide_integer",
            QuotientInteger => "quotient_integer",
            RemainderInteger => "remainder_integer",
            ModInteger => "mod_integer",
            EqualsInteger => "equals_integer",
            LessThanInteger => "less_than_integer",
            LessThanEqualsInteger => "less_than_equals_integer",
            AppendByteString => "append_bytearray",
            ConsByteString => "cons_bytearray",
            SliceByteString => "slice_bytearray",
            LengthOfByteString => "length_of_bytearray",
            IndexByteString => "index_bytearray",
            EqualsByteString => "equals_bytearray",
            LessThanByteString => "less_than_bytearray",
            LessThanEqualsByteString => "less_than_equals_bytearray",
            Sha2_256 => "sha2_256",
            Sha3_256 => "sha3_256",
            Blake2b_256 => "blake2b_256",
            VerifyEd25519Signature => "verify_signature",
            VerifyEcdsaSecp256k1Signature => "verify_ecdsa_secp256k1_signature",
            VerifySchnorrSecp256k1Signature => "verify_schnorr_secp256k1_signature",
            AppendString => "append_string",
            EqualsString => "equals_string",
            EncodeUtf8 => "encode_utf8",
            DecodeUtf8 => "decode_utf8",
            IfThenElse => "if_then_else",
            ChooseUnit => "choose_unit",
            Trace => "trace",
            FstPair => "fst_pair",
            SndPair => "snd_pair",
            ChooseList => "choose_list",
            MkCons => "mk_cons",
            HeadList => "head_list",
            TailList => "tail_list",
            NullList => "null_list",
            ChooseData => "choose_data",
            ConstrData => "constr_data",
            MapData => "map_data",
            ListData => "list_data",
            IData => "i_data",
            BData => "b_data",
            UnConstrData => "un_constr_data",
            UnMapData => "un_map_data",
            UnListData => "un_list_data",
            UnIData => "un_i_data",
            UnBData => "un_b_data",
            EqualsData => "equals_data",
            SerialiseData => "serialise_data",
            MkPairData => "mk_pair_data",
            MkNilData => "mk_nil_data",
            MkNilPairData => "mk_nil_pair_data",
        }
        .to_string()
    }
}

impl<T> From<DefaultFunction> for Term<T> {
    fn from(builtin: DefaultFunction) -> Self {
        Term::Builtin(builtin)
    }
}

impl<T> From<DefaultFunction> for Rc<Term<T>> {
    fn from(builtin: DefaultFunction) -> Self {
        Term::Builtin(builtin).into()
    }
}
