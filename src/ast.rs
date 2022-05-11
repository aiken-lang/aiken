use strum_macros::EnumString;

#[derive(Debug)]
pub struct Program {
    pub version: String,
    pub term: Term,
}

#[derive(Debug, Clone)]
pub enum Term {
    // tag: 0
    Var(String),
    // tag: 1
    Delay(Box<Term>),
    // tag: 2
    Lambda {
        parameter_name: String,
        body: Box<Term>,
    },
    // tag: 3
    Apply {
        function: Box<Term>,
        argument: Box<Term>,
    },
    // tag: 4
    Constant(Constant),
    // tag: 5
    Force(Box<Term>),
    // tag: 6
    Error(Box<Term>),
    // tag: 7
    Builtin(DefaultFunction),
}

#[derive(Debug, Clone)]
pub enum Constant {
    // TODO: figure out the right size for this
    // tag: 0
    Integer(i64),
    // tag: 1
    ByteString(Vec<u8>),
    // tag: 2
    String(String),
    // tag: 3
    Char(char),
    // tag: 4
    Unit,
    // tag: 5
    Bool(bool),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, EnumString)]
pub enum DefaultFunction {
    // Integer functions
    AddInteger,
    SubtractInteger,
    MultiplyInteger,
    DivideInteger,
    QuotientInteger,
    RemainderInteger,
    ModInteger,
    EqualsInteger,
    LessThanInteger,
    LessThanEqualsInteger,
    // ByteString functions
    AppendByteString,
    ConsByteString,
    SliceByteString,
    LengthOfByteString,
    IndexByteString,
    EqualsByteString,
    LessThanByteString,
    LessThanEqualsByteString,
    // Cryptography and hash functions
    Sha2_256,
    Sha3_256,
    Blake2b_256,
    VerifySignature,
    VerifyEcdsaSecp256k1Signature,
    VerifySchnorrSecp256k1Signature,
    // String functions
    AppendString,
    EqualsString,
    EncodeUtf8,
    DecodeUtf8,
    // Bool function
    IfThenElse,
    // Unit function
    ChooseUnit,
    // Tracing function
    Trace,
    // Pairs functions
    FstPair,
    SndPair,
    // List functions
    ChooseList,
    MkCons,
    HeadList,
    TailList,
    NullList,
    // Data functions
    // It is convenient to have a "choosing" function for a data type that has more than two
    // constructors to get pattern matching over it and we may end up having multiple such data
    // types, hence we include the name of the data type as a suffix.
    ChooseData,
    ConstrData,
    MapData,
    ListData,
    IData,
    BData,
    UnConstrData,
    UnMapData,
    UnListData,
    UnIData,
    UnBData,
    EqualsData,
    SerialiseData,
    // Misc constructors
    // Constructors that we need for constructing e.g. Data. Polymorphic builtin
    // constructors are often problematic (See note [Representable built-in
    // functions over polymorphic built-in types])
    MkPairData,
    MkNilData,
    MkNilPairData,
}
