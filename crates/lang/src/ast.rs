pub struct Module {
    pub name: Vec<String>,
    pub docs: Vec<String>,
    pub is_script: bool,
    pub is_lib: bool,
    pub is_policy: bool,
}

pub enum Definition {
    Fn {
        arguments: Vec<String>,
        body: Expr,
        doc: Option<String>,
        name: String,
        public: bool,
        return_annotation: Option<()>,
        return_type: (),
    },

    TypeAlias {
        alias: String,
        annotation: (),
        doc: Option<String>,
        parameters: Vec<String>,
        public: bool,
        tipo: (),
    },

    DataType {
        constructors: Vec<()>,
        doc: Option<String>,
        name: String,
        opaque: bool,
        parameters: Vec<String>,
        public: bool,
        typed_parameters: Vec<()>,
    },

    Use {
        module: Vec<String>,
        as_name: Option<String>,
        // unqualified: Vec<UnqualifiedImport>,
        // package: PackageName,
    },
}

pub enum Expr {}
