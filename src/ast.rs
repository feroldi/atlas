#[derive(PartialEq, Eq, Debug)]
pub(crate) struct TranslationUnit {
    pub(crate) external_decls: Vec<ExternalDecl>,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum ExternalDecl {
    VarDecl(VarDecl),
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct VarDecl {
    pub(crate) type_specifier: Type,
    // TODO(feroldi): This cannot be a string for long, it has to be a Symbol thing.
    pub(crate) identifier: String,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum Type {
    BuiltinType(BuiltinTypeKind),
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum BuiltinTypeKind {
    Int,
}