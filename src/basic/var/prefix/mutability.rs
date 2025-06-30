use derive_more::Display;

#[derive(Debug, Default, Display)]
pub enum VariableMutability {
    #[display("{} ", Self::VAL_REPR)]
    #[default]
    Val,

    #[display("{} ", Self::VAR_REPR)]
    Var,
}

impl VariableMutability {
    pub const VAL_REPR: &str = "val";
    pub const VAR_REPR: &str = "var";
}
