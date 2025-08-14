use derive_more::Display;

use crate::basic::cls::basic_types::BasicType;
use crate::basic::cls::custom_class::CustomClass;

#[derive(Clone, Debug, Display)]
#[display("{}", _0)]
pub enum Class {
    Basic(BasicType),
    Custom(CustomClass),
}
