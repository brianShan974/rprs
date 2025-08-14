use crate::basic::cls::basic_types::BasicType;
use crate::basic::cls::custom_class::CustomClass;

#[derive(Clone, Debug)]
pub enum Class {
    Basic(BasicType),
    Custom(CustomClass),
}
