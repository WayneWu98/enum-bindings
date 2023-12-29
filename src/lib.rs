pub trait Meta {
    type MetaType: Default;
    fn meta(&self) -> Self::MetaType;
}

extern crate macros;

pub use macros::derive_meta;
