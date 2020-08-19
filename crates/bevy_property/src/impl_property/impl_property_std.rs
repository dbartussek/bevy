use crate::{
    impl_property,
    property_serde::{SeqSerializer, Serializable},
    Properties, Property, PropertyIter, PropertyType, PropertyTypeRegistry,
};
use serde::{Deserialize, Serialize};
use std::{
    any::Any,
    collections::{BTreeMap, HashMap, HashSet},
    hash::Hash,
    ops::Range,
};

impl<T> Properties for Vec<T>
where
    T: Property + Clone + Default,
{
    fn prop(&self, _name: &str) -> Option<&dyn Property> {
        None
    }

    fn prop_mut(&mut self, _name: &str) -> Option<&mut dyn Property> {
        None
    }

    fn prop_with_index(&self, index: usize) -> Option<&dyn Property> {
        Some(&self[index])
    }

    fn prop_with_index_mut(&mut self, index: usize) -> Option<&mut dyn Property> {
        Some(&mut self[index])
    }

    fn prop_name(&self, _index: usize) -> Option<&str> {
        None
    }

    fn prop_len(&self) -> usize {
        self.len()
    }

    fn iter_props(&self) -> PropertyIter {
        PropertyIter::new(self)
    }
}

impl<T> Property for Vec<T>
where
    T: Property + Clone + Default,
{
    fn type_name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    fn any(&self) -> &dyn Any {
        self
    }

    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn clone_prop(&self) -> Box<dyn Property> {
        Box::new(self.clone())
    }

    fn set(&mut self, value: &dyn Property) {
        if let Some(properties) = value.as_properties() {
            let len = properties.prop_len();
            self.resize_with(len, || T::default());

            if properties.property_type() != self.property_type() {
                panic!(
                    "Properties type mismatch. This type is {:?} but the applied type is {:?}",
                    self.property_type(),
                    properties.property_type()
                );
            }
            for (i, prop) in properties.iter_props().enumerate() {
                self.prop_with_index_mut(i).map(|p| p.apply(prop));
            }
        } else {
            panic!("attempted to apply non-Properties type to Properties type");
        }
    }

    fn apply(&mut self, value: &dyn Property) {
        self.set(value);
    }

    fn as_properties(&self) -> Option<&dyn Properties> {
        Some(self)
    }

    fn serializable<'a>(&'a self, registry: &'a PropertyTypeRegistry) -> Serializable<'a> {
        Serializable::Owned(Box::new(SeqSerializer::new(self, registry)))
    }

    fn property_type(&self) -> PropertyType {
        PropertyType::Seq
    }
}

// impl_property!(SEQUENCE, VecDeque<T> where T: Clone + Send + Sync + Serialize + 'static);
impl_property!(Option<T> where T: Clone + Send + Sync + Serialize + for<'de> Deserialize<'de> + 'static);
impl_property!(HashSet<T> where T: Clone + Eq + Send + Sync + Hash + Serialize + for<'de> Deserialize<'de> + 'static);
impl_property!(HashMap<K, V> where
    K: Clone + Eq + Send + Sync + Hash + Serialize + for<'de> Deserialize<'de> + 'static,
    V: Clone + Send + Sync + Serialize + for<'de> Deserialize<'de> + 'static,);
impl_property!(BTreeMap<K, V> where
    K: Clone + Ord + Send + Sync + Serialize + for<'de> Deserialize<'de> + 'static,
    V: Clone + Send + Sync + Serialize + for<'de> Deserialize<'de> + 'static);
impl_property!(Range<T> where T: Clone + Send + Sync + Serialize + for<'de> Deserialize<'de> + 'static);

// TODO: Implement lossless primitive types in RON and remove all of these primitive "cast checks"
impl Property for String {
    #[inline]
    fn type_name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    #[inline]
    fn any(&self) -> &dyn Any {
        self
    }

    #[inline]
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }

    #[inline]
    fn clone_prop(&self) -> Box<dyn Property> {
        Box::new(self.clone())
    }

    #[inline]
    fn apply(&mut self, value: &dyn Property) {
        self.set(value);
    }

    fn set(&mut self, property: &dyn Property) {
        let value = property.any();
        if let Some(prop) = value.downcast_ref::<Self>() {
            *self = prop.clone();
        } else {
            panic!(
                "prop value is not {}, but {}",
                std::any::type_name::<Self>(),
                property.type_name()
            );
        }
    }

    fn serializable<'a>(&'a self, _registry: &'a PropertyTypeRegistry) -> Serializable<'a> {
        Serializable::Borrowed(self)
    }
}

impl Property for bool {
    #[inline]
    fn type_name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    #[inline]
    fn any(&self) -> &dyn Any {
        self
    }

    #[inline]
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }

    #[inline]
    fn clone_prop(&self) -> Box<dyn Property> {
        Box::new(self.clone())
    }

    #[inline]
    fn apply(&mut self, value: &dyn Property) {
        self.set(value);
    }

    fn set(&mut self, property: &dyn Property) {
        let value = property.any();
        if let Some(prop) = value.downcast_ref::<Self>() {
            *self = *prop;
        } else {
            panic!(
                "prop value is not {}, but {}",
                std::any::type_name::<Self>(),
                property.type_name()
            );
        }
    }

    fn serializable<'a>(&'a self, _registry: &'a PropertyTypeRegistry) -> Serializable<'a> {
        Serializable::Borrowed(self)
    }
}

unsafe fn downcast_ref_unchecked<'lt, T>(v: &'lt (dyn Any + 'static)) -> &'lt T
where
    T: Any,
{
    &*(v as *const dyn Any as *const T)
}

/// A variadic implementation of Any::downcast_match()
///
/// Arguments:
/// - The reference to be casted
/// - An expression that is ran if all cases fail
/// - The identifier to which the casted value should be assigned
/// - The patterns:
///     - Type to cast to
///     - Expression if the cast was successful
macro_rules! downcast_match {
    (
        // This reference will be casted
        $any_ref:expr,

        // What to do if nothing matches
        $else_expr:expr,

        // Assign to this name after casting
        $cast_name:ident,

        // What types to match
        $($cast_type:ty, $then_expr:expr),*
    ) => {{
        // If $any_ref is a function call / has side effects, we don't want to run it twice
        let __any_ref = $any_ref;

        let __type_id = __any_ref.type_id();

        downcast_match_arm!(
            __any_ref, __type_id, $cast_name, $else_expr,
            $($cast_type, $then_expr,)*
        )
    }};
}

/// Internals of downcast_match.
///
/// It has the same arguments as downcast_match with minor differences:
/// - any_type_id caches the call to any_ref.type_id()
/// - One batch of arguments is popped off in the first matcher and an if is constructed from it.
///     The other matcher handles the else case
#[doc(hidden)]
macro_rules! downcast_match_arm {
    (
        // This reference will be casted
        $any_ref:expr,

        // TypeId of $any_ref. We don't want to recompute this every time
        $any_type_id:expr,

        // Assign to this name after casting
        $cast_name:ident,

        // Run that code if the cast fails
        $else_expr:expr,

        // Cast to type
        $cast_type:ty,

        // Run this code after casting the value
        $then_expr:expr,

        // Remaining casts
        $($cast_type_rec:ty, $then_expr_rec:expr,)*
    ) => {{
        if ($any_type_id) == std::any::TypeId::of::<$cast_type>() {
            let $cast_name = unsafe { downcast_ref_unchecked::<$cast_type>($any_ref) };
            $then_expr

        } else {
            downcast_match_arm!(
                $any_ref, $any_type_id, $cast_name, $else_expr,
                $($cast_type_rec, $then_expr_rec,)*
            )
        }
    }};

    // Default case, run $else_expr
    (
        $any_ref:expr,
        $any_type_id:expr,
        $cast_name:ident,
        $else_expr:expr,
    ) => { $else_expr };
}

macro_rules! integer_property {
    ($integer_type:ty) => {
        impl Property for $integer_type {
            #[inline]
            fn type_name(&self) -> &str {
                std::any::type_name::<Self>()
            }

            #[inline]
            fn any(&self) -> &dyn Any {
                self
            }

            #[inline]
            fn any_mut(&mut self) -> &mut dyn Any {
                self
            }

            #[inline]
            fn clone_prop(&self) -> Box<dyn Property> {
                Box::new(self.clone())
            }

            #[inline]
            fn apply(&mut self, value: &dyn Property) {
                self.set(value);
            }

            fn set(&mut self, property: &dyn Property) {
                let value = property.any();

                // Early exit. We care most about this simple case and want it checked first.
                //
                // The compiler calls value.type_id() twice this way,
                // but that's alright.
                if let Some(prop) = value.downcast_ref::<Self>() {
                    *self = *prop;
                } else {
                    downcast_match!(
                        value,
                        panic!(
                            "prop value is not {}, but {}",
                            std::any::type_name::<Self>(),
                            property.type_name()
                        ),
                        prop,
                        isize,
                        {
                            *self = *prop as Self;
                        },
                        i64,
                        {
                            *self = *prop as Self;
                        },
                        i32,
                        {
                            *self = *prop as Self;
                        },
                        i16,
                        {
                            *self = *prop as Self;
                        },
                        i8,
                        {
                            *self = *prop as Self;
                        },
                        usize,
                        {
                            *self = *prop as Self;
                        },
                        u64,
                        {
                            *self = *prop as Self;
                        },
                        u32,
                        {
                            *self = *prop as Self;
                        },
                        u16,
                        {
                            *self = *prop as Self;
                        },
                        u8,
                        {
                            *self = *prop as Self;
                        }
                    );
                }
            }

            fn serializable<'a>(&'a self, _registry: &'a PropertyTypeRegistry) -> Serializable<'a> {
                Serializable::Borrowed(self)
            }
        }
    };
}

macro_rules! float_property {
    ($float_type:ty) => {
        impl Property for $float_type {
            #[inline]
            fn type_name(&self) -> &str {
                std::any::type_name::<Self>()
            }

            #[inline]
            fn any(&self) -> &dyn Any {
                self
            }

            #[inline]
            fn any_mut(&mut self) -> &mut dyn Any {
                self
            }

            #[inline]
            fn clone_prop(&self) -> Box<dyn Property> {
                Box::new(self.clone())
            }

            #[inline]
            fn apply(&mut self, value: &dyn Property) {
                self.set(value);
            }

            fn set(&mut self, property: &dyn Property) {
                let value = property.any();
                if let Some(prop) = value.downcast_ref::<Self>() {
                    *self = *prop as Self;
                } else if let Some(prop) = value.downcast_ref::<f64>() {
                    *self = *prop as Self;
                } else {
                    panic!(
                        "prop value is not {}, but {}",
                        std::any::type_name::<Self>(),
                        property.type_name()
                    );
                }
            }

            fn serializable<'a>(&'a self, _registry: &'a PropertyTypeRegistry) -> Serializable<'a> {
                Serializable::Borrowed(self)
            }
        }
    };
}

integer_property!(usize);
integer_property!(isize);
integer_property!(u8);
integer_property!(u16);
integer_property!(u32);
integer_property!(u64);
integer_property!(i8);
integer_property!(i16);
integer_property!(i32);
integer_property!(i64);

float_property!(f32);
float_property!(f64);
