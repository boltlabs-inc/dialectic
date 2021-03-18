use crate::unary::*;
use std::convert::{TryFrom, TryInto};
use thiserror::Error;

/// A `Choice` represents a selection between several protocols offered by [`offer!`](crate::offer).
///
/// It wraps an ordinary non-negative number, with the guarantee that it is *strictly less than* the
/// type level number `N`.
///
/// Unless you are implementing a [`backend`](crate::backend), you do not need to interact with
/// `Choice`s directly. However, all backends must implement [`Transmit<Choice<N>,
/// Val>`](crate::backend::Transmit) and [`Receive<Choice<N>>`](crate::backend::Receive) for all `N`
/// in order to support the [`Choose`](crate::Choose) and [`Offer`](crate::Offer) constructs.
///
/// # Examples
///
/// It's possible to construct a [`Choice`] from all `u8` strictly less than its type parameter `N`:
///
/// ```
/// use std::convert::TryInto;
/// use dialectic::backend::Choice;
/// use dialectic::unary::types::*;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let zero: Choice<3> = 0_u8.try_into()?;
/// let one: Choice<3> = 1_u8.try_into()?;
/// let two: Choice<3> = 2_u8.try_into()?;
///
/// assert_eq!(zero, 0_u8);
/// assert_eq!(one, 1_u8);
/// assert_eq!(two, 2_u8);
/// # Ok(())
/// # }
/// ```
///
/// But we cannot construct a [`Choice`] from a `u8` equal to or greater than its type parameter
/// `N`:
///
/// ```
/// # use std::convert::TryInto;
/// # use dialectic::backend::Choice;
/// # use dialectic::unary::types::*;
/// #
/// let attempted_three: Result<Choice<3>, _> = 3_u8.try_into();
/// let attempted_four: Result<Choice<3>, _> = 4_u8.try_into();
///
/// assert!(attempted_three.is_err());
/// assert!(attempted_four.is_err());
/// ```
///
/// Note that this means `Choice<0>` is unrepresentable, because you cannot choose something from a
/// set of zero things:
///
/// ```
/// # use std::convert::TryInto;
/// # use dialectic::backend::Choice;
/// # use dialectic::unary::types::*;
/// #
/// for i in 0 ..= u8::MAX {
///    let attempt: Result<Choice<0>, _> = i.try_into();
///    assert!(attempt.is_err());
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Choice<const N: usize> {
    choice: u8,
}

// Choice<Z> is unconstructable, but for all N, 0 is a Choice<S<N>>:
impl<M: Unary, const N: usize> Default for Choice<N>
where
    Number<N>: ToUnary<AsUnary = S<M>>,
{
    fn default() -> Self {
        0.try_into()
            .expect("0 is in bounds for all non-zero-bounded `Choice`s")
    }
}

impl<const N: usize> PartialEq<u8> for Choice<N> {
    fn eq(&self, other: &u8) -> bool {
        self.choice == *other
    }
}

/// When attempting to construct a [`Choice<N>`](Choice) via [`try_into`](TryInto::try_into) or
/// [`try_from`](TryFrom::try_from), this error is thrown when the choice exceeds the type-level
/// strict upper bound `N`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
pub struct OutOfBoundsChoiceError {
    choice: u8,
    bound: usize,
}

impl std::fmt::Display for OutOfBoundsChoiceError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "choice {} is invalid for exclusive upper bound {}",
            self.choice, self.bound
        )
    }
}

impl<const N: usize> TryFrom<u8> for Choice<N> {
    type Error = OutOfBoundsChoiceError;

    fn try_from(choice: u8) -> Result<Self, Self::Error> {
        if (choice as usize) < N {
            Ok(Choice { choice })
        } else {
            Err(OutOfBoundsChoiceError { choice, bound: N })
        }
    }
}

impl<const N: usize> From<Choice<N>> for u8 {
    fn from(Choice { choice, .. }: Choice<N>) -> u8 {
        choice
    }
}

// If the serde feature is enabled, do custom serialization for `Choice` that fails when receiving
// an out-of-bounds choice.
#[cfg(feature = "serde")]
mod serialization {
    use super::*;

    use serde::{
        de::{self, Visitor},
        Deserialize, Deserializer, Serialize, Serializer,
    };

    impl<const N: usize> Serialize for Choice<N> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_u8(self.choice)
        }
    }

    #[derive(Debug, Clone, Copy)]
    struct ChoiceVisitor<const N: usize>;

    impl<const N: usize> Default for ChoiceVisitor<N> {
        fn default() -> Self {
            ChoiceVisitor
        }
    }

    impl<'de, const N: usize> Visitor<'de> for ChoiceVisitor<N> {
        type Value = Choice<N>;

        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(
                f,
                "a non-negative integer strictly less than {}",
                N.min(u8::MAX as usize)
            )?;
            if N == 0 {
                write!(
                    f,
                    " (since that strict upper bound is 0, this is impossible)"
                )?;
            }
            Ok(())
        }

        // Only `visit_u64` is implemented because all the other `visit_u*` methods forward to this
        // one by default, per the serde documentation.
        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            let choice: u8 = v
                .try_into()
                .map_err(|_| de::Error::invalid_value(de::Unexpected::Unsigned(v), &self))?;
            choice.try_into().map_err(|_| {
                de::Error::invalid_value(de::Unexpected::Unsigned(choice as u64), &self)
            })
        }
    }

    impl<'de, const N: usize> Deserialize<'de> for Choice<N> {
        fn deserialize<D>(deserializer: D) -> Result<Choice<N>, D::Error>
        where
            D: Deserializer<'de>,
        {
            let visitor: ChoiceVisitor<N> = ChoiceVisitor::default();
            deserializer.deserialize_u8(visitor)
        }
    }
}
