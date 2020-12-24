use crate::types::unary::*;
use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;
use thiserror::Error;

/// A `Choice` represents a selection between several protocols offered by [`offer!`](crate::offer).
///
/// It wraps an ordinary non-negative number, with the guarantee that it is *strictly less than* the
/// type level number `N`. Note that this means `Choice<Z>` is unrepresentable, because you cannot
/// choose something from a set of zero things.
///
/// Unless you are implementing a [`backend`](crate::backend), you do not need to interact with
/// `Choice`s directly. However, all backends must implement [`Transmit<'static, Choice<N>,
/// Val>`](crate::backend::Transmit) and [`Receive<Choice<N>>`](crate::backend::Receive) for all
/// `N` in order to support the [`Choose`](crate::Choose) and [`Offer`](crate::Offer)
/// constructs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Choice<N: Unary> {
    choice: u8,
    bound: PhantomData<N>,
}

// Choice<Z> is unconstructable, but for all N, 0 is a Choice<S<N>>:
impl<N: Unary> Default for Choice<S<N>> {
    fn default() -> Self {
        0.try_into()
            .expect("0 is in bounds for all non-zero-bounded `Choice`s")
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

impl<N: Unary> TryFrom<u8> for Choice<N> {
    type Error = OutOfBoundsChoiceError;

    fn try_from(choice: u8) -> Result<Self, Self::Error> {
        if (choice as usize) < N::VALUE {
            Ok(Choice {
                choice,
                bound: PhantomData,
            })
        } else {
            Err(OutOfBoundsChoiceError {
                choice,
                bound: N::VALUE,
            })
        }
    }
}

impl<N: Unary> Into<u8> for Choice<N> {
    fn into(self) -> u8 {
        self.choice
    }
}

// If the serde feature is enabled, do custom serialization for `Choice` that fails when receiving
// an out-of-bounds choice.
#[cfg(feature = "serde")]
mod serialization {
    use super::*;

    use serde_crate::{
        de::{self, Visitor},
        Deserialize, Deserializer, Serialize, Serializer,
    };

    impl<N: Unary> Serialize for Choice<N> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_u8(self.choice)
        }
    }

    #[derive(Debug, Clone, Copy)]
    struct ChoiceVisitor<N: Unary> {
        bound: PhantomData<N>,
    }

    impl<N: Unary> Default for ChoiceVisitor<N> {
        fn default() -> Self {
            ChoiceVisitor { bound: PhantomData }
        }
    }

    impl<'de, N: Unary> Visitor<'de> for ChoiceVisitor<N> {
        type Value = Choice<N>;

        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(
                f,
                "a non-negative integer strictly less than {}",
                N::VALUE.min(u8::MAX as usize)
            )?;
            if N::VALUE == 0 {
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

    impl<'de, N: Unary> Deserialize<'de> for Choice<N> {
        fn deserialize<D>(deserializer: D) -> Result<Choice<N>, D::Error>
        where
            D: Deserializer<'de>,
        {
            let visitor: ChoiceVisitor<N> = ChoiceVisitor::default();
            deserializer.deserialize_u8(visitor)
        }
    }
}
