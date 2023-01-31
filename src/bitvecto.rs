use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Not, Rem};

/// Compute the number of bits needed to store any unsigned value less than
/// `value`.
pub const fn width_needed_for(value: usize) -> u8 {
    let bits = usize::BITS - (value - 1).leading_zeros();
    if bits > u8::MAX as u32 {
        unreachable!()
    } else {
        bits as u8
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Bitvector<const BITS: u8> {
    // The actual data in this bitvector. An invariant that must be upheld is
    // that `data <= mask` (i.e. data is never *not* masked).
    data: usize,
}

impl<const BITS: u8> Bitvector<BITS> {
    const MASK: usize = (1usize << BITS) - 1;

    pub fn rotate_left(self, rhs: Self) -> Self {
        let amount = rhs.data % BITS as usize;
        Self {
            data: (self.data >> amount) | ((self.data << amount) & Self::MASK),
        }
    }

    pub fn rotate_right(self, rhs: Self) -> Self {
        let amount = rhs.data % BITS as usize;
        Self {
            data: (self.data << amount) | ((self.data >> amount) & Self::MASK),
        }
    }
}

impl<const BITS: u8> From<usize> for Bitvector<BITS> {
    fn from(value: usize) -> Self {
        Self {
            data: value & Self::MASK,
        }
    }
}

impl<const BITS: u8> From<Bitvector<BITS>> for usize {
    fn from(value: Bitvector<BITS>) -> Self {
        value.data
    }
}

impl<const BITS: u8> BitAnd for Bitvector<BITS> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            data: (self.data & rhs.data) & Self::MASK,
        }
    }
}

impl<const BITS: u8> BitOr for Bitvector<BITS> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            data: (self.data | rhs.data) & Self::MASK,
        }
    }
}

impl<const BITS: u8> BitXor for Bitvector<BITS> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self {
            data: (self.data ^ rhs.data) & Self::MASK,
        }
    }
}

impl<const BITS: u8> Not for Bitvector<BITS> {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self {
            data: (!self.data) & Self::MASK,
        }
    }
}

impl<const BITS: u8> Add for Bitvector<BITS> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            #[allow(clippy::suspicious_arithmetic_impl)]
            data: self.data.wrapping_add(rhs.data) & Self::MASK,
        }
    }
}

impl<const BITS: u8> Mul for Bitvector<BITS> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            #[allow(clippy::suspicious_arithmetic_impl)]
            data: self.data.wrapping_mul(rhs.data) & Self::MASK,
        }
    }
}

impl<const BITS: u8> Rem for Bitvector<BITS> {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self {
            data: if rhs.data == 0 {
                0
            } else {
                self.data.rem_euclid(rhs.data) & Self::MASK
            },
        }
    }
}
