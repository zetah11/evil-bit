pub trait Pixel {
    fn get<const BITS: u8>(&self, x: usize, y: usize) -> u8;
}

pub fn draw<const BITS: u8, F>(width: usize, height: usize, f: F) -> Vec<Vec<(u8, u8, u8)>>
where
    F: Pixel,
{
    let mut res = Vec::with_capacity(height);

    for y in 0..height {
        let mut row = Vec::with_capacity(width);

        for x in 0..width {
            let byte = f.get::<BITS>(x, y);
            row.push(color(byte));
        }

        res.push(row)
    }

    res
}

impl<F: Fn(usize, usize) -> u8> Pixel for F {
    fn get<const BITS: u8>(&self, x: usize, y: usize) -> u8 {
        self(x, y)
    }
}

fn color(byte: u8) -> (u8, u8, u8) {
    (byte, byte, byte)
}
