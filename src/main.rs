mod bitvecto;
mod draw;
mod expr;
mod simplify;

use std::io::Write;

use rand::{thread_rng, Rng};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::bitvecto::width_needed_for;

use self::draw::draw;
use self::expr::Expr;
use self::simplify::simplify;

/// An abstract value representing the maximum complexity of the generated
/// expressions. A higher value means longer and more deeply nested expressions.
const COMPLEXITY: usize = 50;

/// The width (in pixels) of the resulting image.
const WIDTH: usize = 30;

/// The height (in pixels) of the resulting image.
const HEIGHT: usize = 30;

/// Whether to render the unsimplified expression as well. Useful when creating
/// new simplification rules - if the rules are correct and the simplifier works
/// as it should, then the two images should be identical.
const BEFORE: bool = false;

fn main() {
    const BITS: u8 = {
        let w_bits = width_needed_for(WIDTH);
        let h_bits = width_needed_for(HEIGHT);

        if w_bits > h_bits {
            w_bits
        } else {
            h_bits
        }
    };

    println!("{BITS} bit image");

    let mut data = vec![0u8; COMPLEXITY];
    thread_rng().fill(&mut data[..]);

    let expr = Expr::create_arbitrary::<BITS>(&data);
    let before = expr.clone();
    println!("simplifying {expr}");
    let expr = simplify::<BITS>(expr);
    println!("       into {expr}");

    let mut stream = StandardStream::stdout(ColorChoice::Auto);

    if BEFORE {
        let drawing = draw::<BITS, _>(WIDTH, HEIGHT, before);
        for row in drawing {
            for (r, g, b) in row {
                stream
                    .set_color(ColorSpec::new().set_bg(Some(Color::Rgb(r, g, b))))
                    .unwrap();

                stream.write_all(b"  ").unwrap();
            }

            stream.reset().unwrap();
            stream.write_all(b"\n").unwrap();
        }

        stream.write_all(b"becomes\n").unwrap();
    }

    let drawing = draw::<BITS, _>(WIDTH, HEIGHT, expr);

    for row in drawing {
        for (r, g, b) in row {
            stream
                .set_color(ColorSpec::new().set_bg(Some(Color::Rgb(r, g, b))))
                .unwrap();

            stream.write_all(b"  ").unwrap();
        }

        stream.reset().unwrap();
        stream.write_all(b"\n").unwrap();
    }
}
