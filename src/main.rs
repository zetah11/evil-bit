mod bitvecto;
mod draw;
mod expr;
mod simplify;

use std::io::Write;
use std::path::PathBuf;

use argh::FromArgs;
use image::RgbImage;
use rand::{thread_rng, Rng};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::bitvecto::width_needed_for;

use self::draw::draw;
use self::expr::Expr;
use self::simplify::simplify;

/// An abstract value representing the maximum complexity of the generated
/// expressions. A higher value means longer and more deeply nested expressions.
const COMPLEXITY: usize = 160;

/// The width (in pixels) of the resulting image.
const WIDTH: usize = 30;

/// The height (in pixels) of the resulting image.
const HEIGHT: usize = 30;

/// Whether to render the unsimplified expression as well. Useful when creating
/// new simplification rules - if the rules are correct and the simplifier works
/// as it should, then the two images should be identical.
const BEFORE: bool = false;

/// Generate images with random math!
#[derive(FromArgs)]
struct Args {
    /// saves the generated image to the given path instead of printing it to
    /// stdout.
    #[argh(option, short = 'i')]
    image: Option<PathBuf>,
}

const BITS: u8 = {
    let w_bits = width_needed_for(WIDTH);
    let h_bits = width_needed_for(HEIGHT);

    if w_bits > h_bits {
        w_bits
    } else {
        h_bits
    }
};

fn main() {
    let args: Args = argh::from_env();

    println!("{BITS} bit image");

    let mut data = vec![0u8; COMPLEXITY];
    thread_rng().fill(&mut data[..]);

    let expr = Expr::create_arbitrary::<BITS>(&data);
    let before = expr.clone();
    println!("simplifying {expr}");
    let expr = simplify::<BITS>(expr);
    println!("       into {expr}");

    if let Some(path) = &args.image {
        let image = write_to_image(expr);
        image.save(path).unwrap();
    } else {
        write_to_stdout(before, expr)
    }
}

#[must_use]
fn write_to_image(expr: Expr) -> RgbImage {
    let mut image = RgbImage::new(WIDTH as u32, HEIGHT as u32);
    let drawing = draw::<BITS, _>(WIDTH, HEIGHT, expr);

    for (pix, (r, g, b)) in image.pixels_mut().zip(drawing.into_iter().flatten()) {
        pix.0 = [r, g, b];
    }

    image
}

fn write_to_stdout(before: Expr, after: Expr) {
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

    let drawing = draw::<BITS, _>(WIDTH, HEIGHT, after);

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
