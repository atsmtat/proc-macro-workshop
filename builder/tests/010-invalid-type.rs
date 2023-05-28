// Ensure that macro reports a reasonable error message when the
// caller use it for a type which is not supported.

use derive_builder::Builder;

#[derive(Builder)]
pub struct Id(u32);

#[derive(Builder)]
pub struct Unit();

#[derive(Builder)]
pub enum Command {
    Left(u32),
    Right(u32),
}

fn main() {}
