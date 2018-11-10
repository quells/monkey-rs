extern crate monkey;
use monkey::repl;

fn main() {
    let debug_enabled = true;
    repl::parser(debug_enabled)
}
