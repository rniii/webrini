use std::io::{Result, Write};
use std::process::{Command, Stdio};

fn main() -> Result<()> {
    println!("cargo:rerun-if-changed=web");

    Command::new("elm")
        .args(["make", "web/Main.elm", "--optimize", "--output=dist/elm.js"])
        .spawn()?
        .wait()?;

    let compress = Command::new("uglifyjs")
        .args(["dist/elm.js", "web/index.js"])
        .args(["-c", "pure_getters,pure_funcs='[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9]',unsafe,unsafe_comps,passes=8,module"])
        .output()?;

    let mangle = Command::new("uglifyjs")
        .args(["-m", "toplevel", "--output", "static/dist/index.js"])
        .stdin(Stdio::piped())
        .spawn()?;

    mangle.stdin.unwrap().write_all(&compress.stdout)
}
