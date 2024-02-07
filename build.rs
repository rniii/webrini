use std::io::prelude::*;
use std::process::{Command, Stdio};
use std::{fs, io};

fn main() -> io::Result<()> {
    println!("cargo:rerun-if-changed=client");

    fs::create_dir_all("static")?;

    let elm = Command::new("elm")
        .args([
            "make",
            "client/Main.elm",
            "--optimize",
            "--output=dist/elm.js",
        ])
        .spawn()?
        .wait()?;

    assert!(elm.success());

    let compress = Command::new("uglifyjs")
        .args(["dist/elm.js", "client/index.js"])
        .args(["-c", "pure_getters,pure_funcs='[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9]',unsafe,unsafe_comps,passes=8,module"])
        .output()?;

    let mut mangle = Command::new("uglifyjs")
        .args(["-m", "toplevel", "--output", "static/index.js"])
        .stdin(Stdio::piped())
        .spawn()?;

    mangle.stdin.as_ref().unwrap().write_all(&compress.stdout)?;
    mangle.wait()?;

    Command::new("esbuild")
        .args(["css/index.css", "--minify", "--outdir=static"])
        .spawn()?
        .wait()?;

    Command::new("zopfli")
        .args(["static/index.js", "static/index.css"])
        .spawn()?
        .wait()?;

    Ok(())
}
