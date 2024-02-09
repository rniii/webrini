use std::process::Command;
use std::{fs, io};

fn main() -> io::Result<()> {
    println!("cargo:rerun-if-changed=client");
    println!("cargo:rerun-if-changed=css");

    fs::create_dir_all("static")?;

    job(Command::new("elm").args([
        "make",
        "client/Main.elm",
        "--optimize",
        "--output=dist/elm.js",
    ]))?;

    if cfg!(debug_assertions) {
        fs::write(
            "static/index.js",
            [fs::read("dist/elm.js")?, fs::read("client/index.js")?].join(&b';'),
        )?;

        fs::copy("css/index.css", "static/index.css")?;
    } else {
        job(Command::new("uglifyjs")
            .args(["dist/elm.js", "client/index.js"])
            .args(["--output", "static/index.js"])
            .args(["-c", "pure_getters,pure_funcs='[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9]',unsafe,unsafe_comps,unsafe_math,passes=8,module"])
            .args(["-m", "toplevel"]))?;

        job(Command::new("esbuild").args(["css/index.css", "--minify", "--outdir=static"]))?;
    }

    job(Command::new("zopfli").args(["static/index.js", "static/index.css"]))?;

    Ok(())
}

fn job(cmd: &mut Command) -> io::Result<()> {
    assert!(cmd.spawn()?.wait()?.success());

    Ok(())
}
