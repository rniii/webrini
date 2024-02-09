use std::io::Write;
use std::process::{Command, Stdio};
use std::{fs, io};

fn main() -> io::Result<()> {
    println!("cargo:rerun-if-changed=client");
    println!("cargo:rerun-if-changed=css");

    fs::create_dir_all("static")?;

    job(Command::new("dune").arg("build"))?;

    pipe(
        Command::new("esbuild").args(["_build/default/output/client/client.js", "--bundle"]),
        Command::new("uglifyjs")
            .args([
                "-c",
                "pure_getters,unsafe,unsafe_comps,unsafe_math,passes=8",
            ])
            .args(["-m", "--mangle-props", r#"regex=/^(hd|tl|TAG|_\d+)$/"#])
            .args(["--output", "static/index.js"]),
    )?;

    job(Command::new("esbuild").args(["css/index.css", "--minify", "--outdir=static"]))?;

    job(Command::new("zopfli").args(["static/index.js", "static/index.css"]))?;

    Ok(())
}

fn job(cmd: &mut Command) -> io::Result<()> {
    assert!(cmd.spawn()?.wait()?.success());

    Ok(())
}

fn pipe(a: &mut Command, b: &mut Command) -> io::Result<()> {
    let out = a.output()?;
    assert!(out.status.success());

    let mut in_ = b.stdin(Stdio::piped()).spawn()?;
    in_.stdin.as_ref().unwrap().write_all(&out.stdout)?;
    assert!(in_.wait()?.success());

    Ok(())
}
