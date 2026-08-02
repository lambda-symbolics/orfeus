use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let shader = "shaders/tone_transfer.comp";
    println!("cargo:rerun-if-changed={shader}");
    let output = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR")).join("tone_transfer.spv");
    let status = Command::new("glslc")
        .args(["--target-env=vulkan1.1", "-O", shader, "-o"])
        .arg(&output)
        .status()
        .expect("failed to run glslc; enter the Nix development shell");
    assert!(status.success(), "glslc failed for {shader}");
}
