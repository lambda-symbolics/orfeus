use std::env;
use std::path::PathBuf;
use std::process::Command;

const STAGES: [&str; 2] = ["tone_transfer", "camera_matrix"];

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR"));
    // Every stage includes the shared layout header, so changing it recompiles
    // all of them.
    println!("cargo:rerun-if-changed=shaders/stage.glsl");
    for stage in STAGES {
        let shader = format!("shaders/{stage}.comp");
        println!("cargo:rerun-if-changed={shader}");
        let output = out_dir.join(format!("{stage}.spv"));
        let status = Command::new("glslc")
            .args([
                "--target-env=vulkan1.1",
                "-O",
                "-I",
                "shaders",
                &shader,
                "-o",
            ])
            .arg(&output)
            .status()
            .expect("failed to run glslc; enter the Nix development shell");
        assert!(status.success(), "glslc failed for {shader}");
    }
}
