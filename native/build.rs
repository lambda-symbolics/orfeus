use std::env;
use std::path::PathBuf;
use std::process::Command;

const STAGES: [&str; 6] = [
    "tone_transfer",
    "nr_ycbcr",
    "nr_bilateral",
    "nr_median",
    "nr_combine",
    "nr_conv",
];

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR"));
    // Every stage includes the shared layout header, so changing it recompiles
    // all of them.
    println!("cargo:rerun-if-changed=shaders/stage.glsl");
    for stage in STAGES {
        let shader = format!("shaders/{stage}.comp");
        println!("cargo:rerun-if-changed={shader}");
        let output = out_dir.join(format!("{stage}.spv"));
        let status = match Command::new("glslc")
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
        {
            // Where shaderc is not installed, glslang's own compiler builds
            // the same SPIR-V, without glslc's optimisation pass.
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                Command::new("glslangValidator")
                    .args(["-V", "--target-env", "vulkan1.1", "-Ishaders", &shader, "-o"])
                    .arg(&output)
                    .status()
                    .expect("failed to run glslc or glslangValidator; enter the Nix development shell")
            }
            result => result.expect("failed to run glslc; enter the Nix development shell"),
        };
        assert!(status.success(), "compiling {shader} failed");
    }
}
