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
    let libraw = pkg_config::Config::new()
        .probe("libraw")
        .expect("LibRaw is required; enter the Nix development shell");
    let mut bridge = cc::Build::new();
    bridge
        .cpp(true)
        .file("src/libraw_bridge.cpp")
        .flag_if_supported("-std=c++17");
    for include_path in &libraw.include_paths {
        bridge.include(include_path);
    }
    bridge.opt_level(2).compile("orfeus_libraw_bridge");
    for link_path in &libraw.link_paths {
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", link_path.display());
    }
    println!("cargo:rerun-if-changed=src/libraw_bridge.cpp");

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
