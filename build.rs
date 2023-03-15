use std::fs;

fn main() {
    println!("cargo:rerun-if-changed=bdwgc");
    let mut cfg = cmake::Config::new("bdwgc");
    // Build Boehm GC static library
    cfg.target("x86_64-pc-windows-gnu")
        .profile("Release")
        .define("BUILD_SHARED_LIBS", "OFF");
    let mut lib = cfg.build();
    // Copy `.a` static lib next to executable
    let mut dst = lib.clone();
    dst.pop();
    dst.pop();
    dst.pop();
    dst.push("libgc.a");
    lib.push("build/libgc.a");
    fs::copy(lib, dst).unwrap();
}
