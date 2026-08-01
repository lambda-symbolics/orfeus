{
  description = "Orfeus Common Lisp RAW processor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      lisp = pkgs.sbcl.withPackages (packages: with packages; [
        cffi
        ironclad
      ]);
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          lisp
          cargo
          cmake
          exiftool
          fltk
          gnumake
          lcms2
          lensfun
          libjpeg_turbo
          libraw
          libtiff
          pkg-config
          rustc
        ];

        shellHook = ''
          export CL_SOURCE_REGISTRY="$PWD//:$PWD/../fltk-sun//:"
          export ORFEUS_FLTK_SOURCE="$PWD/../fltk-sun"
          export ORFEUS_NATIVE_LIBRARY="$PWD/native/target/release/liborfeus_native.so"
        '';
      };
    };
}
