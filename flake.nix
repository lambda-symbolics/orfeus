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
          clippy
          cmake
          exiftool
          fltk_1_4
          gnumake
          lcms2
          lensfun
          libjpeg_turbo
          libtiff
          pkg-config
          rustc
          shaderc
          vulkan-loader
        ];

        shellHook = ''
          export CL_SOURCE_REGISTRY="$PWD//:$PWD/../lightfast//"
          export ORFEUS_LIGHTFAST_SOURCE="$PWD/../lightfast"
          export CARGO_TARGET_DIR="$PWD/native/target-nix"
          export ORFEUS_NATIVE_LIBRARY="$CARGO_TARGET_DIR/release/liborfeus_native.so"
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.vulkan-loader ]}:/run/opengl-driver/lib:/usr/lib:''${LD_LIBRARY_PATH:-}"
          if [ -z "''${VK_DRIVER_FILES:-}" ]; then
            # Expose every system ICD rather than the first one found: the
            # renderer ranks adapters itself and prefers an integrated GPU,
            # which measures faster than a discrete card for these
            # transfer-bound passes. Pinning one file hid the other adapters.
            orfeus_icd_files=""
            for icd_directory in /run/opengl-driver/share/vulkan/icd.d /usr/share/vulkan/icd.d /etc/vulkan/icd.d; do
              for icd_path in "$icd_directory"/*.json; do
                if [ -r "$icd_path" ]; then
                  case ":$orfeus_icd_files:" in
                    *":$icd_path:"*) ;;
                    *) orfeus_icd_files="''${orfeus_icd_files:+$orfeus_icd_files:}$icd_path" ;;
                  esac
                fi
              done
            done
            if [ -n "$orfeus_icd_files" ]; then
              export VK_DRIVER_FILES="$orfeus_icd_files"
            fi
            unset orfeus_icd_files
          fi
        '';
      };
    };
}
