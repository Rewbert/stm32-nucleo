{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    # External dependencies
    mhs = {
      url = "github:Rewbert/MicroHs/cf793b46c22328bff7e2a5f94f6c53f1d1ef8b88";
      flake = false;
    };
    ssm-runtime = {
      url = "github:Rewbert/ssm-runtime/b7781c610797c069e066f4e8836db4d9e1f4911a";
      flake = false;
    };
    nor-udb = {
      url = "github:Rewbert/nor-udb/825b3a19977788045526b3e4e49050bb7f82586b";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, mhs, ssm-runtime, nor-udb }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        };

      mhsBuilt = pkgs.stdenv.mkDerivation {
        pname = "mhs";
        version = "cf793b4";
        src = mhs;

        buildPhase = ''
          make bin/mhs
        '';

        installPhase = ''
          mkdir -p $out
          cp -r bin $out/
          cp -r src $out/
          cp -r lib $out/
          cp mhs.conf $out/
        '';
      };
    in {
      devShells.${system}.default = pkgs.mkShellNoCC {
        packages = [
          pkgs.gcc-arm-embedded
          pkgs.openocd
          pkgs.minicom
          # I use STM32_Programmer_CLI to program the option bytes, but no Nix package provides it
        ];

        MHS_ROOT = mhsBuilt;
        SSM_DIR = ssm-runtime;
        NOR_UDB_DIR = nor-udb;
      };

      packages.${system} = {
        mhs = mhsBuilt;

        firmware = pkgs.stdenvNoCC.mkDerivation {
          pname = "stm32l552zeq nucleo firmware";
          version = "1.0";

          src = self;

          nativeBuildInputs = [
            pkgs.gcc-arm-embedded
            pkgs.gnumake
          ];

          buildPhase = ''
            make
          '';

          installPhase = ''
            mkdir -p $out
            cp main.elf $out/
            cp mhs.elf $out/
            cp secure.elf $out/
            cp nonsecure.elf $out/
            cp secure-dc.elf $out/
            cp nonsecure-dc.elf $out/
          '';
        };
      };
    };
}
