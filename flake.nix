{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        };
    in {
      devShells.${system}.default = pkgs.mkShellNoCC {
        packages = [
          pkgs.gcc-arm-embedded
          pkgs.openocd
          pkgs.minicom
          # I use STM32_Programmer_CLI to program the option bytes, but no Nix package provides it
        ];
      };
      
      packages.${system}.firmware = pkgs.stdenvNoCC.mkDerivation {
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
        '';
      };
    };
}
