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
    };
}
