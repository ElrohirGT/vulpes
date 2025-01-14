{
  description = "Vulpes lang flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    zig2nix.url = "github:Cloudef/zig2nix/";
  };

  outputs = {
    self,
    nixpkgs,
    zig2nix,
  }: let
    # System types to support.
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];

    # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    # Nixpkgs instantiated for supported system types.
    nixpkgsFor = forAllSystems (system: import nixpkgs {inherit system;});

    # Zig env for every system.
    zenvFor = forAllSystems (system: zig2nix.zig-env.${system} {zig = nixpkgsFor.${system}.zig;});
  in {
    devShells = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
    in {
      default = pkgs.mkShell {
        packages = [pkgs.zig];
      };
    });

    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      zenv = zenvFor.${system};
      system-triple = zenv.lib.zigTripleFromString system;
    in rec {
      # Cross compilation for a specific target!
      # Usage:
      # nix build .#target.x86_64-linux-gnu
      target = pkgs.lib.genAttrs zenv.lib.allTargetTriples (target:
        zenv.packageForTarget target ({
            src = pkgs.lib.cleanSource ./.;
            nativeBuildInputs = with zenv.pkgs; [];
            buildInputs = with zenv.pkgsForTarget target; [];

            # Smaller binaries and avoids shipping glibc.
            zigPreferMusl = true;

            # This disables LD_LIBRARY_PATH mangling, binary patching etc...
            # The package won't be usable inside nix.
            zigDisableWrap = true;
          }
          // pkgs.lib.optionalAttrs (!builtins.pathExists ./build.zig.zon) {
            pname = "my-zig-project";
            version = "0.0.0";
          }));

      # Compile for current target!
      default = target.${system-triple}.override {
        # Prefer nix friendly settings.
        zigPreferMusl = false;
        zigDisableWrap = false;
      };
    });
  };
}
