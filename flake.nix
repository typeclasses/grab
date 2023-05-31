{
  inputs = {
    "nixos-22.11".url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "ascii-char";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-22.11" = import inputs."nixos-22.11" { inherit system; };
        };
        pkgs = nixpkgs."nixos-22.11";
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          testConfigurations = let

            inherit (pkgs.haskell.lib) dontCheck;

            makeTestConfiguration = let defaultPkgs = pkgs;
            in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            hs = (pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = combineOverrides old [
                (packageSourceOverrides { grab = ./grab; })
                (packageSourceOverrides { grab-form = ./grab-form; })
                overrides
              ];

            }));
            in pkgs.symlinkJoin { name = "grab-${ghcVersion}"; paths = [ hs.grab hs.grab-form ]; };

          in rec {
            ghc-9-0 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc90";
            };
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc92";
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc94";
            };
            all = pkgs.symlinkJoin {
              name = "grab";
              paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ];
            };
          };
        };
      });
}
