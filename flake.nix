{
  inputs = {
    "nixos-24.11".url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-24.11" = import inputs."nixos-24.11" { inherit system; };
        };
        pkgs = nixpkgs."nixos-24.11";
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        packages = let

          inherit (pkgs.haskell.lib) dontCheck;

          makeTestConfiguration = let defaultPkgs = pkgs;
          in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
          let
            inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            hs = (pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = combineOverrides old [
                (packageSourceOverrides { grab = ./grab; })
                (packageSourceOverrides { grab-form = ./grab-form; })
                overrides
              ];

            }));
          in pkgs.symlinkJoin {
            name = "grab-${ghcVersion}";
            paths = [ hs.grab hs.grab-form ];
          };

        in rec {
          ghc-9-6 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc96";
          };
          ghc-9-8 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc98";
          };
          all = pkgs.symlinkJoin {
            name = "grab";
            paths = [ ghc-9-6 ghc-9-8 ];
          };
        };
      });
}
