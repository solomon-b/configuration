{
  description = "hkd-options-parsing";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/nixos-22.05;
    flake-utils.url = github:numtide/flake-utils;
    pre-commit-hooks.url = github:cachix/pre-commit-hooks.nix;
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
  }: let
    utils = flake-utils.lib;
  in
    utils.eachDefaultSystem (system: let
      compilerVersion = "ghc8107";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          hkd-options-parsing = hfinal.callCabal2nix "hkd-options-parsing" ./. {};
        };
      };
    in rec {
      packages =
        utils.flattenTree
        {hkd-options-parsing = hsPkgs.hkd-options-parsing;};

      # nix flake check
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            alejandra.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
          };
        };
      };

      # nix develop
      devShell = hsPkgs.shellFor {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        withHoogle = true;
        packages = p: [
          p.hkd-options-parsing
        ];
        buildInputs = with pkgs;
          [
            hsPkgs.haskell-language-server
            haskellPackages.cabal-install
            cabal2nix
            haskellPackages.ghcid
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt
            nodePackages.serve
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      };

      # nix build
      defaultPackage = packages.hkd-options-parsing;
    });
}
