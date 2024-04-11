{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    mt19937-hs.url = "github:raehik/mt19937-hs";
    mt19937-hs.flake = false;
    binrep.url = "github:raehik/binrep";
    binrep.flake = false;
    bytezap.url = "github:raehik/bytezap";
    bytezap.flake = false;
    flatparse.url = "github:AndrasKovacs/flatparse";
    flatparse.flake = false;
    strongweak.url   = "github:raehik/strongweak";
    strongweak.flake = false;
    generic-data-functions.url   = "github:raehik/generic-data-functions";
    generic-data-functions.flake = false;
    generic-type-asserts.url   = "github:raehik/generic-type-asserts";
    generic-type-asserts.flake = false;
    generic-type-functions.url   = "github:raehik/generic-type-functions";
    generic-type-functions.flake = false;
  };
  outputs = inputs:
  let
    # simple devshell for non-dev compilers: really just want `cabal repl`
    nondevDevShell = compiler: {
      mkShellArgs.name = "${compiler}-frkps2-hs";
      hoogle = false;
      tools = _: {
        hlint = null;
        haskell-language-server = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-frkps2-hs;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc98 = {
          # shouldn't work, pkgs aren't up to date and mine aren't 9.8 ready
          basePackages = pkgs.haskell.packages.ghc98;
          packages.mt19937.source = inputs.mt19937-hs;
          packages.binrep.source = inputs.binrep;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          devShell = nondevDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.mt19937.source = inputs.mt19937-hs;
          packages.binrep.source = inputs.binrep;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          devShell.mkShellArgs.name = "ghc96-frkps2-hs";
          devShell.tools = _: {
            haskell-language-server = null; # 2024-03-06: broken
          };
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages.mt19937.source = inputs.mt19937-hs;
          packages.binrep.source = inputs.binrep;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          devShell = nondevDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          packages.mt19937.source = inputs.mt19937-hs;
          packages.binrep.source = inputs.binrep;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          devShell = nondevDevShell "ghc92";
        };
      };
    };
}
