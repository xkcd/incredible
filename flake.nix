{
  description = "incredible";

  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=09ad7aaffd920d5817fc56f77ab5ddd1628cbe08";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  nixConfig = {
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    extra-substituters = [
      "https://cache.iog.io"
    ];
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils, haskellNix, ... }:
    let
      frontendModule = system: import ./nix/incredible-frontend.nix (self.outputs.packages.${system}.incredible-client);
      serverModule = system: import ./nix/incredible-server.nix (self.outputs.packages.${system}."incredible:exe:incredible-server");
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system overlays;
                              inherit (haskellNix) config; };
      pkgs-unstable = import nixpkgs-unstable { inherit system overlays; };
      # TODO add prefetch-npm-deps to devShell
      incredible-client = script: pkgs-unstable.buildNpmPackage {
        pname = "incredible";
        version = "1.0.0";
        src = ./client;
        nodejs = pkgs-unstable.nodejs_20;
        npmDepsHash = "sha256-IXOXhgf2tpysiOa3B5faHF1IWZ2uwzGzwHS+Z5frqgI="; #nix run nixpkgs#prefetch-npm-deps client/package-lock.json
        npmBuildScript = script;
      };
    
      deploy = import ./nix/deploy.nix { inherit pkgs; };
      incredible-docs = pkgs.runCommand "incredible-docs-gen" {} ''
        mkdir $out
        ${flake.packages."incredible:exe:incredible-docs"}/bin/incredible-docs --output $out/incredible-docs.json
        ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate -i $out/incredible-docs.json -g html --skip-validate-spec 
        cp index.html $out/index.html
      '';
      overlays = [ haskellNix.overlay
        (final: prev: {
          incredible =
            final.haskell-nix.project' {
              src = ./.;
              index-state = "2024-03-16T23:00:16Z";
              compiler-nix-name = "ghc964";
              shell = {
                tools = {
                  cabal = {};
                  ghcid = {};
                  haskell-language-server = {};
                };
                withHoogle = true;
                buildInputs = with pkgs-unstable; [
                  nodejs_20 nodePackages.npm nodePackages.webpack nodePackages.webpack-cli
                  prefetch-npm-deps redis
                ];
              };

              modules =
                [{ enableLibraryProfiling = true;
                   enableProfiling = true;
                 }
                ];
            };
        })
      ];

      flake = pkgs.incredible.flake { };

    in nixpkgs.lib.recursiveUpdate flake {
      packages.incredible-client = incredible-client "build";
      packages.incredible-client-dev = incredible-client "build:dev";
      packages.incredible-digital-ocean = (pkgs.nixos {
        imports =  [
            (serverModule system)
            (frontendModule system)
            (import ./nix/incredible-digital-ocean.nix)
            (import ./nix/incredible-cfg.nix)
            (import ./nix/profiles/staging.nix)
          ];
      }).digitalOceanImage;
      packages.incredible-docs = incredible-docs;
      apps.deploy = {
        type = "app";
        program = "${deploy}/bin/deployScript";
      };
    }) // {
      nixosConfigurations =
      let system = "x86_64-linux";
      in {
        incredible-do-staging = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            (serverModule system)
            (frontendModule system)
            (import ./nix/incredible-digital-ocean.nix)
            (import ./nix/incredible-cfg.nix)
            (import ./nix/profiles/staging.nix)
          ];
        };

        incredible-vm = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            (serverModule system)
            (frontendModule  system)
            (import ./nix/incredible-qemu.nix)
            (import ./nix/incredible-cfg.nix)
            (import ./nix/profiles/staging.nix)
          ];
        };
      };
    };
}
