# Incredible

[https://xkcd.com/2916](https://xkcd.com/2916)

## Development

There is a Nix shell defined in `flake.nix` that can be accessed with `nix develop`. This includes everything to build the client and the server.

### Frontend

The client is built with NodeJS 20.

A live development version can be started with `npm run start:dev` in `client/`.

Production JS can be built with `nix build .#incredible-client` or `npm run build` in `client/`.

### Backend

The server is built with GHC 9.6.4.

To build and run the webserver with Cabal, `cabal run incredible-server` or with Nix, `nix run .#incredible:exe:incredible-server`.

There is also an executable for generating puzzles, `cabal run incredible-gen` or with Nix, `nix run .#incredible:exe:incredible-gen`.

### VM

You can build a Nix VM with the frontend and backend built with the configuration in `/config` with `nixos-rebuild build-vm --flake .#incredible-vm`.

The VM can then be run with `./result/bin/run-nixos-vm`.
