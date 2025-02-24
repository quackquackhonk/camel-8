{
  description = "OCaml Template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        ocamlPackages = pkgs.ocamlPackages;

        buildInputs = with pkgs; [
          ocamlPackages.findlib
          ocamlPackages.stdint
          ocamlPackages.ppxlib
          ocamlPackages.ppx_deriving
          ocamlPackages.alcotest
          ocamlPackages.lwt
          ocamlPackages.lambda-term
       ];

        nativeBuildInputs = with pkgs; [
          ocamlPackages.ocaml
          ocamlPackages.dune_3
          ocamlPackages.utop
          ocamlPackages.findlib
          ocamlPackages.ocaml-lsp
          ocamlPackages.ocamlformat
          ocamlPackages.ocp-indent
        ];
      in
      {
        packages = {
          default = ocamlPackages.buildDunePackage {
            pname = "camel8";
            version = "0.0.0";
            duneVersion = "3";
            src = ./.;

            strictDeps = true;

            inherit nativeBuildInputs buildInputs;
          };
        };

        devShells.default = pkgs.mkShell { inherit nativeBuildInputs buildInputs; };
      }
    );
}
