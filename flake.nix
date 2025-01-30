{
  description = "OCaml Template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    {
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

        buildInputs = [
          ocamlPackages.findlib
          ocamlPackages.ppx_deriving
          ocamlPackages.ppx_inline_test
        ];

        nativeBuildInputs = with pkgs; [
          ocamlPackages.ocaml
          # the dune build system
          ocamlPackages.dune_3
          ocamlPackages.findlib
          # ocamlPackages.utop
          ocamlPackages.lsp
          ocamlPackages.ocamlformat
          ocamlPackages.ocp-indent
        ];
      in
      {
        packages = {
          default = ocamlPackages.buildDunePackage {
            pname = "template";
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
