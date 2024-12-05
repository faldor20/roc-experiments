{
  inputs = {
    nixpkgs.url = "nixpkgs-unstable";
    flakelight={url = "github:nix-community/flakelight";
    inputs.nixpkgs.follows="nixpkgs";
    };
  };
  outputs =
    { flakelight, ... }@inputs:
    flakelight ./. {
      inherit inputs;
      devShell = pkgs: {
        packages = with pkgs; [
          ocamlPackages.ocaml
          ocamlPackages.dune_3
          ocamlPackages.ocaml-lsp
          ocamlPackages.ppx_jane
          ocamlPackages.base
          ocamlPackages.stdio
          ocamlPackages.findlib
          ocamlPackages.ppx_deriving
          go
        ];

      };

    };
}
