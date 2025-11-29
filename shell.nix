{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages;
    [ ocaml dune_3 ocamlPackages.ocaml-lsp ocamlformat_0_26_2 ];
}
