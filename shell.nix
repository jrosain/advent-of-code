{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages;
    [ ocaml dune_3 ocamlformat_0_26_2 lp_solve ] ++
    ( with ocamlPackages;
      [ ocaml-lsp findlib ]);
}
