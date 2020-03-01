# based on/copied from https://gitlab-extern.itemis.de/web-engineering/akka/scalish/blob/master/shell.nix
let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  name = "duplicity-utils";
  buildInputs = with pkgs; [
    racket
    gnupg
  ];
}
