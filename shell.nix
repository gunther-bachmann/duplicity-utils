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
