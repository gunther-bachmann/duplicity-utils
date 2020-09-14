let
  pkgs = import <unstable> { };
in
pkgs.mkShell {
  name = "duplicity-utils";
  buildInputs = with pkgs; [
    racket
    gnupg
  ];
}
