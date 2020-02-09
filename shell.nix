# based on/copied from https://gitlab-extern.itemis.de/web-engineering/akka/scalish/blob/master/shell.nix
let
  pkgs = import <nixpkgs> { };
  unstable = import <unstable> { };
in
pkgs.mkShell {
  name = "duplicity-utils";
  buildInputs = with pkgs; [
      # List packages that should be on the path
      # You can search for package names using nix-env -qaP | grep <name>
      unstable.racket
      gnupg
    ];
  }
