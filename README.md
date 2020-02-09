# duplicity-utils

Tooling around duplicity (http://duplicity.nongnu.org/) to streamline backup creation and lifecycle.


## development

Project development makes extensive use of emacs (https://www.gnu.org/software/emacs/), racket (https://racket-lang.org/), nix (https://nixos.org/nix/) and direnv (https://direnv.net/).

Run tests within emacs using racket-mode (https://github.com/greghendershott/racket-mode) via `C-c C-t`.
Run all tests from cli via `raco test .` in the project root.
