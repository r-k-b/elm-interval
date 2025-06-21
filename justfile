# Just uses sh by default, which doesn't support passing rest args with "$@".
set shell := ["bash", "-uc"]

default:
    @just --list --justfile {{justfile()}}

check: check-format test
    # further Nix-based checks TBA

alias cf := check-format
check-format:
    npm run check-format

# Run elm-review. Passes extra args, like `--watch`.
alias cr := check-review
check-review *args='':
    elm-review $@

install:
    npm install

# Run unit tests. Passes extra args, like `--watch`.
[positional-arguments]
@test *args='': install
    elm-test $@

update-nix:
    nix flake update

update: update-nix
