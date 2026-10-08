#!/bin/sh
# Format Elm and Nix sources, or verify their formatting.
set -eu

cd "$(dirname "$0")"

case "${1:-}" in
    "")
        nixfmt flake.nix elm-srcs.nix
        elm-format --yes src tests
        ;;
    --check)
        nixfmt --check flake.nix elm-srcs.nix
        elm-format --validate src tests
        ;;
    *)
        echo "usage: $0 [--check]" >&2
        exit 2
        ;;
esac
