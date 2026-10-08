#!/bin/sh
# Refresh the Elm dependency snapshot used by Nix builds.
set -eu

cd "$(dirname "$0")"
temporary_directory=$(mktemp -d)
trap 'rm -rf "$temporary_directory"' 0
trap 'exit 1' HUP INT TERM

# elm2nix needs an application manifest; elm-test resolves the package's test graph.
elm-test make
version=$(elm-test --version)
cp "elm-stuff/generated-code/elm-community/elm-test/$version/elm.json" "$temporary_directory/"
(
    cd "$temporary_directory"
    elm2nix convert > elm-srcs.nix
    elm2nix snapshot
    nixfmt elm-srcs.nix
)
mv "$temporary_directory/elm-srcs.nix" elm-srcs.nix
mv "$temporary_directory/registry.dat" registry.dat
