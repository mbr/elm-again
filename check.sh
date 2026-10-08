#!/bin/sh
# Check formatting, public API documentation, and retry behavior.
set -eu

cd "$(dirname "$0")"

./format.sh --check
mkdir -p elm-stuff
elm make --docs=elm-stuff/docs.json
elm-test
