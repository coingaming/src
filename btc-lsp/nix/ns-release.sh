#!/bin/sh

set -e
set -x

THIS_DIR="$(dirname "$(realpath "$0")")"

echo "$THIS_DIR/$1"
ls -la "$THIS_DIR/$1"

nix-build \
  "$THIS_DIR/$1" \
  --option extra-substituters "https://cache.nixos.org https://hydra.iohk.io https://all-hies.cachix.org" \
  --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
