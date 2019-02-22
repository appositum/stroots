#!/bin/sh

nix-shell --run "cabal v1-run && cabal v1-repl lib:stroots"
