#!/bin/bash

nix-shell --run "source ~/.bashrc; cabal v1-repl lib:stroots"
