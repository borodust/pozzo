#!/usr/bin/env sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

exec sbcl --dynamic-space-size 4096 --script "$SCRIPT_DIR/pozzo.lisp" +pozzo --system 'pozzo/example' --wait --repl-server slynk +godot "$@"
