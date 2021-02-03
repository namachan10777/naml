#!/bin/sh

find . -type f -name '*.ml' | xargs ocamlformat -i $f
