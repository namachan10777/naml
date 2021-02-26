#!/bin/sh

cd $(dirname $0)

find . -type f -name '*.ml' | xargs ocamlformat -i $f
