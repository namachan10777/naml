#!/bin/sh

for f in *.ml; do
	ocamlformat -i $f
done
