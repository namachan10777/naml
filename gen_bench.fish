#!/usr/bin/fish
#
echo "compiler2 asmgen ./compiler/example/$argv[1].ml -o a.out"
./compiler2 asmgen ./compiler/example/$argv[1].ml -o $argv[1].out
echo "time a.out"
time ./$argv[1].out

echo "ocamlopt ./compiler/example/$argv[1].ml -o a.out"
ocamlopt ./compiler/example/$argv[1].ml -o $argv[1].out
echo "time a.out"
time ./$argv[1].out

echo "time ocaml ./compiler/example/$argv[1].ml"
time ocaml ./compiler/example/$argv[1].ml

echo "time compiler1 tree ./code/example/$argv[1].ml"
time ./compiler1 tree ./code/example/$argv[1].ml

echo "time compiler1 cam ./code/example/$argv[1].ml"
time ./compiler1 cam ./code/example/$argv[1].ml

echo "time compiler1 zam ./code/example/$argv[1].ml"
time ./compiler1 zam ./code/example/$argv[1].ml
