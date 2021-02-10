# time ./main.exe tree example/maccarthy91.ml
result: (K6ast.IntVal 91)
________________________________________________________
Executed in    2.56 millis    fish           external
   usr time    2.55 millis  333.00 micros    2.22 millis
   sys time    0.07 millis   69.00 micros    0.00 millis


# time ./main.exe cam example/maccarthy91.ml
result: (Cam.Int 91)
________________________________________________________
Executed in    2.62 millis    fish           external
   usr time    0.28 millis  279.00 micros    0.00 millis
   sys time    2.38 millis   56.00 micros    2.32 millis


# time ./main.exe zam example/maccarthy91.ml
result: (Zam.Int 91)
________________________________________________________
Executed in    1.73 millis    fish           external
   usr time    2.42 millis  979.00 micros  1439.00 micros
   sys time    0.20 millis  196.00 micros    0.00 micros
