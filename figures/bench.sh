time ./main.exe tree example/fib.ml
result: (K6ast.IntVal 14930352)
________________________________________________________
Executed in    2.71 secs   fish           external
   usr time    2.70 secs  516.00 micros    2.70 secs
   sys time    0.01 secs   96.00 micros    0.01 secs


time ./main.exe cam example/fib.ml
result: (Cam.Int 14930352)
________________________________________________________
Executed in    1.83 secs   fish           external
   usr time  1824.63 millis  452.00 micros  1824.17 millis
   sys time    3.41 millis   84.00 micros    3.33 millis


time ./main.exe zam example/fib.ml
result: (Zam.Int 14930352)
________________________________________________________
Executed in    2.70 secs   fish           external
   usr time    2.69 secs  422.00 micros    2.69 secs
   sys time    0.00 secs   78.00 micros    0.00 secs
