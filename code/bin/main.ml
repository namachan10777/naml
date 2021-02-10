let () =
    let switch = Sys.argv.(1) in
    let fname = Sys.argv.(2) in
    let ch = open_in fname in
    let src = really_input_string ch (in_channel_length ch) in
    let ast = S8.K7top.parse_repl_string src in
    close_in ch ;
    Printf.printf "result: %s"
    @@
    match switch with
    | "zam" -> S8.Zam.show_val_t @@ S8.Zam.exec @@ S8.Compile_to_zam.compile ast
    | "cam" ->
        S8.Cam.show_value_t @@ S8.Cam.exec @@ S8.Compile_to_cam.compile [] ast
    | _ -> failwith @@ Printf.sprintf "unsupported switch %s\n" switch
