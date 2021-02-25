type inst_t =
    | Ldi of int
    | Ldb of bool
    | Lds of string
    | Access of int
    | Closure of code_t
    | Let
    | EndLet
    | Test of code_t * code_t
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Gret
    | Less
    | Eq
    | Neq
    | App
    | Not
    | TailApp
    | PushMark
    | Grab
    | Return
    | Drop
[@@deriving show]

and code_t = inst_t list [@@deriving show]

type val_t =
    | Int of int
    | Bool of bool
    | Str of string
    | ClosVal of code_t * env_t
    | Eps
[@@deriving show]

and env_t = val_t list

and stack_t = val_t list

type zam_t = {code: code_t; env: env_t; astack: stack_t; rstack: stack_t}
[@@deriving show]

let rec exec zam =
    let bin_i_i op =
        match zam.astack with
        | Int lhr :: Int rhr :: astack ->
            exec
              { zam with
                code= List.tl zam.code
              ; astack= Int (op lhr rhr) :: astack }
        | _ -> failwith "cannot execute op: int -> int -> int"
    in
    let bin_i_b op =
        match zam.astack with
        | Int lhr :: Int rhr :: astack ->
            exec
              { zam with
                code= List.tl zam.code
              ; astack= Bool (op lhr rhr) :: astack }
        | _ -> failwith "cannot execute op: int -> int -> bool"
    in
    match zam.code with
    | [] -> (
      match zam with
      | {code= []; rstack= []; astack= [v]; env= []} -> v
      | _ -> failwith "invalid state" )
    | Ldi i :: code -> exec {zam with code; astack= Int i :: zam.astack}
    | Ldb b :: code -> exec {zam with code; astack= Bool b :: zam.astack}
    | Lds s :: code -> exec {zam with code; astack= Str s :: zam.astack}
    | Access addr :: code ->
        exec {zam with code; astack= List.nth zam.env addr :: zam.astack}
    | EndLet :: code -> exec {zam with code; env= List.tl zam.env}
    | Let :: code ->
        exec
          { zam with
            code
          ; env= List.hd zam.astack :: zam.env
          ; astack= List.tl zam.astack }
    | Closure code' :: code ->
        exec {zam with code; astack= ClosVal (code', zam.env) :: zam.astack}
    | App :: code -> (
      match zam.astack with
      | (ClosVal (code', env') as c) :: v :: astack ->
          exec
            { code= code'
            ; astack
            ; env= v :: c :: env'
            ; rstack= ClosVal (code, zam.env) :: zam.rstack }
      | _ ->  failwith "cannot execute app" )
    | TailApp :: _ -> (
      match zam.astack with
      | (ClosVal (code', env') as c) :: v :: astack ->
          exec {zam with code= code'; astack; env= v :: c :: env'}
      | _ -> failwith "cannot execute app" )
    | Grab :: code -> (
      match (zam.astack, zam.rstack) with
      | Eps :: astack, ClosVal (code', env') :: rstack ->
          exec
            { code= code'
            ; astack= ClosVal (code, zam.env) :: astack
            ; env= env'
            ; rstack }
      | v :: astack, _ -> exec {zam with code; astack; env= v :: ClosVal(code, zam.env) :: zam.env}
      | _ -> failwith "cannot execute grab" )
    | Return :: _ -> (
      match (zam.astack, zam.rstack) with
      | v :: Eps :: astack, ClosVal (code', env') :: rstack ->
          exec {code= code'; env= env'; astack= v :: astack; rstack}
      | ClosVal(code', env') :: v :: astack, rstack ->
          exec {code= code'; env= v :: ClosVal(code', env') :: env'; astack; rstack}
      | _, _ -> failwith "cannot execute return" )
    | PushMark :: code -> exec {zam with code; astack= Eps :: zam.astack}
    | Test (c1, c2) :: code -> (
      match List.hd zam.astack with
      | Bool true -> exec {zam with code= c1 @ code; astack= List.tl zam.astack}
      | Bool false -> exec {zam with code= c2 @ code; astack= List.tl zam.astack}
      | _ -> failwith "test needs boolean value as condition" )
    | Add :: _ -> bin_i_i ( + )
    | Sub :: _ -> bin_i_i ( - )
    | Mul :: _ -> bin_i_i ( * )
    | Div :: _ -> bin_i_i ( / )
    | Mod :: _ -> bin_i_i ( mod )
    | Gret :: _ -> bin_i_b ( > )
    | Less :: _ -> bin_i_b ( < )
    | Eq :: code -> (
      match zam.astack with
      | Bool lhr :: Bool rhr :: astack ->
          exec {zam with code; astack= Bool (lhr = rhr) :: astack}
      | Int lhr :: Int rhr :: astack ->
          exec {zam with code; astack= Bool (lhr = rhr) :: astack}
      | Str lhr :: Str rhr :: astack ->
          exec {zam with code; astack= Bool (lhr = rhr) :: astack}
      | _ -> failwith "cannot execute op: a -> a -> bool" )
    | Neq :: code -> (
      match zam.astack with
      | Bool lhr :: Bool rhr :: astack ->
          exec {zam with code; astack= Bool (lhr = rhr) :: astack}
      | Int lhr :: Int rhr :: astack ->
          exec {zam with code; astack= Bool (lhr = rhr) :: astack}
      | Str lhr :: Str rhr :: astack ->
          exec {zam with code; astack= Bool (lhr = rhr) :: astack}
      | _ -> failwith "cannot execute op: a -> a -> bool" )
    | Not :: code -> (
      match zam.astack with
      | Bool b :: astack -> exec {zam with code; astack= Bool (not b) :: astack}
      | _ -> failwith "cannot execute op: a -> a -> bool" )
    | Drop :: code -> exec {zam with code; astack= List.tl zam.astack}
