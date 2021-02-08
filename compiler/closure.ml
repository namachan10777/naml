type t =
    | LetClosure of
        Types.vid_t * Types.vid_t list * t list * Types.vid_t * string
    | LetCall of Types.vid_t * Types.vid_t * Types.vid_t list
    | LetInt of Types.vid_t * int
    | Phi of Types.vid_t * Types.vid_t * Types.vid_t
    | Test of Types.vid_t * t list * t list
    | LetBool of Types.vid_t * bool
    | LetAccess of Types.vid_t * Types.vid_t
    | If of Types.vid_t * t list * t list
    | End
[@@deriving show]

type inst_t = t list [@@deriving show]

let count_v = ref 0

let fresh_v () =
    count_v := !count_v + 1 ;
    Types.VidSSA !count_v

let init () = count_v := 0

let rec g env = function
    | Typing.App (f, args) ->
        let f, f_id = g env f in
        let args, arg_ids = Util.unzip @@ List.map (g env) args in
        let ret_id = fresh_v () in
        ((LetCall (ret_id, f_id, arg_ids) :: f) @ List.concat args, ret_id)
    | Typing.Int i ->
        let id = fresh_v () in
        ([LetInt (id, i)], id)
    | Typing.Bool b ->
        let id = fresh_v () in
        ([LetBool (id, b)], id)
    | Typing.Let ([(Typing.PVar (id, _), def)], expr) ->
        let expr, _ = g env expr in
        ( ( expr
          @
          match g env def with
          | LetBool (_, b) :: rest, _ -> LetBool (id, b) :: rest
          | LetInt (_, i) :: rest, _ -> LetInt (id, i) :: rest
          | LetCall (_, f, args) :: rest, _ -> LetCall (id, f, args) :: rest
          | Phi (_, x, y) :: rest, _ -> Phi (id, x, y) :: rest
          | LetClosure (_, args, block, ret, label) :: rest, _ ->
              LetClosure (id, args, block, ret, label) :: rest
          | _ -> failwith "unimplemented" )
        , id )
    | Typing.LetRec ([(id, _, def)], expr) ->
        let expr, _ = g env expr in
        ( ( expr
          @
          match g env def with
          | LetBool (_, b) :: rest, _ -> LetBool (id, b) :: rest
          | LetInt (_, i) :: rest, _ -> LetInt (id, i) :: rest
          | LetCall (_, f, args) :: rest, _ -> LetCall (id, f, args) :: rest
          | Phi (_, x, y) :: rest, _ -> Phi (id, x, y) :: rest
          | LetClosure (_, args, block, ret, label) :: rest, _ ->
              LetClosure (id, args, block, ret, label) :: rest
          | x, _ ->
              failwith @@ Printf.sprintf "unimplemented %s" @@ show_inst_t x )
        , id )
    | Typing.If (cond, then_e, else_e) ->
        let cond, id = g env cond in
        let then_e, id_then = g env then_e in
        let else_e, id_else = g env else_e in
        let ret_id = fresh_v () in
        ( Phi (ret_id, id_then, id_else) :: Test (id, then_e, else_e) :: cond
        , ret_id )
    | Typing.Never -> ([End], fresh_v ())
    | Typing.Var id -> ([], id)
    (* クロージャ変換しようね *)
    | Typing.Fun (args, body, _, label, _) ->
        let id = fresh_v () in
        let body, ret_id = g env body in
        ([LetClosure (id, List.map fst args, List.rev body, ret_id, label)], id)
    | t -> failwith @@ Printf.sprintf "unimplemented %s" @@ Typing.show t

let f ast = List.rev @@ fst @@ g [] ast
