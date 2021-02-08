type t =
    | LetCosure of Types.vid_t * Types.vid_t list
    | LetCall of Types.vid_t * Types.vid_t * Types.vid_t list
    | LetInt of Types.vid_t * int
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
          | _ -> failwith "unimplemented" )
        , id )
    | Typing.Never -> ([End], fresh_v ())
    | Typing.Var id -> ([], id)
    | t -> failwith @@ Printf.sprintf "unimplemented %s" @@ Typing.show t

let f ast = fst @@ g [] ast
