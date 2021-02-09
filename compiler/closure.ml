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

type vars_t = Types.vid_t list [@@deriving show]

type inst_t = t list [@@deriving show]

let count_v = ref 0

let fresh_v () =
    count_v := !count_v + 1 ;
    Types.VidSSA !count_v

let init () = count_v := 0

let list_free_variables env t =
    let rec f = function
        | Typing.Let ([(Typing.PVar (id, _), def)], expr) ->
            List.filter (( <> ) id) @@ f expr
        | Typing.Var id -> [id]
        | Typing.App (g, args) -> f g @ List.concat (List.map f args)
        | Typing.Fun (args, body, ty, label, p) ->
            let defs = List.map fst args in
            List.filter (fun id -> List.for_all (( <> ) id) defs) @@ f body
        | Typing.Int _ -> []
        | Typing.Bool _ -> []
        | Typing.If (c, e1, e2) -> f c @ f e1 @ f e2
        | _ -> failwith "list free vairables unimplemented"
    in
    List.filter (fun id -> List.for_all (( <> ) id) env) @@ f t

let rec replace_variables map =
    let replace id =
        let rec f id = function
            | (id', y) :: _ when id = id' -> y
            | _ :: remain -> f id remain
            | [] -> id
        in
        f id map
    in
    function
    | Typing.App (f, args) ->
        Typing.App
          (replace_variables map f, List.map (replace_variables map) args)
    | Typing.Fun (args, body, ty, label, p) ->
        Typing.Fun
          ( List.map (fun (arg, ty) -> (replace arg, ty)) args
          , replace_variables map body
          , ty
          , label
          , p )
    | Typing.If (c, e1, e2) ->
        Typing.If
          ( replace_variables map c
          , replace_variables map e1
          , replace_variables map e2 )
    | Typing.Var id -> Typing.Var (replace id)
    | Typing.Int i -> Typing.Int i
    | Typing.Bool b -> Typing.Bool b
    | _ -> failwith "repalce_vairables unimplemented"

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
        let expr, _ = g (id :: env) expr in
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
        let expr, _ = g (id :: env) expr in
        ( ( expr
          @
          match g (id :: env) def with
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
    | Typing.Fun (args, body, _, label, _) as self ->
        let let_id = fresh_v () in
        ( ( match list_free_variables env self with
          | [] ->
              let body, ret_id = g env body in
              [ LetClosure
                  (let_id, List.map fst args, List.rev body, ret_id, label) ]
          | frees ->
              let map = List.map (fun free -> (free, fresh_v ())) frees in
              let body = replace_variables map body in
              let body, ret_id = g env body in
              let closure_id = fresh_v () in
              let closure =
                  LetClosure
                    ( closure_id
                    , List.map snd map @ List.map fst args
                    , List.rev body
                    , ret_id
                    , label )
              in
              let let_call = LetCall (let_id, closure_id, List.map fst map) in
              [let_call; closure] )
        , let_id )
    | t -> failwith @@ Printf.sprintf "unimplemented %s" @@ Typing.show t

let f ast =
    List.rev @@ fst
    @@ g (List.map (fun (_, id, _) -> id) Types.pervasive_vals) ast
