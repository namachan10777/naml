type opr_t = R of int | M of int | F of int [@@deriving show]

type reg_t = Reg of int
[@@deriving show]
type mem_t = Mem of int
[@@deriving show]

exception Internal

type t =
    | Test of reg_t * mem_t * t list * t list
    | Ldi of reg_t * int
    | Ldb of reg_t * bool
    | Load of reg_t * mem_t
    | Save of mem_t * reg_t
    (* label *)
    | MkClosure of reg_t * string
    (* reg * mems *)
    | Call of reg_t * reg_t * mem_t list
    | App of reg_t * reg_t * mem_t list
    | CallTop of reg_t * int * mem_t list
    | AppTop of reg_t * int * mem_t list
[@@deriving show]

type insts_t = (string * t list) list [@@deriving show]

let rec vid2stack cnt = function
    | Closure.LetApp (id, _, _) :: remain ->
        (id, M cnt) :: vid2stack (cnt + 1) remain
    | Closure.LetBool (id, _) :: remain ->
        (id, M cnt) :: vid2stack (cnt + 1) remain
    | Closure.LetInt (id, _) :: remain ->
        (id, M cnt) :: vid2stack (cnt + 1) remain
    | Closure.LetClosure (id, _, _, _, _, _) :: remain ->
        (id, M cnt) :: vid2stack (cnt + 1) remain
    | Closure.LetCall (id, _, _) :: remain ->
        (id, M cnt) :: vid2stack (cnt + 1) remain
    | Closure.Phi (id, _, _) :: remain ->
        (id, M cnt) :: vid2stack (cnt + 1) remain
    | Closure.Test (_, block1, block2) :: remain ->
        let ids1 = vid2stack (cnt) block1 in
        let ids2 = vid2stack (cnt + List.length ids1) block2 in
        ids1 @ ids2 @ vid2stack (cnt + List.length ids1 + List.length ids2) remain
    | Closure.End :: _ -> []
    | [] -> []

type stackmap_t = (Types.vid_t * opr_t) list [@@deriving show]

let rec g stackmap =
    let lookup id =
        let rec f = function
            | (id', stack) :: _ when id = id' -> stack
            | _ :: remain -> f remain
            | [] ->
                Printf.printf "%s in %s\n" (Types.show_vid_t id)
                  (show_stackmap_t stackmap) ;
                raise Internal
        in
        f stackmap
    in
    let lookup_mem id = match lookup id with
         | M m -> Mem m
         | _ -> failwith "mem required"
    in
    function
    | Closure.LetInt (mem, i) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks,  Ldi (Reg 0, i) ::Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.LetBool (mem, b) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, Ldb (Reg 0, b) :: Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.LetCall (mem, Types.VidTop f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, CallTop (Reg 0, f, List.map lookup_mem args) ::Save (lookup_mem mem, Reg 0) ::  insts)
    | Closure.LetApp (mem, Types.VidTop f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, AppTop (Reg 0, f, List.map lookup_mem args) ::Save (lookup_mem mem, Reg 0) ::  insts)
    | Closure.LetCall (mem, f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, Load (Reg 1, lookup_mem f) :: Call (Reg 0, Reg 1, List.map lookup_mem args) :: Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.LetApp (mem, f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, Load (Reg 1, lookup_mem f) :: App (Reg 0, Reg 1, List.map lookup_mem args) :: Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.LetClosure (mem, args, inner, ret, label, pre_applied) :: remain
      ->
        let args_on_stack = List.mapi (fun i id -> (id, M i)) args in
        let stackmap_inner =  
              (args_on_stack @ vid2stack (List.length args_on_stack) inner) in
        Printf.printf "%s stackmap %s\n" (Types.show_vid_t mem) (show_stackmap_t stackmap_inner);
        let blocks, inner =
            g
            stackmap_inner
              inner
        in
        let blocks', insts = g stackmap remain in
        (((label, inner) :: blocks) @ blocks', MkClosure (Reg 0, label) :: Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.Test (cond, block1, block2) :: Closure.Phi (ret, r1, r2) :: remain
      ->
        let blocks1, inner1 = g stackmap block1 in
        let blocks2, inner2 = g stackmap block2 in
        let blocks, insts = g stackmap remain in
        let ret = lookup_mem ret in
        let r1 = lookup_mem r1 in
        let r2 = lookup_mem r2 in
        let inner1 = Save (ret, Reg 0) :: Load (Reg 0, r1) :: inner1 in
        let inner2 = Save (ret, Reg 0) :: Load (Reg 0, r2) :: inner2 in
        ( blocks1 @ blocks2 @ blocks
        , Load (Reg 0, lookup_mem cond) :: Test (Reg 0, ret, inner1, inner2) :: insts )
    | [] -> ([], [])
    | [Closure.End] -> ([], [])
    | x ->
        failwith @@ Printf.sprintf "invalid state %s" @@ Closure.show_inst_t x

let f clos =
    let stackmap = vid2stack 0 clos in
    let blocks, main_inner = g stackmap clos in
    ("main", main_inner) :: blocks
