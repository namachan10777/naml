(* ここはad-hocとカスの実装の塊で、マジで動くというただそれだけでしか無いです *)
(* Regは適当な関数呼び出しに関わりなさそうな汎用レジスタ *)

type opr_t = R of int | M of int | F of int [@@deriving show]

type reg_t = Reg of int [@@deriving show]

type mem_t = Mem of int [@@deriving show]

exception Internal

type t =
    | Test of reg_t * mem_t * t list * t list
    | Ldi of reg_t * int
    | Ldb of reg_t * bool
    | Load of reg_t * mem_t
    | Save of mem_t * reg_t
    (* label *)
    | MkClosure of reg_t * string * int * mem_t list * mem_t
    (* reg * mems *)
    | Call of reg_t * reg_t * mem_t list
    | App of reg_t * reg_t * mem_t list
    | CallTop of reg_t * int * mem_t list
    | AppTop of reg_t * int * mem_t list
[@@deriving show]

type insts_t = (string * int * int * t list * mem_t) list [@@deriving show]

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
        let ids1 = vid2stack cnt block1 in
        let ids2 = vid2stack (cnt + List.length ids1) block2 in
        ids1 @ ids2
        @ vid2stack (cnt + List.length ids1 + List.length ids2) remain
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
    let lookup_mem id =
        match lookup id with M m -> Mem m | _ -> failwith "mem required"
    in
    function
    | Closure.LetInt (mem, i) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, Ldi (Reg 0, i) :: Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.LetBool (mem, b) :: remain ->
        let blocks, insts = g stackmap remain in
        (blocks, Ldb (Reg 0, b) :: Save (lookup_mem mem, Reg 0) :: insts)
    | Closure.LetCall (mem, Types.VidTop f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        ( blocks
        , CallTop (Reg 0, f, List.map lookup_mem args)
          :: Save (lookup_mem mem, Reg 0)
          :: insts )
    | Closure.LetApp (mem, Types.VidTop f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        ( blocks
        , AppTop (Reg 0, f, List.map lookup_mem args)
          :: Save (lookup_mem mem, Reg 0)
          :: insts )
    | Closure.LetCall (mem, f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        ( blocks
        , Load (Reg 1, lookup_mem f)
          :: Call (Reg 0, Reg 1, List.map lookup_mem args)
          :: Save (lookup_mem mem, Reg 0)
          :: insts )
    | Closure.LetApp (mem, f, args) :: remain ->
        let blocks, insts = g stackmap remain in
        ( blocks
        , Load (Reg 1, lookup_mem f)
          :: App (Reg 0, Reg 1, List.map lookup_mem args)
          :: Save (lookup_mem mem, Reg 0)
          :: insts )
    | Closure.LetClosure (mem, args, inner, ret, label, pre_applied) :: remain
      ->
        let args_on_stack = List.mapi (fun i id -> (id, M i)) args in
        let stackmap_inner =
            args_on_stack @ vid2stack (List.length args_on_stack) inner
        in
        Printf.printf "%s stackmap %s\n" (Types.show_vid_t mem)
          (show_stackmap_t stackmap_inner) ;
        let blocks, inner = g stackmap_inner inner in
        let blocks', insts = g stackmap remain in
        let n_args = List.length args + List.length pre_applied in
        let insts =
            match pre_applied with
            | [] ->
                MkClosure (Reg 0, label, n_args, [], lookup_mem ret)
                :: Save (lookup_mem mem, Reg 0)
                :: insts
            | pre_applied ->
                MkClosure
                  ( Reg 0
                  , label
                  , n_args
                  , List.map lookup_mem pre_applied
                  , lookup_mem ret )
                :: Save (lookup_mem mem, Reg 0)
                :: insts
        in
        (((label, n_args, List.length stackmap, inner, lookup_mem ret) :: blocks) @ blocks', insts)
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
        , Load (Reg 0, lookup_mem cond)
          :: Test (Reg 0, ret, inner1, inner2)
          :: insts )
    | [] -> ([], [])
    | [Closure.End] -> ([], [])
    | x ->
        failwith @@ Printf.sprintf "invalid state %s" @@ Closure.show_inst_t x

let code2reg = function
    | 0 -> Emit.Rax
    | 1 -> Emit.Rbx
    | n -> failwith @@ Printf.sprintf "resister overflow %d" n

let arg_reg = function
    | 0 -> Emit.Rdi
    | 1 -> Emit.Rsi
    | 2 -> Emit.Rdx
    | 3 -> Emit.Rcx
    | 4 -> Emit.R8
    | 5 -> Emit.R9
    | _ -> failwith "too many arguments"

module E = Emit

let prepare_args = []
let cnt = ref 0
let fresh_label () =
    cnt := !cnt + 1;
    ".L" ^ string_of_int !cnt

let rec codegen = function
    | Load (Reg r, Mem m) :: remain ->
        E.I (E.Movq (E.Ind (E.Rbp, Some (-8 * m)), E.Reg (code2reg r)))
        :: codegen remain
    | Save (Mem m, Reg r) :: remain ->
        E.C "save"
        :: E.I (E.Movq (E.Reg (code2reg r), E.Ind (E.Rbp, Some (-8 * m))))
        :: codegen remain
    | MkClosure (Reg r, label, size, args, Mem ret) :: remain ->
        let alloc_container =
            [ E.I (E.Movq (E.Imm (2 + size), E.Reg E.Edi))
            ; E.I (E.Call (E.Label "malloc@PLT"))
            ; E.I (E.Movq (E.IndL (E.Rip, Some label), E.Ind (E.Rax, None)))
            ; E.I (E.Movq (E.Imm (List.length args), E.Ind (E.Rax, Some 8))) ]
        in
        let copy_args =
            List.mapi
              (fun i (Mem m) ->
                [E.I (E.Movq (E.Ind (E.Rbp, Some (-8 * m)), E.Ind (E.Rax, Some i)))])
              args
        in
        alloc_container @ List.concat copy_args @ codegen remain @ [E.I (E.Movq (E.Ind (E.Rbp, Some(-ret)), E.Reg E.Rax))]
    | Call (ret, Reg f, args) :: remain ->
        let calc_arg_addr =
            [ E.I (E.Movq (E.Ind (code2reg f, Some 8), E.Reg E.Rbx))
            ; E.I (E.Addq (E.Reg (code2reg f), E.Reg E.Rbx))
            ; E.I (E.Addq (E.Imm (2*8), E.Reg E.Rbp)) ]
        in
        let copy_args =
            List.concat
            @@ List.mapi
                 (fun i (Mem m) ->
                   [ E.I
                       (E.Movq (E.Ind (E.Rbp, Some (-8*m)), E.Ind (E.Rbx, Some i)))
                   ])
                 args
        in
        let call =
            [ E.I (E.Movq (E.Ind (code2reg f, None), E.Reg E.Rax))
            ; E.I (E.Call (E.Reg E.Rax)) ]
        in
        calc_arg_addr @ copy_args @ call @ codegen remain
    | CallTop (Reg r, 0, [Mem lhr; Mem rhr]) :: remain ->
        E.I (E.Movq (E.Ind (E.Rbp, Some (-8 * lhr)), E.Reg (code2reg r)))
        :: E.I (E.Addq (E.Ind (E.Rbp, Some (-8 * lhr)), E.Reg (code2reg r)))
        :: codegen remain
    | CallTop (Reg r, 1, [Mem lhr; Mem rhr]) :: remain ->
        E.I (E.Movq (E.Ind (E.Rbp, Some (-8 * lhr)), E.Reg (code2reg r)))
        :: E.I (E.Subq (E.Ind (E.Rbp, Some (-8 * lhr)), E.Reg (code2reg r)))
        :: codegen remain
    (* 実装サボってます。CtorやTupleを比較するために再帰的に比較する必要があるんですが、実装サボってます。 *)
    | CallTop (Reg r, 7, [Mem lhr; Mem rhr]) :: remain ->
        E.I (E.Movq (E.Ind (E.Rbp, Some (-8 * lhr)), E.Reg (code2reg r)))
        :: E.C "calltop"
        :: E.I (E.Cmpq (E.Reg (code2reg r), E.Ind (E.Rbp, Some (-8 * lhr))))
        :: E.I (E.Sete (E.Reg E.Al))
        :: E.I (E.Movzbq (E.Reg E.Al, E.Reg (code2reg r)))
        :: codegen remain
    | CallTop (Reg r, 15, [Mem arg]) :: remain ->
        E.I (E.Movq (E.IndL(E.Rip,  Some ".print_int_s"), E.Reg E.Rdi))
        :: E.I (E.Movq (E.Ind (E.Rbp, Some (-8 * arg)), E.Reg E.Rsi))
        :: E.I (E.Call (E.Label "printf@PLT"))
        :: codegen remain
    | Test (Reg r, Mem ret, br1, br2) :: remain ->
        let else_l = fresh_label () in
        let goal_l = fresh_label () in
        E.I (E.Testq (E.Reg (code2reg r), E.Reg (code2reg r)))
        :: E.I (E.Jne else_l)
        :: (codegen br1)
        @ [E.L else_l]
        @ (codegen br2)
        @ [E.L goal_l]
        @ codegen remain
    | Ldb (Reg r, true) :: remain ->
        E.I (E.Movq (E.Imm 1, E.Reg (code2reg r))) :: codegen remain
    | Ldb (Reg r, false) :: remain ->
        E.I (E.Movq (E.Imm 0, E.Reg (code2reg r))) :: codegen remain
    | Ldi (Reg r, i) :: remain ->
        E.I (E.Movq (E.Imm i, E.Reg (code2reg r))) :: codegen remain
    | [] -> []
    | ir :: _ -> failwith @@ Printf.sprintf "unsupported ir %s\n" @@ show ir

let gen_blocks clos =
    let stackmap = vid2stack 3 clos in
    let store_return_code = [
        Ldi (Reg 0, 0);
        Save (Mem 2, Reg 0);
    ] in
    let blocks, main_inner = g stackmap clos in
    ("main", 2 , 3 + List.length stackmap, main_inner @ store_return_code, Mem 2) :: blocks

let f clos =
    let blocks = gen_blocks clos in
    let blocks = List.map (fun (label, n_args, n_vars, codes, Mem ret) ->
        let header = [
            E.D E.Text;
            E.D (E.Global label);
            E.D (E.Type (label, "function"));
            E.L label;
            E.I (E.Pushq (E.Reg E.Rbp));
            E.I (E.Movq (E.Reg E.Rsp, E.Reg E.Rbp));
            E.I (E.Pushq (E.Reg E.Rbx));
            E.I (E.Subq (E.Imm (8*n_vars), E.Reg E.Rsp));
        ] in
        let copy_args = List.init n_args (fun i -> E.I (E.Movq (E.Reg (arg_reg i), E.Ind (E.Rbp, Some (-8*i))))) in
        let footer = [
            E.I (E.Movq (E.Ind (E.Rbp, Some(-8*ret)), E.Reg E.Rax));
            E.I E.Leave;
            E.I E.Retq;
            E.L (fresh_label ());
            E.D (E.SizeSub (label, ".", label));
        ] in
        header @ copy_args @ codegen codes @ footer
    ) blocks
    |> List.concat in
    let preamble = [
        E.D (E.File "test.ml");
        E.D E.Text;
        E.D (E.Section (".rodata", None));
        E.L ".print_int_s";
        E.D (E.Asciz "%d");
    ] in
    let footer = [
        E.D (E.Section (".note.GNU-stack", Some ("", Some ("progbits", None))));
    ] in
    Emit.f @@ preamble @ blocks @ footer
