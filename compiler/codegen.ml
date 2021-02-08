type reg_t =
    | Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsi
    | Rdi
    | Rbp
    | Rsp
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15

type directive_t =
    | Text
    | File of string
    | Global of string
    | P2align of int * int
    | Type of string * string
    | Asciz of string
    | Section of string * (string * (string * int option) option) option
    | Size of string * int
    | SizeSub of string * string * string

type operand_t = Imm of int | Reg of reg_t | Ind of reg_t * int option

type inst_t =
    | Pushq of operand_t
    | Popq of operand_t
    | Movq of operand_t * operand_t
    | Subq of operand_t * operand_t
    | Addq of operand_t * operand_t
    | Movl of operand_t * operand_t
    | Cmpl of operand_t * operand_t
    | Je of string
    | Jne of string
    | Jmp of string
    | Callq of string
    | Retq

type t = D of directive_t | L of string | I of inst_t

let print_reg = function
    | Rax -> "%rax"
    | Rbx -> "%rbx"
    | Rcx -> "%rcx"
    | Rdx -> "%rdx"
    | Rsi -> "%rsi"
    | Rdi -> "%rdi"
    | Rbp -> "%rbp"
    | Rsp -> "%rsp"
    | R8 -> "%r8"
    | R9 -> "%r9"
    | R10 -> "%r10"
    | R11 -> "%r11"
    | R12 -> "%r12"
    | R13 -> "%r13"
    | R14 -> "%r14"
    | R15 -> "%r15"

let print_operand = function
    | Imm i -> "$" ^ string_of_int i
    | Reg r -> print_reg r
    | Ind (r, None) -> Printf.sprintf "(%s)" @@ print_reg r
    | Ind (r, Some offset) -> Printf.sprintf "%d(%s)" offset @@ print_reg r

let print_directive = function
    | Text -> ".text"
    | File f -> Printf.sprintf ".file\t\"%s\"" f
    | Global n -> Printf.sprintf ".globl\t%s" n
    | P2align (align, op) -> Printf.sprintf ".p2align\t%d, 0x%x" align op
    | Type (l, ty) -> Printf.sprintf ".type\t%s,@%s" l ty
    | Asciz s -> Printf.sprintf ".asciz\t\"%s\"" s
    | Section (name, None) -> Printf.sprintf ".section\t%s" name
    | Section (name, Some (flags, None)) ->
        Printf.sprintf ".section\t%s,\"%s\"" name flags
    | Section (name, Some (flags, Some (attrs, None))) ->
        Printf.sprintf ".section\t%s,\"%s\",@%s" name flags attrs
    | Section (name, Some (flags, Some (attrs, Some opt))) ->
        Printf.sprintf ".section\t%s,\"%s\",@%s,%d" name flags attrs opt
    | Size (name, s) -> Printf.sprintf ".size\t%s, %d" name s
    | SizeSub (name, a, b) -> Printf.sprintf ".size\t%s, %s-%s" name a b

let print_inst = function
    | Pushq op -> Printf.sprintf "pushq\t%s" @@ print_operand op
    | Je label -> Printf.sprintf "je\t%s" label
    | Jne label -> Printf.sprintf "jne\t%s" label
    | Jmp label -> Printf.sprintf "jmp\t%s" label
    | Callq label -> Printf.sprintf "callq\t%s" label
    | Retq -> "retq"
    | Popq op -> Printf.sprintf "popq\t%s" @@ print_operand op
    | Movq (src, dest) ->
        Printf.sprintf "movq\t%s, %s" (print_operand src) (print_operand dest)
    | Movl (src, dest) ->
        Printf.sprintf "movl\t%s, %s" (print_operand src) (print_operand dest)
    | Subq (src, dest) ->
        Printf.sprintf "subq\t%s, %s" (print_operand src) (print_operand dest)
    | Addq (src, dest) ->
        Printf.sprintf "addq\t%s, %s" (print_operand src) (print_operand dest)
    | Cmpl (src, dest) ->
        Printf.sprintf "cmpl\t%s, %s" (print_operand src) (print_operand dest)

let print = function
    | D d -> "\t" ^ print_directive d
    | L l -> l ^ ":"
    | I i -> "\t" ^ print_inst i

let f _ =
    let insts = [D Text; D (File "test.ml")] in
    List.fold_left (fun acc i -> acc ^ print i ^ "\n") "" insts
