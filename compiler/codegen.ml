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
    | Section of string
    | Size of string * string
    | SizeSub of string * string * string

type operand_t = Imm of int | Reg of reg_t | Ind of reg_t * int option

type inst_t =
    | Pushq of operand_t
    | Popq of operand_t
    | Movq of operand_t
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
