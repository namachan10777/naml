type 'a t

exception NotFound

val get: 'a t -> Id.t -> 'a option
val get_unwrap: 'a t -> Id.t -> 'a
val push: 'a t -> Id.t -> 'a -> 'a t
val make: (Id.t * 'a) list -> 'a t
val alpha_env: 'a t -> (string list, Id.t) Tbl.t
val alpha_var_env: 'a t -> (string list, (Id.t * bool)) Tbl.t
val map: ('a -> 'b) -> 'a t -> 'b t
val concat: 'a t list -> 'a t
val names: 'a t -> Id.t list
val enclose_module: 'a t -> 'a t -> string list -> 'a t
val pp: (Ppx_deriving_runtime.Format.formatter -> 'a -> Ppx_deriving_runtime.unit)
            -> Ppx_deriving_runtime.Format.formatter
            ->'a t
            -> Ppx_deriving_runtime.unit
