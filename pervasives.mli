val list_id: Id.t
val ref_id: Id.t
val array_id: Id.t
val names: Id.t list
val vars: Types.scheme_t Env.t
val ctors: ((int * Types.t list) * (int * Types.t)) Env.t
val types: Types.scheme_t Env.t
