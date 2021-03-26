type 'a t

exception NotFound

val get: 'a t -> Id.t -> 'a option
val get_unwrap: 'a t -> Id.t -> 'a
val push: 'a t -> Id.t -> 'a
