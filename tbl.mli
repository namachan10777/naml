type ('a, 'b) t
val empty: ('a, 'b) t
val lookup: 'a -> ('a, 'b) t -> 'b option
val expect: string -> 'b option -> 'b
val make: ('a * 'b) list -> ('a, 'b) t
val push: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val map: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
type ('a, 'b) mut_t
val lookup_mut: 'a -> ('a, 'b) mut_t -> 'b option
val push_mut: 'a -> 'b -> ('a, 'b) mut_t -> unit
val make_mut: ('a * 'b) list -> ('a, 'b) mut_t
val concat: ('a, 'b) t list -> ('a, 'b) t
