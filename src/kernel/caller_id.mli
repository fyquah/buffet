(* Small helper to find out who is the caller of a function *)
type t = Printexc.location option list [@@deriving sexp_of]

val get : unit -> t
