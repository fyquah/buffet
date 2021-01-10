open Hardcaml

type 'a t =
  { width : int
  ; value : 'a value
  }

and 'a value =
  | Constant  of Bits.t
  | Operation of 'a operation
  | Comparator of 'a comparator

and 'a operation =
  | Add of ('a * 'a)
  | Sub of ('a * 'a)

and 'a comparator = 
  | Lt of ('a * 'a)
  | Gt of ('a * 'a)
  | Eq of ('a * 'a)

val width : _ t -> int

val one  : int -> _ t
val zero : int -> _ t
val of_int : width: int -> int -> _ t
