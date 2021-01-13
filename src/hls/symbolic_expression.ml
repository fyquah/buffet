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

let width t = t.width

let one width       = { width; value = Constant (Bits.one width) }
let zero width      = { width; value = Constant (Bits.zero width) }
let of_int ~width x = { width; value = Constant (Bits.of_int ~width x) }
