open Core_kernel
open Hardcaml

type 'a t =
  { width : int
  ; value : 'a value
  }
[@@deriving equal, sexp_of]

and 'a value =
  | Constant  of Bits.t
  | Operation of 'a operation
  | Comparator of 'a comparator
  | Mux of 'a mux
  | Not of 'a
  | Select of 'a select
  | Concat_msb of 'a list

and 'a operation =
  | Add  of ('a * 'a)
  | Sub  of ('a * 'a)
  | Mult of ('a * 'a) 
  | And  of ('a * 'a)
  | Or   of ('a * 'a)
  | Xor  of ('a * 'a)
  | Multu  of ('a * 'a)
  | Mults  of ('a * 'a)

and 'a mux =
  { select : 'a
  ; cases  : 'a list
  }

and 'a comparator = 
  | Lt of ('a * 'a)
  | Gt of ('a * 'a)
  | Eq of ('a * 'a)

and 'a select =
  { lo : int
  ; hi : int
  ; arg : 'a
  }

let width t = t.width

let one width       = { width; value = Constant (Bits.one width) }
let zero width      = { width; value = Constant (Bits.zero width) }
let of_int ~width x = { width; value = Constant (Bits.of_int ~width x) }
