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

let evaluate (type a b) (module Comb : Comb.S with type t = a) (t : b t) ~eval_child =
  match t.value with
  | Constant constant ->
    Comb.of_constant (Hardcaml.Bits.to_constant constant)

  | Operation (Add (lhs, rhs)) ->
    Comb.(+:) (eval_child lhs) (eval_child rhs)

  | Operation (Sub (lhs, rhs)) ->
    Comb.(-:) (eval_child lhs) (eval_child rhs)

  | Operation (Mult (lhs, rhs)) ->
    Comb.( *: ) (eval_child lhs) (eval_child rhs)

  | Operation (And (lhs, rhs)) ->
    Comb.( &: ) (eval_child lhs) (eval_child rhs)

  | Operation (Or (lhs, rhs)) ->
    Comb.( |: ) (eval_child lhs) (eval_child rhs)

  | Operation (Xor (lhs, rhs)) ->
    Comb.( ^: ) (eval_child lhs) (eval_child rhs)

  | Operation (Multu (lhs, rhs)) ->
    Comb.( *: ) (eval_child lhs) (eval_child rhs)

  | Operation (Mults (lhs, rhs)) ->
    Comb.( *+ ) (eval_child lhs) (eval_child rhs)

  | Comparator (Lt (lhs, rhs)) ->
    Comb.( <: ) (eval_child lhs) (eval_child rhs)

  | Comparator (Eq (lhs, rhs)) ->
    Comb.( ==: ) (eval_child lhs) (eval_child rhs)

  | Comparator (Gt (lhs, rhs)) ->
    Comb.( >: ) (eval_child lhs) (eval_child rhs)

  | Mux { select; cases } ->
    let select = eval_child select in
    let cases = List.map cases ~f:eval_child in
    Comb.mux select cases

  | Not arg -> Comb.( ~: ) (eval_child arg)

  | Select { lo; hi; arg } ->
    Comb.select (eval_child arg) hi lo

  | Concat_msb args ->
    Comb.concat_msb (List.map args ~f:eval_child)
;;
