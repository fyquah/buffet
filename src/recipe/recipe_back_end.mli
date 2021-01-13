open Hardcaml
open Ocaml_edsl_kernel
open Instructions

module Expression = Signal

include Base
(* include Loop with type expr := Expression.t and type 'a t := 'a t *)
include Ref with type expr := Expression.t and type 'a t := 'a t

val compile : Reg_spec.t -> 'a t -> Signal.t -> (Signal.t, 'a) With_valid.t2
