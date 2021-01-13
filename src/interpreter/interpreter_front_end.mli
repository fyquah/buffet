open Core_kernel
open Ocaml_edsl_kernel
open Hardcaml

module Expression : sig
  type t =
    | Value     of Bits.t
    | Reference of Bits.t ref

  val value : t -> Bits.t

  include Instructions.Expression with type t := t
end

include module type of Instructions.Make(Expression)
include Loop
include Ref with type variable = Bits.t ref

val interpret : 'a t -> 'a
