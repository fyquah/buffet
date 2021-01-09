open Core_kernel
open Hardcaml

module Expression : sig
  type t =
    | Value     of Bits.t
    | Reference of Bits.t ref

  val value : t -> Bits.t

  include Front_end.Expression with type t := t
end

include module type of Front_end.Make(Expression)
include Loop
include Ref with type variable = Bits.t ref

val interpret : 'a t -> 'a
