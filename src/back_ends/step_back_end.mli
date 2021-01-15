open Hardcaml
open Buffet_kernel
open Instructions

module Step_monad := Digital_components.Step_monad

module Expression : sig
  include Dynamic_expression.S
    with type reference := Bits.t ref
     and type underlying := Bits.t

  val evaluate : t -> Bits.t

  val pp : out_channel -> t -> unit
end

include Base
include Ref         with type expr := Expression.t and type 'a t := 'a t
include While       with type expr := Expression.t and type 'a t := 'a t
include Conditional with type expr := Expression.t and type 'a t := 'a t
include Join        with                               type 'a t := 'a t

val run : 'a t -> 'a
