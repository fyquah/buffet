(**
 * Recipe
 *
 * This is based on the "A Recipe for controlling Lego using Lava"
 * from The Monad Reader Issue 7 (https://wiki.haskell.org/wikiupload/0/03/TMR-Issue7.pdf)
 *)

open Hardcaml
open Ocaml_edsl_kernel
open Instructions

module Expression : sig
  include module type of Signal

  val pp : Format.formatter -> t -> unit
end

include Base
include Ref         with type expr := Expression.t and type 'a t := 'a t
include While       with type expr := Expression.t and type 'a t := 'a t
include Join        with                               type 'a t := 'a t
include Conditional with type expr := Expression.t and type 'a t := 'a t

val compile : Reg_spec.t -> 'a t -> Signal.t -> (Signal.t, 'a) With_valid.t2
