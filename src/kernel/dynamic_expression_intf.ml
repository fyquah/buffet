open Hardcaml

module type S = sig
  type underlying

  include Comb.S

  val value : t -> underlying
end

module type Dynamic_expression = sig
  module Make(Underlying : Comb.Primitives) : S with type underlying := Underlying.t
end
