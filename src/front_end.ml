open Core_kernel

module type Instruction = sig
  type ('a, 'b) t
end

module type Program = sig
  module Instruction : Instruction

  type 'a t =
    | Return : 'a                                        -> 'a t
    | Then   : (('a, 'a t) Instruction.t * ('a -> 'b t)) -> 'b t

  include Monad.S with type 'a t := 'a t
end

module type Expression = sig
  type t

  val zero : int -> t
  val one  : int -> t
  val of_int : width: int -> int -> t

  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
end

module type Ref = sig
  type expression
  type 'a program
  type 'a container

  type t

  val create : expression -> t container program
  val get    : t -> expression container program
  val set    : t -> expression container         -> unit program
end

module type Loop = sig
  type expression
  type 'a program

  val for_ : expression -> expression -> (expression -> unit program) -> unit program
end

module type Automatic_storage = sig
  module Program : Program
  module Expression : Expression

  module Ref : Ref
    with type expression := Expression.t
     and type 'a program := 'a Program.t
     and type 'a container := 'a

  (*
  module Ref : Ref with type 'a container := 'a

  module Make_ref(Container : Hardcaml.Interface.S) :
    Ref with type 'a container := 'a Container.t
     *)
end
