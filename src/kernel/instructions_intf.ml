open Core_kernel

module type Base = sig
  include Monad.S

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let+) : 'a t -> ('a -> 'b)   -> 'b t
end

module type Expression = sig
  type t

  include Hardcaml.Comb.S with type t := t

  (** [pp] pretty-prints an output expression.  *)
  val pp : Out_channel.t -> t -> unit
end

module type Variable = sig
  type expr
  type t

  val get_ref : t -> expr
end

(** API for Refs and Variable value storage *)
module type Ref = sig
  type variable
  type expr
  type 'a t

  (** [new_ref] creates a new variable (the terms variable and ref are interchangable).
      In hardware, this is synthesized into.
   *)
  val new_ref : expr -> variable t

  (** [get_ref] deferences a ref to get the underlying expression. Note that this does
      not obtained a value, but rather an expression, meaning that the value is only
      guranteed to be the same on a given clock cycle.

      Note that [get_ref] does not return a [expr t], but rather just [expr]. This makes
      writing expressions like: [get_ref foo +: get_ref var] much more pleasant without
      having to go through monad binding nightmares.
   *)
  val get_ref : variable -> expr

  (** [set_ref] sets the value of a variable to a given expression's value at the given
      cycle. This takes one clock cycle. To do parallel assignments (multiple assignments
      in a clock cycle), see [par].
   *)
  val set_ref : variable -> expr -> unit t
end

(** API for parallel compositions. *)
module type Join = sig
  type 'a t

  (** [join_], [join2], [join3] *)
  val join_ : 'a t list -> 'a list t
  val join2 : 'a t -> 'b t         -> ('a * 'b) t
  val join3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** [par*] functions are similar to the [join] functions, except they discard the result
      upon completion.
   *)
  val par   : unit t list -> unit t
  val par2  : unit t -> unit t           -> unit t
  val par3  : unit t -> unit t -> unit t -> unit t
end

(** API for basic control flow. *)
module type Control_flow = sig
  type expr
  type 'a t

  (** Combinatorial conditional statement for expressions. *)
  val if_ : expr -> expr t -> expr t -> expr t

  (** [while_ cond body] executes the [body] until [cond]'s value resolves to false. *)
  val while_ : expr -> unit t -> unit t
end

(** API for debugging statements *)
module type Debugging = sig
  type 'a t

  (** [debugf "%a" Expression.pp Expression.gnd] prints debugging output to stdout or
      simply performs a no-op, depending on the backend's implementation.

      Due to limitations in the type-system, this is not presently encoded as an instruction,
      but rather just an API call that implementations are expected to implement.
   *)
  val debugf : ('r, Out_channel.t, unit, unit t) format4 -> 'r
end

module type Instructions = sig
  module type Base = Base
  module type Expression = Expression
  module type Variable = Variable

  module type Ref = Ref
  module type Join = Join
  module type Control_flow = Control_flow
  module type Debugging = Debugging

  module Make(Expression : Expression) : sig
    type expr = Expression.t

    type 'a instruction = ..

    type 'a t =
      | Return : 'a                              -> 'a t
      | Then   : ('a instruction * ('a -> 'b t)) -> 'b t

    include Monad.S with type 'a t := 'a t

    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
    val (let+) : 'a t -> ('a -> 'b)   -> 'b t

    module type Variable    = Variable with type expr := Expression.t

    (** Base line module types to build minimal sequential logic in hardware. *)
    module type Ref          = Ref          with type expr := expr and type 'a t := 'a t
    module type Join         = Join         with                       type 'a t := 'a t
    module type Control_flow = Control_flow with type expr := expr and type 'a t := 'a t

    (** Modules that do not have semantical meaning in hardware, but useful to have around. *)
    module type Debugging   = Debugging   with type 'a t := 'a t

    module Ref(Variable : Variable) : sig
      type 'a instruction +=
        | New_ref : Expression.t                -> Variable.t   instruction
        | Set_ref : (Variable.t * Expression.t) -> unit         instruction

      include Ref
    end

    module Join() : sig
      type 'a instruction += 
        | Join : 'a t list -> 'a list instruction

      include Join
    end

    module Control_flow() : sig
      type if_ =
        { cond  : expr
        ; then_ : expr t
        ; else_ : expr t
        }

      type while_ =
        { cond : expr
        ; body : unit t
        }

      type 'a instruction += 
        | If : if_ -> expr instruction
        | While : while_ -> unit instruction

      include Control_flow
    end

    (* TODO: Can the debugging instructions be represented an instruction rather than just an arbitrary
     * API call?
     *)
    module Debugging_stdout() : Debugging
    module Debugging_ignore() : Debugging
  end
end
