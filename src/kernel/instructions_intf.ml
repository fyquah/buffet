open Core_kernel

module type Variable = sig
  type expr
  type t

  val get_ref : t -> expr
end

module type Base = sig
  include Monad.S

  (** Nicer monad infixs. *)
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let+) : 'a t -> ('a -> 'b)   -> 'b t
end

module type Expression = sig
  type t

  include Hardcaml.Comb.S with type t := t

  val pp : Stdlib.out_channel -> t -> unit
end

module type Ref = sig
  type variable
  type expr
  type 'a t

  val new_ref : expr -> variable t
  val get_ref : variable -> expr
  val set_ref : variable -> expr -> unit t
end

module type Join = sig
  type 'a t

  val join_ : 'a t list -> 'a list t
  val join2 : 'a t -> 'b t         -> ('a * 'b) t
  val join3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val par   : unit t list -> unit t
  val par2  : unit t -> unit t           -> unit t
  val par3  : unit t -> unit t -> unit t -> unit t
end

module type Conditional = sig
  type expr
  type 'a t

  val if_ : expr -> expr t -> expr t -> expr t
end

module type While = sig
  type expr
  type 'a t

  val while_ : expr -> unit t -> unit t
end

module type Instructions = sig
  module type Base = Base
  module type Expression = Expression
  module type Variable = Variable

  module type Ref = Ref
  module type Join = Join
  module type Conditional = Conditional
  module type While = While

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

    (* API module types. *)
    module type Ref         = Ref         with type expr := expr and type 'a t := 'a t
    module type Join        = Join        with type 'a t := 'a t
    module type Conditional = Conditional with type expr := expr and type 'a t := 'a t
    module type While       = While       with type expr := expr and type 'a t := 'a t

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

    module Conditional() : sig
      type if_ =
        { cond  : expr
        ; then_ : expr t
        ; else_ : expr t
        }

      type 'a instruction += 
        | If : if_ -> expr instruction

      include Conditional (** @inline *)
    end

    module While() : sig
      type while_ =
        { cond : expr
        ; body : unit t
        }

      type 'a instruction += 
        | While : while_ -> unit instruction

      include While
    end
  end
end
