open Core_kernel

module type Variable = T

module type Expression = sig
  type t

  val zero : int -> t
  val one  : int -> t
  val of_int : width: int -> int -> t

  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
end

module type Loop = sig
  type expr
  type 'a t

  val for_ : expr -> expr -> (expr -> unit t) -> unit t
  val while_ : expr -> unit t -> unit t
end

module type Ref = sig
  type variable
  type expr
  type 'a t

  val new_ref : expr -> variable t
  val get_ref : variable -> expr t
  val set_ref : variable -> expr -> unit t
end

module Make(Expression : Expression) = struct

  type expr = Expression.t

  type 'a instruction = ..

  module T = struct
    type 'a t =
      | Return : 'a                                      -> 'a t
      | Then   : ('a instruction * ('a -> 'b t)) -> 'b t

    let return x = Return x

    let rec bind t ~f =
      match t with
      | Return a -> f a
      | Then (instr, k) ->
        Then (instr, (fun a -> bind (k a) ~f))

    let map = `Define_using_bind
  end

  include T
  include Monad.Make(T)

  module type Loop = Loop with type expr := expr and type 'a t := 'a t
  module type Ref = Ref with type expr := expr and type 'a t := 'a t

  module Loop() = struct
    type for_ =
      { start  : Expression.t
      ; end_   : Expression.t
      ; f      : Expression.t -> unit t
      }

    type while_ =
      { cond :  expr
      ; f    : unit t
      }

    type 'a instruction +=
      | For     : for_   -> unit instruction
      | While   : while_ -> unit instruction

    let for_ start end_ f = Then (For { start; end_; f }, return)
    let while_ cond f = Then (While { cond; f }, return)
  end

  module Ref(Variable : Variable) = struct
    type variable = Variable.t

    type 'a instruction +=
      | New_ref : Expression.t                -> Variable.t   instruction
      | Get_ref : Variable.t                  -> Expression.t instruction
      | Set_ref : (Variable.t * Expression.t) -> unit         instruction

    let new_ref expr       = Then (New_ref expr, return)
    let get_ref var        = Then (Get_ref var, return)
    let set_ref var expr   = Then (Set_ref (var, expr), return)
  end
end

