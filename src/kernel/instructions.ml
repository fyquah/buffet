open Core_kernel

module type Variable = T

module type Base = sig
  include Monad.S

  (** Nicer monad infixs. *)
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let+) : 'a t -> ('a -> 'b)   -> 'b t
end

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
  val get_ref : variable -> expr
  val set_ref : variable -> expr -> unit t
end

module type Scheduling = sig
  type 'a t

  val in_a_cycle : 'a t -> 'a t
end

module type Conditional = sig
  type expr
  type 'a t

  val if_ : expr -> 'a t -> 'a t -> 'a t
end

module Make(Expression : Expression) = struct

  type expr = Expression.t

  type 'a instruction = ..

  module T = struct
    type 'a t =
      | Return : 'a                              -> 'a t
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

  let (let*) a f = a >>= f
  let (let+) a f = a >>| f

  module type Loop = Loop with type expr := expr and type 'a t := 'a t
  module type Ref  = Ref with type expr := expr and type 'a t := 'a t
  module type Scheduling = Scheduling with type 'a t := 'a t
  module type Conditional = Conditional with type expr := expr and type 'a t := 'a t

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

    (* Confusingly, a [Get_ref] instruction is explicitly exlucded. This makes writing
     * things like
     *
     * {[ while_ (Variable.value a) (
           body
         )
         ]}

       much more nicer, rather than having to bind the value to a temporary ocaml binding
     *)
    type 'a instruction +=
      | New_ref : Expression.t                -> Variable.t   instruction
      | Set_ref : (Variable.t * Expression.t) -> unit         instruction

    let new_ref expr       = Then (New_ref expr, return)
    let set_ref var expr   = Then (Set_ref (var, expr), return)
  end

  module Scheduling() = struct
    type 'a instruction += 
      | In_a_cycle : 'a t -> 'a instruction

    let in_a_cycle prog = Then (In_a_cycle prog, return)
  end

  module Conditional() = struct
    type 'a if_ =
        { cond  : expr
        ; then_ : 'a t
        ; else_ : 'a t
        }

    type 'a instruction += 
      | If : 'a if_ -> 'a instruction

    let if_ cond then_ else_ = Then (If { cond; then_; else_ }, return)
  end
end
