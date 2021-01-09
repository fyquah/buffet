(*
module Prog = struct
  type 'a t =
    | Return : 'a -> 'a t
    | Bind   : 'a t -> ('a -> 'b t) -> 'b t
    | Instr  : ('a t, exp, 'a) instr -> 'a t
end


module type Interpreter = sig
  module M : Monad.S

  val interpret : (('a prog, exp, 'a) instr  -> 'a M.t) -> 'a prog -> 'a M.t
end

module Expression = struct
  type 'a t =
    | H_var : ('a Type.t * string)        -> 'a t
    | H_lit : ('a Type.t * 'a)            -> 'a t
    | H_add : ('a Number.t * 'a t * 'a t) -> 'a t
end

module Ref = struct
  type 'a t =
    | RefE of 'a ref
    | RefC of string
end

module Val = struct
  type 'a t =
    | ValE of 'a
    | ValC of string
end

module Comp = struct
  type ('prg, 'exp, 'a) t =
    | New_ref : 'a Expression.t -> ('prg, 'expr, 'a Ref.t) t
    | Get_ref : 'a Ref.t        -> ('prg, 'expr, 'a Val.t) t
    | Set_ref : 'a Ref.t        -> 'a 'expr -> ('prg, 'exp, unit) t
    | For     : ('a 'expr, 'a 'expr, 'a 'expr, ('a Val.t -> unit 'prg)) -> ('prg, 'exp, unit) t
end

module type Mappable = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end
   *)

type 'a h = 'a Higher_kinded.t

module Expr = struct
  type 'f t =
    | In of ('f t -> 'f) h
end

module Val = struct
  module T = struct
    type 'a t =
      | Val of int

    let map (Val a) ~f:_ = (Val a)
  end

  include T
  include Higher_kinded.Make(T)
end

module Add = struct
  module T = struct
    type 'a t =
      | Add of ('a * 'a) 

    let map t ~f =
      match t with
      | Add (a, b) -> Add (f a, f b)
    ;;
  end

  include T
  include Higher_kinded.Make(T)
end

module type Instruction = sig
  include Higher_kinded.S

  val map : 'a t -> f:('a -> 'b) -> 'b t
end


module Both = struct
  type ('a, 'l, 'r) t =
    | Inl of ('a -> 'l) h
    | Inr of ('a -> 'r) h

  module Make(L : Instruction)(R : Instruction) = struct
    module T = struct
      type nonrec 'a t = ('a, L.higher_kinded, R.higher_kinded) t
    end

    include T
    include Higher_kinded.Make(T)

    let map t ~f =
      inject (
        match project t with
        | Inl l -> Inl (L.inject (L.map ~f (L.project l)))
        | Inr r -> Inr (R.inject (R.map ~f (R.project r)))
      )
    ;;
  end
end

module B = Both.Make(Val)(Add)

let add_example : B.higher_kinded Expr.t =
  Expr.In (B.inject (Inl (Val.inject (Val 1))))
;;

let rec fold_expr : 'a. f:(('a -> B.higher_kinded) h -> 'a) -> B.higher_kinded Expr.t -> 'a =
  fun ~f (In t) ->
    f (B.map ~f:(fun x -> fold_expr ~f x) t)
;;
