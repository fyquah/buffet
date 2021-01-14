open Core_kernel
open Ocaml_edsl_kernel

module Var_id : sig
  include Comparable.S

  val create : int -> t

  val width : t -> int
end

module Expression : sig
  type t =
    | Var_value of Var_id.t
    | Reference of t
    | Expression of t Symbolic_expression.t

  val reference : t -> t

  val var_value : Var_id.t -> t

  val width : t -> int

  val (==:) : t -> t -> t

  val (+:.) : t -> int -> t

  val (>:) : t -> t -> t

  include Instructions.Expression with type t := t
end

module Ast : sig
  type expr = Expression.t

  type 'a t =
    | Seq        of (unit t * 'a t)
    | With_var   of 'a with_var
    | If         of 'a if_
    | Assign_var of (Var_id.t * Expression.t)
    | Return     of 'a

  and for_ =
    { start : expr
    ; end_  : expr
    ; index : Var_id.t
    ; for_body  : unit t
    }

  and 'a if_ =
    { cond   : expr
    ; then_  : 'a t
    ; else_  : 'a t
    }

  and 'a with_var =
    { var_id        : Var_id.t
    ; initial_value : expr
    ; body          : 'a t
    }

  val with_vars : (Var_id.t * expr) list -> 'a t -> 'a t

  val seq : unit t list -> 'a t -> 'a t
end

include module type of Instructions.Make(Expression)
include Ref with type variable = Var_id.t

val compile_to_ast : 'a t -> 'a Ast.t
