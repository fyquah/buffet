open Core_kernel

module Ast = Hls_front_end.Ast
module Expression = Hls_front_end.Expression
module Var_id = Hls_front_end.Var_id

module State_id : sig
  include Comparable.S

  val to_string : t -> string
end

module State : sig
  type assignments = Expression.t Map.M(Var_id).t

  type transitions =
    | Tree of
        { assignments : assignments
        ; children    : (Expression.t, transitions) List.Assoc.t
        }
    | Leaf of
        { assignments : assignments
        ; next_state  : State_id.t
        }

  type t =
    { id          : State_id.t
    ; assignments : assignments
    ; transitions : transitions
    }
end


(** A CFG-style representation of a state machine. *)
type t =
  { initial_state : State.t
  ; states        : State.t Map.M(State_id).t
  ; variables     : Set.M(Var_id).t
  }

val compile_expression
   : Expression.t Ast.t
  -> t
