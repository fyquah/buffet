open Core_kernel

module Ast = Hls_front_end.Ast
module Expression = Hls_front_end.Expression
module Var_id = Hls_front_end.Var_id

module State_id : sig
  include Sexpable
  include Comparable.S with type t := t

  val to_string : t -> string
end

module State : sig
  type assignments = (Var_id.t * Expression.t) list

  type transitions =
    { assignments : assignments
    ; children    : children
    }

  and children =
    | Tree of
        { cases : (Expression.t * transitions) list
        ; default : transitions
        }
    | Leaf of leaf

  and leaf =
    | Done
    | State of State_id.t

  type t =
    { id          : State_id.t
    ; transitions : transitions
    }
end

module Cfg : sig
  type t =
    { initial_state : State_id.t
    ; states : State.t Map.M(State_id).t
    }

  val initial_state : t -> State.t


  (** Guranteses to return the initial state as the first element of the
   * list. *)
  val all_states : t -> State_id.t list
end

(** A CFG-style representation of a state machine. *)
type t =
  { preemptive_assignments : (Var_id.t * Expression.t) list
  ; variables : Set.M(Var_id).t
  ; cfg : Cfg.t
  }

val compile : unit Ast.t -> t
