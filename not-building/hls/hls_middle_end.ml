open Core_kernel

module Ast = Hls_front_end.Ast
module Expression = Hls_front_end.Expression
module E = Expression
module Var_id = Hls_front_end.Var_id

module State_id = Buffet_kernel.Identifier.Make(struct
    let name = "state_id"
  end)

module State = struct
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

  let _leaf ?(assignments = []) leaf =
    { assignments; children = Leaf leaf }
  ;;

  let iter_children ~f (t : t) =
    let rec loop (transitions : transitions) =
      match transitions.children with
      | Leaf Done -> ()
      | Leaf (State child_state) -> f child_state
      | Tree { cases; default = _ } ->
        List.iter cases ~f:(fun (_expr, transitions) -> loop transitions)
    in
    loop t.transitions

  ;;
end

module Cfg = struct
  type t =
    { initial_state : State_id.t
    ; states : State.t Map.M(State_id).t
    }

  let dfs (t : t) ~f =
    let visited = ref (State_id.Set.singleton t.initial_state) in
    let rec loop state_id =
      f state_id;
      State.iter_children (Map.find_exn t.states state_id) ~f:(fun child ->
          if not (Set.mem !visited child) then (
            visited := Set.add !visited child;
            loop child
          )
        );
    in
    loop t.initial_state
  ;;

  let all_states (t : t) =
    let states = ref [] in
    dfs t ~f:(fun s -> states := s :: !states);
    List.rev !states
  ;;

  let initial_state (t : t) = Map.find_exn t.states t.initial_state

  let singleton ~assignments =
    let initial_state = State_id.create () in
    { initial_state
    ; states =
        State_id.Map.singleton initial_state
          { State.
            id = initial_state
          ; transitions =
              { assignments
              ; children = Leaf Done
              }
          }
    }
  ;;

  let rec rewrite_leaves ~f (transitions : State.transitions) =
    match transitions.children with
    | Tree { cases; default } ->
      let children =
        State.Tree
          { cases =
              List.map cases ~f:(fun (expr, trans) ->
                  (expr, rewrite_leaves ~f trans))
          ; default = rewrite_leaves ~f default
          }
      in
      { transitions with children }
    | State.Leaf leaf -> f transitions.assignments leaf
  ;;

  let rewrite_state_leaves ~f (t : t) =
    let states =
      Map.map t.states ~f:(fun (state : State.t) ->
          let transitions = rewrite_leaves ~f state.transitions in
          { state with transitions })
    in
    { t with states }
  ;;

  let _rewrite_done (t : t) ~f =
    rewrite_state_leaves t ~f:(fun assignments leaf ->
        match leaf with
        | State _ -> { assignments; children = Leaf leaf }
        | Done -> f assignments)
  ;;
end

type t =
  { preemptive_assignments : (Var_id.t * Expression.t) list
  ; variables : Set.M(Var_id).t
  ; cfg : Cfg.t
  }


let combine_map m1 m2 =
  Map.merge  m1 m2 ~f:(fun ~key:_ data ->
      match data with
      | `Left l | `Right l -> Some l
      | `Both _ -> assert false)
;;

let seq (ta : t) (tb : t) =
  let cfg_a = ta.cfg in
  let cfg_b = tb.cfg in
  let cfg_a =
    Cfg.rewrite_state_leaves cfg_a ~f:(fun assignments leaf ->
        match leaf with
        | State _ -> { State. assignments; children = Leaf leaf }
        | Done ->
          { State.
            assignments = tb.preemptive_assignments
          ; children = Leaf (State cfg_b.initial_state)
          })
  in
  { preemptive_assignments = ta.preemptive_assignments
  ; variables = Set.union ta.variables tb.variables
  ; cfg = 
      { initial_state = cfg_a.initial_state
      ; states = combine_map cfg_a.states cfg_b.states
      }
  }

;;

let rec compile (ast : unit Ast.t) =
  match ast with
  | Seq (ta, tb) ->
    let ta = compile ta in
    let tb = compile tb in
    seq ta tb

  | With_var { var_id; initial_value; body } ->
    let t = compile body in
    let preemptive_assignments =
      (var_id, initial_value) :: t.preemptive_assignments
    in
    { preemptive_assignments
    ; variables = Set.add t.variables var_id
    ; cfg = t.cfg
    }

  | Assign_var (var_id, expr) ->
    { preemptive_assignments = []
    ; variables = Var_id.Set.singleton var_id
    ; cfg = Cfg.singleton ~assignments:[(var_id, expr)]
    }

  | _ -> assert false
;;
