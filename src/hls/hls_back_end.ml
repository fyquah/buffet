open Core_kernel
open Hardcaml
open Buffet_kernel

module Middle_end = Hls_middle_end
module State_id = Middle_end.State_id
module Cfg = Middle_end.Cfg
module Var_id = Middle_end.Var_id
module Expression = Hls_front_end.Expression
module SE = Symbolic_expression

type t =
  { state_machine : State_id.t Always.State_machine.t
  ; proc          : Always.t list
  ; variables     : Always.Variable.t Map.M(Var_id).t
  }

module Env = struct
  type t =
    { on_done : Always.t
    ; next_state : State_id.t -> Always.t
    ; variables : Always.Variable.t Map.M(Var_id).t
    }

  let variable t var_id = Map.find_exn t.variables var_id

  (** TODO: Caching, dedup and all that beautiful stuff. *)
  let rec resolve_expr (t : t) (expr : Expression.t) =
    match expr with
    | Var_value var_id ->
      (Map.find_exn t.variables var_id).value
    | Reference r -> resolve_expr t r
    | Expression s ->
      begin match s.value with
        | Constant bits  -> Signal.of_constant (Bits.to_constant bits)
        | Comparator cmp -> resolve_cmp t cmp
        | Operation  op2 -> resolve_op2 t op2
        | _ -> assert false
      end

  and resolve_cmp (t : t) (cmp : _ SE.comparator) =
    let r = resolve_expr t in
    let open Signal in
    match cmp with
    | Lt (a, b) -> r a <:  r b
    | Gt (a, b) -> r a >:  r b
    | Eq (a, b) -> r a ==: r b

  and resolve_op2 (t : t) (op2 : _ SE.operation) =
    let open Signal in
    let r = resolve_expr t in
    match op2 with
    | Add (a, b) -> r a +: r b
    | Sub (a, b) -> r a -: r b
    | _ -> assert false
  ;;
end

let rec compile_transitions (env : Env.t) (transitions : Middle_end.State.transitions) =
  let assignments =
    List.map transitions.assignments ~f:(fun (var_id, expr) ->
        let variable = Env.variable env var_id in
        Always.(variable <-- Env.resolve_expr env expr))
  in
  let next_state =
    match transitions.children with
    | Leaf Done               -> [ env.on_done ]
    | Leaf (State next_state) -> [ env.next_state next_state ]
    | Tree { cases; default } ->
      compile_cases env cases default
  in
  (assignments @ next_state)

and compile_cases (env : Env.t) cases default =
  match cases with
  | [] ->
    compile_transitions env default
  | (cond, case) :: tl ->
    [ Always.if_ (Env.resolve_expr env cond)
        (compile_transitions env case)
        (compile_cases env tl default)
    ]
;;

let compile_cases env (middle_end : Middle_end.t) =
  List.map (Middle_end.Cfg.all_states middle_end.cfg)  ~f:(fun state_id ->
      let state = Map.find_exn middle_end.cfg.states state_id in
      (state_id, compile_transitions env state.transitions))
;;

let compile (reg_spec : Reg_spec.t) (middle_end : Middle_end.t) =
  let module States = struct
    type t = State_id.t
    [@@deriving sexp_of, compare]

    let all = Middle_end.Cfg.all_states middle_end.cfg
    ;;
  end
  in
  let state_machine =
    Always.State_machine.create (module States) reg_spec ~enable:Signal.vdd
  in
  let variables =
    List.map (Set.to_list middle_end.variables) ~f:(fun var_id ->
        (var_id,
         Always.Variable.reg reg_spec ~enable:Signal.vdd ~width:(Var_id.width var_id)))
    |> Var_id.Map.of_alist_exn
  in
  let cases =
    compile_cases 
      { Env.
        on_done = state_machine.set_next middle_end.cfg.initial_state
      ; next_state = (fun s -> state_machine.set_next s)
      ; variables
      }
      middle_end
  in
  { state_machine
  ; proc = [ state_machine.switch cases ]
  ; variables
  }
;;
