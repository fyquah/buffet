open Core_kernel
open Hardcaml
open Buffet_kernel

module Expression = struct
  include Hardcaml.Signal
            
  let pp ppf (_ : t) = fprintf ppf "?"
end
module E = Expression

module Var_id = Identifier.Make(struct
    let name = "var_id"
  end)

module Variable = struct
  type t =
    { var_id : Var_id.t
    ; value  : Signal.t
    }

  let get_ref t = t.value
end

include Instructions.Make(Expression)
include Ref(Variable)
include Control_flow()
include Join()
include Debugging_ignore()

module Env = struct
  type var =
    { out : Signal.t
    ; ins : Signal.t With_valid.t list
    ; name : string option
    }

  type t =
    { vars : var Map.M(Var_id).t
    ; reg_spec : Reg_spec.t
    }

  let empty reg_spec =
    { vars = Map.empty (module Var_id)
    ; reg_spec
    }
  ;;

  let add_var ~name ~initial_input ~width (t : t) =
    let var_id = Var_id.create () in
    let out = Signal.wire width in
    let vars =
      Map.add_exn t.vars ~key:var_id
        ~data:{ out
              ; ins = [ initial_input ]
              ; name
              }
    in
    { Variable. var_id; value = out }, { t with vars }
  ;;

  let delay t a =
    Signal.reg t.reg_spec ~enable:Signal.vdd a
  ;;

  let set_reset (env : t) ~set ~reset =
    let out = Signal.wire 1 in
    let q = delay env E.(out &: (~:reset)) in
    E.(out <== (set |: q));
    out
  ;;

  let set_var (t : t) ~(var : Variable.t) ~start ~value =
    let vars =
      Map.update t.vars var.var_id ~f:(function 
          | None -> assert false
          | Some s ->
            let ins = { With_valid.valid = start; value } :: s.ins in
            { s with ins })
    in
    { t with vars }
  ;;
end

type 'a compile_results =
  { env : Env.t
  ; done_ : Signal.t
  ; return : 'a
  }

let rec compile : 'a . Env.t -> 'a t -> Signal.t -> 'a compile_results =
  fun env program start ->
  match program with
  | Return a ->
    { env
    ; done_ = start
    ; return = a
    }
  | Then (ins, k) ->
    begin match ins with
    | New_ref expression ->
      let new_var, new_env =
        (Env.add_var
           env
           ~name:None
           ~width:(Signal.width expression)
           ~initial_input:{ valid = start; value = expression })
      in
      compile
        new_env
        (k new_var)
        (Env.delay env start)
      
    | Set_ref (var, value) ->
      compile
        (Env.set_var env ~var ~value ~start)
        (k ())
        (Env.delay env start)

    | While { cond; body } ->
      let ready = Signal.wire 1 in
      let result = compile env body E.(cond &: ready) in
      E.(ready <== (start |: result.done_));
      compile
        result.env
        (k ())
        E.(~:cond &: ready)

    | Join progs -> compile_join env progs k start

    | If if_ -> compile_if env if_ k ~start

    (* | Debugf _fmt -> compile env (k ()) start *)

    | _ -> raise_s [%message "Incomplete implementation, this should not have happened!"]
    end

and compile_join : 'a 'b. Env.t -> 'a t list -> ('a list -> 'b t) -> Signal.t -> 'b compile_results =
  fun env_init progs k start ->
  let env, results =
    List.fold_map progs ~init:env_init ~f:(fun env prog ->
        let result = compile env prog start in
        result.env, result)
  in
  let fin = Signal.wire 1 in
  Signal.(
    fin <== (
      List.map results ~f:(fun result ->
          Env.set_reset env ~set:result.done_ ~reset:fin)
      |> Signal.tree ~arity:2 ~f:(reduce ~f:E.(&:)))
  );
  compile
    env
    (k (List.map results ~f:(fun r -> r.return)))
    fin

and compile_if : 'b. Env.t -> if_ -> (expr -> 'b t) -> start: Signal.t -> 'b compile_results =
  fun env0 (if_ : if_) k ~start ->
  let then_ = compile env0       if_.then_ E.(    if_.cond &: start) in
  let else_ = compile then_.env  if_.else_ E.(~:(if_.cond) &: start) in
  compile
    else_.env
    (k (E.mux2 then_.done_ then_.return else_.return))
    E.(then_.done_ |: else_.done_)
;;

let compile reg_spec (program : _ t) start =
  let compilation_result = compile (Env.empty reg_spec) program start in
  Map.iter compilation_result.env.vars ~f:(fun var ->
      let open Signal in
      let enable =
        List.map var.ins ~f:(fun a -> a.valid)
        |> tree ~arity:2 ~f:(reduce ~f:(|:)) 
      in
      var.out <== Signal.reg reg_spec ~enable (Signal.onehot_select var.ins));
  { With_valid.
    valid = compilation_result.done_
  ; value = compilation_result.return
  }
;;
