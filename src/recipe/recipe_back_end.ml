open Core_kernel
open Hardcaml
open Ocaml_edsl_kernel

module Expression = Hardcaml.Signal

module Var_id = Identifier.Make(struct
    let name = "var_id"
  end)

module Variable = struct
  type t =
    { var_id : Var_id.t
    ; value  : Signal.t
    }

  let value t = t.value
end

include Instructions.Make(Expression)
include Ref(Variable)

let get_ref = Variable.value

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

let rec compile (env : Env.t) (program : _ t) (start : Signal.t) =
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

    | _ -> raise_s [%message "Incomplete implementation, this should not have happened!"]
    end
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
