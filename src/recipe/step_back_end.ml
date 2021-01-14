open Core_kernel
open Hardcaml
open Ocaml_edsl_kernel

module Expression = struct
  include Dynamic_expression.Make(Bits)(struct
    type t = Bits.t ref [@@deriving sexp_of, equal]

    let width t = Bits.width !t
  end)

  let evaluate t = evaluate ~deref:(!) t
end

module Step_monad = Digital_components.Step_monad

include Instructions.Make(Expression)
include Ref(struct type t = Bits.t ref end)

let get_ref r = Expression.reference r

module Executor = struct
  module Component = Digital_components.Component
  module Empty = struct
    type t = unit [@@deriving sexp_of, equal]

    let undefined = ()
  end

  let execute step =
    let component, result_event =
      Step_monad.create_component
        ~created_at:[%here]
        ~start:(fun () ->
            let%bind.Step_monad result = step in
            Step_monad.return { Step_monad.Component_finished. output = () ; result }
          )
        ~input:(module Empty)
        ~output:(module Empty)
    in
    Component.run_until_finished
      component
      ~first_input:()
      ~next_input:(fun () ->
          if Option.is_some (Step_monad.Event.value result_event) then
            Component.Next_input.Finished
          else
            Component.Next_input.Input ());
    match Step_monad.Event.value result_event with
    | None -> assert false
    | Some x -> x.result
  ;;

  let rec program_to_step (program : _ t) =
    match program with
    | Return a -> Step_monad.return a
    | Then (ins, k) ->
      begin match ins with
        | New_ref expression ->
          let new_var = ref (Expression.evaluate expression) in
          let%bind.Step_monad _output_ignored = Step_monad.next_step [%here] () in
          program_to_step (k new_var)

        | Set_ref (var, value) ->
          let value = Expression.evaluate value in
          let%bind.Step_monad _output_ignored =
            Step_monad.next_step [%here] () 
          in
          var := value;
          program_to_step (k ())

        | _ -> raise_s [%message "Incomplete implementation, this should not have happened!"]
      end
  ;;
end

let run program = Executor.(execute (program_to_step program))
