open Core_kernel
open Hardcaml
open Ocaml_edsl_kernel

module type Api = sig
  open Instructions

  module Expression : Expression

  include Base
  include Loop with type expr := Expression.t and type 'a t := 'a t
  include Ref  with type expr := Expression.t and type 'a t := 'a t
end

module Make(Api : Api) = struct
  open Api
  open Api.Let_syntax

  module E = Api.Expression

  let main =
    let* var = new_ref (Expression.zero 8) in
    let* () =
      Api.for_ (E.of_int ~width:8 0)  (E.of_int ~width:8 8) (fun i ->
          set_ref var Expression.(get_ref var +: i))
    in
    return (get_ref var)
  ;;
end

let%expect_test "test arithmetic" =
  let open Ocaml_edsl_interpreter in
  let open Make(Interpreter_front_end) in
  Stdio.printf "%d"
    (Bits.to_int (Interpreter_front_end.Expression.value
                    (Interpreter_front_end.interpret main)));
  [%expect {| 36 |}]
;;
