open Core_kernel
open Hardcaml
open Ocaml_edsl_kernel
open Ocaml_edsl_back_ends

module Recipe_back_end = Utils.Recipe_back_end

module type Api = sig
  open Instructions

  module Expression : Expression

  include Base
  include Ref  with type expr := Expression.t and type 'a t := 'a t
end

module Program(Api : Api) = struct
  open Api
  open Api.Let_syntax

  module E = Api.Expression

  let main =
    let open E in
    let* foo = new_ref (Expression.of_int ~width:8 0xaa) in
    let* bar = new_ref (Expression.of_int ~width:8 0xbb) in
    let* () = set_ref foo (get_ref foo +: of_int ~width:8 1) in
    Stdio.print_endline "Called set_ref on foo!";
    let* () = set_ref bar (get_ref bar +: get_ref foo) in
    Stdio.print_endline "Called set_ref on bar!";
    return (get_ref bar)
  ;;
end

let%expect_test "test step monad" =
  let open Program(Step_back_end) in
  let result = Step_back_end.run main in
  Stdio.printf "0x%x"
    (Bits.to_int (Step_back_end.Expression.evaluate result));
  [%expect {|
    Called set_ref on foo!
    Called set_ref on bar!
    0x66 |}]
;;

let%expect_test "test with recipe circuit" =
  let open Program(Recipe_back_end) in
  let compiled = Utils.compile main in
  [%expect {|
    Called set_ref on foo!
    Called set_ref on bar! |}];
  (* This demonstrates that printf is only executed during compilation
   * and not execution.
   *)
  Stdio.printf "0x%x" (Bits.to_int (Utils.run compiled));
  [%expect {| 0x66 |}];
  Stdio.printf "0x%x" (Bits.to_int (Utils.run compiled));
  [%expect {| 0x66 |}];
;;
