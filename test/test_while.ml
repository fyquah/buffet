open Core_kernel
open Hardcaml
open Ocaml_edsl_kernel

module Recipe_back_end = Utils.Recipe_back_end

module type Api = sig
  open Instructions

  module Expression : Expression

  include Base
  include Ref   with type expr := Expression.t and type 'a t := 'a t
  include While with type expr := Expression.t and type 'a t := 'a t
end

module Program(Api : Api) = struct
  open Api

  module E = Api.Expression

  let factorial n =
    let* n = new_ref (E.of_int ~width:8 n) in
    let* acc = new_ref (E.of_int ~width:32 1) in
    let* () =
      while_ E.(get_ref n <>:. 0) (
        let* () = set_ref acc E.(get_ref acc *: uresize (get_ref n) 32) in
        let* () = set_ref n   E.(get_ref n -:. 1) in
        return ()
      )
    in
    return (get_ref acc)
  ;;
end

let%expect_test "test step monad" =
  let open Ocaml_edsl_recipe in
  let open Program(Step_back_end) in
  let factorial n =
    let result = Step_back_end.run (factorial n) in
    Stdio.printf "%d"
      (Bits.to_int (Step_back_end.Expression.evaluate result));
  in
  factorial 1;
  [%expect {| 1 |}];
  factorial 6;
  [%expect {| 720 |}]
;;
