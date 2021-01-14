open Core_kernel
open Hardcaml
open Ocaml_edsl_kernel
open Ocaml_edsl_back_ends

module Recipe_back_end = Utils.Recipe_back_end

module type Api = sig
  open Instructions

  module Expression : Expression

  include Base
  include Ref   with type expr := Expression.t and type 'a t := 'a t
  include While with type expr := Expression.t and type 'a t := 'a t
  include Conditional with type expr := Expression.t and type 'a t := 'a t
  include Join with                               type 'a t := 'a t
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

  let (!) = get_ref

  let fibonacci n =
    let* n   = new_ref (E.of_int ~width:8 n) in
    let* f0  = new_ref (E.of_int ~width:32 1) in
    let* f1  = new_ref (E.of_int ~width:32 1) in
    if_ E.((!n ==:. 0) |: (!n ==:. 1)) (
      return (E.of_int ~width:32 1)
    ) @@ (
      let* () =
        while_ E.(!n >:. 2) (
          let+ () =
            par [
              set_ref f0    (!f1)
            ; set_ref f1    E.(!f0 +: !f1)
            ; set_ref n     E.(get_ref n -:. 1)
            ]
          in
          Format.printf "n = %a, f0 = %a, f1 = %a\n\n"
            Expression.pp (!n)
            Expression.pp (!f0)
            Expression.pp (!f1)
        )
      in
      return !f1
    )
  ;;

end

let%expect_test "test step monad" =
  let open Program(Step_back_end) in
  let test ~f n =
    let result = Step_back_end.run (f n) in
    Stdio.printf "%d"
      (Bits.to_int (Step_back_end.Expression.evaluate result));
  in
  let factorial = test ~f:factorial in
  let fibonacci = test ~f:fibonacci in
  factorial 1;
  [%expect {| 1 |}];
  factorial 6;
  [%expect {| 720 |}];
  fibonacci 1;
  [%expect {| 1 |}];
  fibonacci 2;
  [%expect {| 1 |}];
  fibonacci 3;
  [%expect {| 2n = 8'u2, f0 = 32'u1, f1 = 32'u2 |}];
  fibonacci 7;
  [%expect {|
    n = 8'u6, f0 = 32'u1, f1 = 32'u2

    n = 8'u5, f0 = 32'u2, f1 = 32'u3

    n = 8'u4, f0 = 32'u3, f1 = 32'u5

    n = 8'u3, f0 = 32'u5, f1 = 32'u8

    n = 8'u2, f0 = 32'u8, f1 = 32'u13

    13 |}];
;;
