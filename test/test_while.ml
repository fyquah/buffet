open Core_kernel
open Hardcaml
open Buffet_kernel
open Buffet_back_ends

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

  let (!) = get_ref

  let factorial n =
    let* (n, acc) =
      join2
        (new_ref (E.of_int ~width:8 n))
        (new_ref (E.of_int ~width:32 1))
    in
    let* () =
      while_ E.(!n <>:. 0) (
        let* () = set_ref acc E.((!acc *: uresize !n 32).:[31, 0]) in
        let* () = set_ref n   E.(!n -:. 1) in
        return ()
      )
    in
    return !acc
  ;;

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

let%expect_test "test recipe back end" =
  let open Program(Recipe_back_end) in
  let test ~f n =
    let compiled = Utils.compile (f n) in
    let result = Utils.run compiled in
    Stdio.printf "%d\n" (Bits.to_int result);
  in
  let factorial = test ~f:factorial in
  let fibonacci = test ~f:fibonacci in
  factorial 1;
  [%expect {| 1 |}];
  factorial 6;
  [%expect {| 720 |}];
  fibonacci 1;
  [%expect {|
    1
    n = ?, f0 = ?, f1 = ? |}];
  fibonacci 2;
  [%expect {|
    1
    n = ?, f0 = ?, f1 = ? |}];
  fibonacci 3;
  [%expect {|
    2
    n = ?, f0 = ?, f1 = ? |}];
  fibonacci 7;
  [%expect {|
    13
    n = ?, f0 = ?, f1 = ? |}]
;;
