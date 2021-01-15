open Core_kernel
open Hardcaml
open Buffet_kernel

module type Api = sig
  open Instructions

  module Expression : Expression

  include Base
  include Join          with                               type 'a t := 'a t
  include Control_flow  with type expr := Expression.t and type 'a t := 'a t
  include Ref           with type expr := Expression.t and type 'a t := 'a t
  include Debugging     with                               type 'a t := 'a t
end

module Make(Api : Api) = struct
  open Api
  open Api.Let_syntax

  module E = Api.Expression

  let (!) = get_ref

  let main n =
    let* foo = new_ref (Expression.zero 8) in
    let* bar = new_ref (Expression.zero 8) in
    let* () =
      let foo_next = E.(!foo +:. 1) in
      let debug () = debugf "foo = %a, foo <- %a\n" E.pp (!foo) E.pp foo_next in
      let task1 =
        let* () = debug () in
        let* () = set_ref foo E.(!foo +:. 1) in
        let* () = debug () in
        let* () = set_ref foo E.(!foo +:. 1) in
        let* () = debug () in
        let* () = set_ref foo E.(!foo +:. 1) in
        let* () = debug () in
        let* () = set_ref foo E.(!foo +:. 1) in
        let* () = debug () in
        return ()
      in
      let task2 =
        let* () = pass_n n in
        let* () = debugf "setting bar to = %a\n" E.pp (!foo) in
        let* () = set_ref bar !foo in
        return ()
      in
      par2 task1 task2
    in
    return (get_ref bar)
  ;;
end

let%expect_test "test arithmetic" =
  let open Buffet_back_ends in
  let open Make(Step_back_end) in
  let test n =
    Stdio.printf "%d"
      (Bits.to_int (Step_back_end.Expression.evaluate (Step_back_end.run (main n))));
  in
  test 2;
  [%expect {|
    foo = 8'u0, foo <- 8'u1
    foo = 8'u1, foo <- 8'u2
    setting bar to = 8'u2
    foo = 8'u2, foo <- 8'u3
    foo = 8'u3, foo <- 8'u4
    foo = 8'u4, foo <- 8'u5
    2 |}];
  test 1;
  [%expect {|
    foo = 8'u0, foo <- 8'u1
    setting bar to = 8'u1
    foo = 8'u1, foo <- 8'u2
    foo = 8'u2, foo <- 8'u3
    foo = 8'u3, foo <- 8'u4
    foo = 8'u4, foo <- 8'u5
    1 |}];
  test 0;
  [%expect {|
    foo = 8'u0, foo <- 8'u1
    setting bar to = 8'u0
    foo = 8'u1, foo <- 8'u2
    foo = 8'u2, foo <- 8'u3
    foo = 8'u3, foo <- 8'u4
    foo = 8'u4, foo <- 8'u5
    0 |}];
  (* Test that the increment operators don't still keep going even after the operation
   * has been completed.
   *)
  test 10;
  [%expect {|
    foo = 8'u0, foo <- 8'u1
    foo = 8'u1, foo <- 8'u2
    foo = 8'u2, foo <- 8'u3
    foo = 8'u3, foo <- 8'u4
    foo = 8'u4, foo <- 8'u5
    setting bar to = 8'u4
    4 |}]
;;
