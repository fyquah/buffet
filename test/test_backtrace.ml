open Core_kernel
open Buffet_kernel

module type Api = sig
  open Instructions

  module Expression : Expression

  include Base
  include Ref  with type expr := Expression.t and type 'a t := 'a t
end

module Make(Api : Api) = struct
  open Api
  open Api.Let_syntax

  module E = Api.Expression

  let (!) = get_ref

  let main ~error_location =
    let* foo = new_ref (Expression.zero 8) in
    let* bar = new_ref (Expression.zero 6) in
    let* baz = new_ref (Expression.zero 8) in
    let* () = return () in
    let* ()  =
      match error_location with
      | `Location_1 ->
        set_ref bar E.(!foo +: !bar +: !baz)
      | _ -> return ()
    in
    let* ()  =
      match error_location with
      | `Location_2 ->
        set_ref bar E.(!foo +: !bar +: !baz)
      | _ -> return ()
    in
    let* ()  =
      match error_location with
      | `Location_3 ->
        set_ref bar E.(!foo +: !bar +: !baz)
      | _ -> return ()
    in
    return (get_ref foo)
  ;;
end

let find_first_entry_in_this_file bt =
  List.find_map (Backtrace.to_string_list bt) ~f:(fun entry ->
      if Core_kernel.String.is_substring entry ~substring:"test_backtrace.ml" then
        (* Silly hack to stop inline thest helper from printing CR from expect test
         * collector.
         *)
        Some (String.subo ~pos:12 entry)
      else
        None
    )
;;

(* Note that in these tests, the line numbers are relevant in the tests, since they
 * show that they happen where they are suppoed to
 *)
let%expect_test "Demonstrate backtrace displays real error location in step backend" =
  Backtrace.elide := false;
  let open Buffet_back_ends in
  let open Make(Step_back_end) in
  let test error_location =
    try
      ignore (
        Step_back_end.Expression.evaluate (Step_back_end.run (main ~error_location))
        : Hardcaml.Bits.t
      )
    with
    | _ -> 
      (match (find_first_entry_in_this_file (Backtrace.Exn.most_recent ())) with
       | None -> Stdio.print_endline "<failed to match for exception entry>"
       | Some a -> Stdio.print_endline a)
  in
  test `Location_1;
  [%expect {| file "test/test_backtrace.ml", line 29, characters 23-35 |}];
  test `Location_2;
  [%expect {| file "test/test_backtrace.ml", line 35, characters 23-35 |}];
  test `Location_3;
  [%expect {| file "test/test_backtrace.ml", line 41, characters 23-35 |}]
;;

let%expect_test "Demonstrate backtrace displays real error location in recipe backend" =
  Backtrace.elide := false;
  let open Buffet_back_ends in
  let open Make(Recipe_back_end) in
  let test error_location =
    try
      let open Hardcaml in
      let clock = Signal.input "clock" 1 in
      let start = Signal.input "start" 1 in
      let reg_spec = Reg_spec.create ~clock () in
      ignore (
        (Recipe_back_end.compile reg_spec (main ~error_location) start)
        : _ Hardcaml.With_valid.t2
      )
    with
    | _ -> 
      (match (find_first_entry_in_this_file (Backtrace.Exn.most_recent ())) with
       | None -> Stdio.print_endline "<failed to match for exception entry>"
       | Some a -> Stdio.print_endline a)
  in
  test `Location_1;
  [%expect {| file "test/test_backtrace.ml", line 29, characters 23-35 |}];
  test `Location_2;
  [%expect {| file "test/test_backtrace.ml", line 35, characters 23-35 |}];
  test `Location_3;
  [%expect {| file "test/test_backtrace.ml", line 41, characters 23-35 |}]
;;
