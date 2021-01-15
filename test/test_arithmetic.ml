open Core_kernel
open Hardcaml
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

  let main =
    let* var = new_ref (Expression.zero 8) in
    return (get_ref var)
  ;;
end

let%expect_test "test arithmetic" =
  let open Buffet_back_ends in
  let open Make(Step_back_end) in
  Stdio.printf "%d"
    (Bits.to_int (Step_back_end.Expression.evaluate
                    (Step_back_end.run main )));
  [%expect {| 0 |}]
;;
