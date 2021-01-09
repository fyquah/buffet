open Hardcaml
open Ocaml_edsl

module type Api = sig
  open Front_end

  module Instruction : Instruction
  module Program     : Program with module Instruction := Instruction
  module Expression  : Expression

  module Ref         : Ref
    with type expression   := Expression.t
     and type 'a program   := 'a Program.t
     and type 'a container := 'a

  include Loop with type expression := Expression.t and type 'a program := 'a Program.t
end

module Make(Api : Api) = struct
  open Api
  open Api.Program.Let_syntax

  module E = Api.Expression

  let main =
    let%bind var = Api.Ref.create (Expression.zero 8) in
    let%bind () =
      Api.for_ (E.of_int ~width:8 0)  (E.of_int ~width:8 8) (fun i ->
          let%bind x = Api.Ref.get var in
          Api.Ref.set var Expression.(x +: i))
    in

    Ref.get var
  ;;
end

let%expect_test "test arithmetic" =
  let open Make(Interpreter_front_end) in
  Stdio.printf "%d" (Bits.to_int (Interpreter_front_end.interpret main));
  [%expect {| 36 |}]
;;
