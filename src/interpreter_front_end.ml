open Core_kernel
open Hardcaml

module Expression = struct
  open Bits

  type t =
    | Value of Bits.t
    | Reference of Bits.t ref

  let value = function
    | Value a -> a
    | Reference r -> !r

  let zero width = Value (Bits.zero width)
  let one width = Value (Bits.one width)
  let of_int ~width value = Value (Bits.of_int ~width value)

  let (+:) a b = Value ((value a) +: (value b))
  let (+:.) a b = Value ((value a) +:. b)
  let (-:) a b = Value ((value a) -: (value b))

  let is_vdd a = Bits.is_vdd (value a)
  let (>:) a b = Value (value a >: value b)
end

include Front_end.Make(Expression)
include Loop ()
include Ref (struct
    type t = Bits.t ref
  end)

open Let_syntax

let rec loop_bits ~start ~end_ f =
  if Expression.(is_vdd ((>:) start end_ )) then
    return ()
  else
    let%bind () = f start in
    loop_bits ~start:(Expression.(+:.) start 1) ~end_ f
;;

let rec interpret (program : _ t) =
  match program with
  | Return a -> a
  | Then (ins, k) ->
    begin match ins with
    | New_ref expression ->
      interpret (k (ref (Expression.value expression)))
    | Get_ref r -> interpret (k (Expression.Reference r))
    | Set_ref (r, v) ->
      r := Expression.value v;
      interpret (k ())
    | For { start; end_; f; } ->
      interpret (
        let%bind () = loop_bits ~start ~end_ f in
        k ()
      )
    | _ -> raise_s [%message "Implementation incomplete!"]
    end

;;
