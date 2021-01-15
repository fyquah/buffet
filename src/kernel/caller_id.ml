open Base
module Printexc = Caml.Printexc

(* Small helper to find out who is the caller of a function *)
type t = Printexc.location option list

let sexp_of_location (t : Printexc.location) =
  let loc = Printf.sprintf "%s:%i:%i" t.filename t.line_number t.start_char in
  [%sexp (loc : string)]
;;

let sexp_of_t = [%sexp_of: location option list]

let get () =
  (*
  let skip =
    "list.ml"
    :: "list0.ml"
    :: "array.ml"
    :: "comb.ml"
    :: "interface.ml"
    :: "signal.ml"
    :: "bits.ml"
    :: "with_valid.ml"
    :: "scope.ml"
    :: "parameter.ml"
    :: "hierarchy.ml"
    :: Caml.__FILE__
    :: skip
  in
  let skip = Set.of_list (module String) skip in
     *)
  let stack = Printexc.get_callstack 16 in
  let len = Printexc.raw_backtrace_length stack in
  let rec full pos =
    if pos = len
    then []
    else
      (Printexc.get_raw_backtrace_slot stack pos
       |> Printexc.convert_raw_backtrace_slot
       |> Printexc.Slot.location)
      :: full (pos + 1)
  in
  full 0
;;
