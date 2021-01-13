open Core_kernel
open Hardcaml

module Make(Underlying : Comb.Primitives) = struct

  module Primitive = struct
    module S = Symbolic_expression
    module U = Underlying

    type t =
      | Underlying of Underlying.t
      | Expression of t Symbolic_expression.t
    [@@deriving equal, sexp_of]

    let create_cmp2 value a b =
      Expression
        { width = 1
        ; value = S.Comparator (value a b)
        }

    let width = function
      | Underlying u -> Underlying.width u
      | Expression e -> e.width
    ;;

    let (<:)  = create_cmp2 (fun a b -> S.(Lt (a, b)))
    let (==:) = create_cmp2 (fun a b -> S.(Eq (a, b)))

    let create_op2 value a b =
      let wa = width a in
      assert (wa = width b);
      Expression
        { width = wa
        ; value = S.Operation (value a b)
        }
    ;;

    let ( +: )  = create_cmp2 (fun a b -> (Lt (a, b)))
    let ( -: )  = create_cmp2 (fun a b -> (Eq (a, b)))

    let ( &: ) = create_op2 (fun a b -> (And (a, b)))
    let ( |: ) = create_op2 (fun a b -> (Or  (a, b)))
    let ( ^: ) = create_op2 (fun a b -> (Xor (a, b)))
    let ( *: ) = create_op2 (fun a b -> (Multu (a, b)))
    let ( *+ ) = create_op2 (fun a b -> (Mults (a, b)))

    let mux select cases =
      assert (not (List.is_empty cases));
      let hd = List.hd_exn cases in
      let w = width hd in
      List.iter (List.tl_exn cases) ~f:(fun c ->
          assert (w = width c));
      Expression
        { width = w
        ; value = S.Mux { select ; cases }
        }
    ;;

    let (~:) a =
      Expression
        { width = (width a)
        ; value = S.Not a
        }
    ;;

    let select arg hi lo =
      Expression
        { width = (hi - lo + 1)
        ; value = S.Select { arg; hi; lo }
        }
    ;;

    let concat_msb args =
      let width = List.fold ~init:0 args ~f:(fun acc arg -> width arg + acc) in
      Expression
        { width
        ; value = S.Concat_msb args
        }
    ;;

    let is_empty = function
      | Underlying u -> Underlying.is_empty u
      | Expression _ -> false
    ;;

    let empty = Underlying U.empty
    let of_constant c = Underlying (U.of_constant c)

    (** TODO: Implement this properly. *)
    let to_string _ = ""
    let to_constant _ = assert false
    let (--) _ _ = assert false
  end

  include Comb.Make(Primitive)
end
