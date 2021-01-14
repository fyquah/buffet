open Core_kernel
open Hardcaml

module type Reference = sig
  type underlying
  type t [@@deriving sexp_of, equal]

  val width : t -> int
end

module type S = sig
  type reference
  type underlying

  include Comb.S

  val evaluate : deref:(reference -> t) -> t -> underlying
end

module Make
    (Underlying : Comb.S)
    (Reference : Reference with type underlying := Underlying.t) = struct

  module Primitive = struct
    module S = Symbolic_expression
    module U = Underlying

    type t =
      | Reference of Reference.t
      | Constant of Underlying.t
      | Expression of t Symbolic_expression.t
    [@@deriving equal, sexp_of]

    let create_cmp2 value a b =
      Expression
        { width = 1
        ; value = S.Comparator (value a b)
        }

    let width = function
      | Reference r  -> Reference.width r
      | Constant u -> Underlying.width u
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

    let ( +: )  = create_op2 (fun a b -> (Add (a, b)))
    let ( -: )  = create_op2 (fun a b -> (Sub (a, b)))

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
      | Constant u -> Underlying.is_empty u
      | Reference _
      | Expression _ -> false
    ;;

    let empty = Constant U.empty
    let of_constant c = Constant (U.of_constant c)

    (** TODO: Implement this properly. *)
    let to_string _ = ""
    let to_constant _ = assert false
    let (--) x _name = x
  end

  include Primitive
  include Comb.Make(Primitive)

  let rec evaluate ~(deref : Reference.t -> Underlying.t) (t : t) =
    match t with
    | Constant c -> c
    | Reference r -> deref r
    | Expression symbolic_expression ->
      Symbolic_expression.evaluate
        (module Underlying)
        symbolic_expression
        ~eval_child:(evaluate ~deref)
  ;;

  let reference r = Reference r
end
