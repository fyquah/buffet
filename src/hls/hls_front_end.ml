open Core_kernel
open Ocaml_edsl_kernel

module Var_id : sig
  include Comparable.S

  val width : t -> int

  val create : int -> t
end = struct

  module T = struct
    type t =
      { id    : int
      ; width : int
      }
    [@@deriving sexp]

    let compare a b = Int.compare a.id b.id
  end

  include T
  include Comparable.Make(T)

  let width t = t.width

  let create =
    let ctr = ref (-1) in
    fun width ->
      Int.incr ctr;
      { id = !ctr; width }
  ;;
end

module Expression = struct
  module S = Symbolic_expression

  type t =
    | Var_value of Var_id.t
    | Reference of t
    | Expression of t Symbolic_expression.t

  let rec width = function
    | Var_value var -> Var_id.width var
    | Reference x -> width x
    | Expression e -> Symbolic_expression.width e

  let reference     x = Reference x

  let of_int ~width x = Expression (S.of_int ~width x)
  let one  x = Expression (S.one  x)
  let zero x = Expression (S.zero x)
      
  let (+:) a b =
    assert (width a = width b);
    Expression
      { width = width a
      ; value = Operation (Add (a, b))
      }
  ;;

  let (+:.) a b =
    let width = width a in
    let b = of_int ~width b in
    Expression
      { width
      ; value = Operation (Add (a, b))
      }
  ;;

  let (-:) a b =
    assert (width a = width b);
    Expression
      { width = width a
      ; value = Operation (Sub (a, b))
      }
  ;;

  let (==:) a b =
    assert (width a = width b);
    Expression
      { width = width a
      ; value = Comparator (Eq (a, b))
      }
  ;;

  let (>:) a b =
    assert (width a = width b);
    Expression
      { width = width a
      ; value = Comparator (Gt (a, b))
      }
  ;;

  let var_value var_id = Var_value var_id
end

module Ast = struct
  type expr = Expression.t

  type 'a t =
    | Seq        of (unit t * 'a t)
    | With_var   of 'a with_var
    | For        of for_
    | If         of 'a if_
    | Assign_var of (Var_id.t * Expression.t)
    | Return     of 'a

  and for_ =
    { start : expr
    ; end_  : expr
    ; index : Var_id.t
    ; for_body  : unit t
    }

  and 'a if_ =
    { cond   : expr
    ; then_  : 'a t
    ; else_  : 'a t
    }

  and 'a with_var =
    { var_id        : Var_id.t
    ; initial_value : expr
    ; body          : 'a t
    }

  let rec with_vars bindings body =
    match bindings with
    | [] -> body
    | (var_id, initial_value) :: tl ->
      With_var { var_id; initial_value; body = with_vars tl body }
  ;;

  let rec seq actions last =
    match actions with
    | [] -> last
    | hd:: tl -> Seq (hd, seq tl last)
  ;;
end

include Instructions.Make(Expression)
include Loop ()
include Ref (struct
    type t = Var_id.t
  end)

let rec compile_to_ast : 'a. 'a t -> 'a Ast.t =
  fun program ->
  match program with
  | Return a -> Ast.Return a
  | Then (instr, cont) ->
    begin match instr with
      | New_ref initial_value ->
        let var_id = Var_id.create (Expression.width initial_value) in
        Ast.With_var
          { var_id
          ; initial_value
          ; body = compile_to_ast (cont var_id)
          }

      | Get_ref var_id ->
        compile_to_ast (cont (Expression.var_value var_id))

      | Set_ref (var_id, expr) ->
        Seq (Assign_var (var_id, expr), compile_to_ast (cont ()))

      | For { start; end_; f; } ->
        assert (Expression.width start = Expression.width end_);
        let index = Var_id.create (Expression.width start) in
        Seq (
          For { start; end_; for_body = compile_to_ast (f (Expression.var_value index)); index },
          compile_to_ast (cont ())
        )

      | _ -> raise_s [%message "Incomplete implementation, this should not have happened!"]
    end
;;
