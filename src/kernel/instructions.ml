open Core_kernel

include Instructions_intf

module Make(Expression : Expression) = struct

  type expr = Expression.t

  type 'a instruction = ..

  module T = struct
    type 'a t =
      | Return : 'a                              -> 'a t
      | Then   : ('a instruction * ('a -> 'b t)) -> 'b t

    let return x = Return x

    let rec bind t ~f =
      match t with
      | Return a -> f a
      | Then (instr, k) ->
        Then (instr, (fun a -> bind (k a [@noinline]) ~f))

    let map = `Define_using_bind
  end

  include T
  include Monad.Make(T)

  let (let*) a f = a >>= f
  let (let+) a f = a >>| f

  module type Variable = Variable with type expr := Expression.t
  module type Ref  = Ref with type expr := expr and type 'a t := 'a t
  module type Join = Join with type 'a t := 'a t
  module type Control_flow = Control_flow with type expr := expr and type 'a t := 'a t
  module type Debugging   = Debugging   with type 'a t := 'a t

  module Ref(Variable : Variable) = struct
    type variable = Variable.t

    (* Confusingly, a [Get_ref] instruction is explicitly exlucded. This makes writing
     * things like
     *
     * {[ while_ (Variable.value a) (
           body
         )
         ]}

       much more nicer, rather than having to bind the value to a temporary ocaml binding
     *)
    type 'a instruction +=
      | New_ref : Expression.t                -> Variable.t   instruction
      | Set_ref : (Variable.t * Expression.t) -> unit         instruction

    let new_ref expr       = Then (New_ref expr, return)
    let set_ref var expr   = Then (Set_ref (var, expr), return)
    let get_ref var        = Variable.get_ref var
  end

  module Join() = struct
    type 'a instruction += 
      | Join : 'a t list -> 'a list instruction

    let join_ progs = Then (Join progs, return)

    let t1 x = x >>| fun p -> `P1 p
    let t2 x = x >>| fun p -> `P2 p
    let t3 x = x >>| fun p -> `P3 p

    let join2 p1 p2 = Then (Join [ t1 p1; t2 p2 ], (function
        | [ `P1 p1; `P2 p2 ] -> return (p1, p2)
        | _ -> assert false))

    let join3 p1 p2 p3 = Then (Join [ t1 p1; t2 p2; t3 p3 ], function
        | [ `P1 p1; `P2 p2; `P3 p3 ] -> return (p1, p2, p3)
        | _ -> assert false)

    let par  (progs : unit t list) = Then (Join progs, (fun _result -> return ()))
    let par2  prog1 prog2       = Then (Join [ prog1; prog2 ], (fun _result -> return ()))
    let par3  prog1 prog2 prog3 = Then (Join [ prog1; prog2; prog3 ], (fun _result -> return ()))
  end

  module Control_flow() = struct
    type if_ =
        { cond  : expr
        ; then_ : expr t
        ; else_ : expr t
        }

    type while_ =
      { cond : expr
      ; body : unit t
      }

    type 'a instruction += 
      | If : if_ -> expr instruction
      | While : while_ -> unit instruction

    let if_ cond then_ else_ = Then (If { cond; then_; else_ }, return)

    let while_ cond body = Then (While { cond; body }, return )
  end

  module Debugging_ignore() = struct
    let debugf fmt = Caml.Printf.ikfprintf (fun _ -> return ()) Stdio.stdout fmt
  end

  module Debugging_stdout() = struct
    let debugf fmt = Printf.kfprintf (fun _ -> return ()) Stdio.stdout fmt
  end
end
