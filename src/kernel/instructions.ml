open Core_kernel
open Instructions_aux

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
  module type Channels     = Channels     with type expr := expr and type 'a t := 'a t
  module type Debugging   = Debugging   with type 'a t := 'a t

  module Ref(Variable : Variable) = struct
    type variable = Variable.t

    (* See documentation for the [Ref] module type for information on why [Get_ref]
     * is not included as an instruction.
     * *)
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
      | If    : if_ -> expr instruction
      | While : while_ -> unit instruction
      | Pass  : unit instruction

    let if_ cond then_ else_ = Then (If { cond; then_; else_ }, return)

    let while_ cond body = Then (While { cond; body }, return )

    let pass = Then (Pass, return)

    let rec pass_n n =
      if n < 0 then (
        raise_s [%message "argument to [pass_n] must be a non-negative integer"]
      ) else if n = 0 then (
        return ()
      ) else (
        Then (Pass, (fun () -> pass_n (n - 1)))
      )
    ;;
  end

  module Channels (Channel : T) = struct
    module Input_channel0 = struct
      type 'a t =
        { to_expression : 'a -> Expression.t
        ; chan          : Channel.t
        }
    end

    module Output_channel0 = struct
      type 'a t =
        { of_expression : Expression.t -> 'a
        ; chan          : Channel.t
        }
    end

    type 'a pipe_ =
      { pipe_args : Pipe_args.t
      ; to_expression : 'a -> Expression.t
      ; of_expression : Expression.t -> 'a
      }

    type 'a instruction +=
      | Pipe           : 'a pipe_                    -> ('a Input_channel0.t * 'a Output_channel0.t) instruction
      | Read_channel   : 'a Input_channel0.t         -> 'a instruction
      | Write_channel  : ('a Output_channel0.t * 'a) -> unit instruction

    module Input_channel = struct
      include Input_channel0

      let of_raw chan = { chan; to_expression = Fn.id }
      let read chan = Then (Read_channel chan, return)
    end

    module Output_channel = struct
      include Output_channel0

      let of_raw chan = { chan; of_expression = Fn.id }
      let write chan value = Then (Write_channel (chan, value), return)
    end
    
    let pipe pipe_args = Then (Pipe { pipe_args; to_expression = Fn.id; of_expression = Fn.id }, return)

    module Pipe(M : Hardcaml.Interface.S) = struct
      let indices_and_width =
        (M.scan M.port_widths ~init:0 ~f:(fun acc w -> (acc + w, (acc + w, w))))
      ;;

      let to_expression (expr : Expression.t M.t) =
        M.iter2 expr M.port_widths ~f:(fun e w ->
            assert (Expression.width e = w));
        M.to_list expr
        |> Expression.concat_lsb
      ;;

      let of_expression expr =
        M.map indices_and_width ~f:(fun (i, w) ->
            Expression.select expr (i + w - 1) i)
      ;;

      let pipe pipe_args = Then (Pipe { pipe_args; to_expression; of_expression }, return)
    end
  end


  module Debugging_ignore() = struct
    let debugf fmt = Caml.Printf.ikfprintf (fun _ -> return ()) Stdio.stdout fmt
  end

  module Debugging_stdout() = struct
    let debugf fmt = Printf.kfprintf (fun _ -> return ()) Stdio.stdout fmt
  end
end
