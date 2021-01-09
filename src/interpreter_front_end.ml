open Core_kernel

module Expression = Hardcaml.Bits

module rec Program : sig

  type 'a t =
    | Return : 'a                                        -> 'a t
    | Then   : (('a, 'a t) Instruction.t * ('a -> 'b t)) -> 'b t

  include Monad.S with type 'a t := 'a t

end = struct

  module T = struct
    type 'a t =
      | Return : 'a                                        -> 'a t
      | Then   : (('a, 'a t) Instruction.t * ('a -> 'b t)) -> 'b t

    let return a = Return a

    let rec bind t ~f =
      match t with
      | Return a -> f a
      | Then (instr, k) ->
        Then (instr, (fun a -> bind ~f (k a)))
    ;;

    let map = `Define_using_bind
  end

  include T
  include Monad.Make(T)

end 
and Ref : sig

  type t = Expression.t ref

  val create : Expression.t -> t Program.t
  val get    : t -> Expression.t Program.t
  val set    : t -> Expression.t -> unit Program.t

end = struct
  open Program

  type t = Expression.t ref

  let create initial = Then ((Instruction.New_ref initial), return)

  let get r = Then ((Instruction.Get_ref r), return)

  let set r v = Then ((Instruction.Set_ref (r, v)), return)
end
and Instruction : sig

  type for_ =
    { start  : Expression.t
    ; end_   : Expression.t
    ; f      : Expression.t -> unit Program.t
    }

  type ('a, 'b) t =
    | New_ref : Expression.t           -> (Ref.t, 'b) t
    | Get_ref : Ref.t                  -> (Expression.t, 'b) t
    | Set_ref : (Ref.t * Expression.t) -> (unit, 'b) t
    | For     : for_ -> (unit, unit Program.t) t

end = struct

  type for_ =
    { start  : Expression.t
    ; end_   : Expression.t
    ; f      : Expression.t -> unit Program.t
    }

  type ('a, 'b) t =
    | New_ref : Expression.t           -> (Ref.t, 'b) t
    | Get_ref : Ref.t                  -> (Expression.t, 'b) t
    | Set_ref : (Ref.t * Expression.t) -> (unit, 'b) t
    | For     : for_ -> (unit, unit Program.t) t
end

let for_ start end_ f =
  Program.Then (
    For { start; end_; f; },
    Program.return
  )
;;

let rec loop_bits ~start ~end_ f =
  if Expression.(is_vdd ((>:) start end_ )) then
    Program.return ()
  else
    let%bind.Program () = f start in
    loop_bits ~start:(Expression.(+:.) start 1) ~end_ f
;;

let rec interpret (program : _ Program.t) =
  match program with
  | Return a -> a
  | Then (ins, k) ->
    begin match ins with
    | New_ref expression -> interpret (k (ref expression))
    | Get_ref r -> interpret (k r.contents)
    | Set_ref (r, v) ->
      r := v;
      interpret (k ())
    | For { start; end_; f; } ->
      interpret (
        let%bind.Program () = loop_bits ~start ~end_ f in
        k ()
      )
    end

;;
