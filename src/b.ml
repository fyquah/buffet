open Core_kernel

module Progam(Instruction : T2) = struct
  module T = struct
    type 'a t =
      | Return : 'a                                        -> 'a t
      | Then   : (('a, 'a t) Instruction.t * ('a -> 'b t)) -> 'b t

    let return a = Return a

    let rec (>>=) t f =
      match t with
      | Return a -> f a
      | Then (a, fa) ->
        Then (a, (fun a -> fa a >>= f))
    ;;

    let bind a ~f = (>>=) a f

    let map = `Define_using_bind

    let singleton (instr : _ Instruction.t) = Then (instr, return)
  end

  include T
  include Monad.Make(T)
end

module Stack_instruction = struct
  type ('a, _) t =
    | Push : int -> (unit, _) t
    | Pop  : (int, _) t
end

module The_program = struct
  include Progam(Stack_instruction)

  let singleton (instr : _ Stack_instruction.t) =
    Then (instr, return)
  ;;

  let push x = singleton (Stack_instruction.Push x)
  let pop    = singleton Stack_instruction.Pop
end


let the_program =
  let open The_program in
  let open The_program.Let_syntax in
  let%bind () = push 1 in
  let%bind a  = pop in
  printf "%d" a;
  return ()
;;

module Parser_instruction = struct
  type ('a, 'b) t =
    | Symbol : (char, _) t
    | M_zero : ('a, _)   t
    | M_plus : ('b * 'b) -> ('a, 'b) t
end

module The_parser = struct
  include Progam(Parser_instruction)

  let symbol        = Then (Symbol, return)
  let mzero         = Then (M_zero, return)
  let mplus a b     = singleton (M_plus (a, b))
end

let the_parser =
  let open The_parser in
  let open The_parser.Let_syntax in
  let is_one =
    let%bind a = symbol in
    if Char.equal '1' a
    then return 1
    else mzero
  in
  let%bind number = is_one in
  return (number + 3)
;;

let rec interpret (the_parser : 'a The_parser.t) (characters : char list) =
  match the_parser with
  | Return a -> [a]
  | Then (Symbol, f) ->
    (match characters with
     | hd :: tl -> interpret (f hd) tl
     | _ -> [])
  | Then (M_zero, _f) -> []
  | Then (M_plus (pa, pb), f) -> 
    let open The_parser in
    interpret (pa >>= f) characters
    @ interpret (pb >>= f) characters
;;

let () =
  interpret the_parser [ 'a'; 'b'; 'c' ]
  |> [%sexp_of: int list]
  |> Stdio.print_s
;;


