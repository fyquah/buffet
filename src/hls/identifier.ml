open Core_kernel

module Make(M : sig
    val name : string
  end) = struct
  include Int
  let to_int t = t

  let create =
    let r = ref (-1) in
    fun () ->
      Int.incr r;
      !r
  ;;

  let to_string x = sprintf "%s__%d" M.name x
end
