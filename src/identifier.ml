open Core_kernel

module Make() = struct
  include Int
  let to_int t = t

  let create =
    let r = ref (-1) in
    fun () ->
      Int.incr r;
      !r
  ;;
end
