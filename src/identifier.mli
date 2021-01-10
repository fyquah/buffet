open Core_kernel

module Make() : sig
  type t

  include Comparable.S with type t := t

  val to_int : t -> int

  val create : unit -> t
end

