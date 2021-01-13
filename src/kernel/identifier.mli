open Core_kernel

module Make(M : sig
    val name : string
  end) : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t

  val to_int : t -> int

  val to_string : t -> string

  val create : unit -> t
end

