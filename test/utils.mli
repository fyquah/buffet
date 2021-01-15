open Hardcaml

module Recipe_back_end = Buffet_back_ends.Recipe_back_end

type t

val compile : Signal.t Recipe_back_end.t -> t
val run     : t -> Bits.t

