open Hardcaml
open Core_kernel

module Front_end = Hls_front_end
module Middle_end = Hls_middle_end
module Var_id = Middle_end.Var_id

val compile
  : unit Front_end.t
  -> clock: Signal.t
  -> clear: Signal.t
  -> Signal.t Map.M(Var_id).t
