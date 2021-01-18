open Core_kernel
open Hardcaml

module Middle_end = Hls_middle_end
module Var_id = Middle_end.Var_id
module State_id = Middle_end.State_id
module Cfg = Middle_end.Cfg

type t =
  { state_machine : State_id.t Always.State_machine.t
  ; proc          : Always.t list
  ; variables     : Always.Variable.t Map.M(Var_id).t
  }

val compile : Reg_spec.t -> Middle_end.t -> t
