open Core_kernel
open Hardcaml

module Front_end = Hls_front_end
module Middle_end = Hls_middle_end
module Back_end = Hls_back_end
module Var_id = Middle_end.Var_id

let compile (instructions : _ Front_end.t) =
  fun ~clock ~clear ->
    let ast = Front_end.compile_to_ast instructions in
    let middle_end  = Middle_end.compile ast in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let back_end = Back_end.compile reg_spec middle_end in
    Always.compile back_end.proc;
    Map.map ~f:(fun var -> var.value) back_end.variables
;;
