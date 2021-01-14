open Hardcaml

module Recipe_back_end = Ocaml_edsl_back_ends.Recipe_back_end

type t = Cyclesim.t_port_list

let compile program =
  let clock = Signal.input "clock" 1 in
  let start = Signal.input "start" 1 in
  let result =
    Recipe_back_end.compile
      (Reg_spec.create ~clock ())
      program
      start
  in
  let done_ = Signal.output "done_" result.valid in
  let value = Signal.output "value" result.value in
  let circuit =
    Circuit.create_exn ~name:"recipe_circuit" [ done_; value ]
  in
  Cyclesim.create circuit
;;

let run (sim : t) =
  let start = Cyclesim.in_port sim "start" in
  let done_ = Cyclesim.out_port sim "done_" in
  let value = Cyclesim.out_port sim "value" in
  start := Bits.vdd;
  Cyclesim.cycle sim;
  start := Bits.gnd;

  while Bits.is_gnd !done_ do
    Cyclesim.cycle sim;
  done;

  !value
;;
