module F := Front_end

module Expression = Hardcaml.Bits

module Instruction : F.Instruction
module Program : F.Program with module Instruction := Instruction

module Ref : F.Ref
  with type expression := Expression.t
   and type 'a program := 'a Program.t
   and type 'a container := 'a

include F.Loop with type expression := Expression.t and type 'a program := 'a Program.t

val interpret : 'a Program.t -> 'a
