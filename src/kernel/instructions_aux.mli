module Pipe_args : sig
  type t =
    { handshake : [ `Fire_and_forget | `Blocking ]
    }
end


