module Pipe_args = struct
  type t =
    { handshake : [ `Fire_and_forget | `Blocking ]
    }
end

