module Num = struct
  module Int = struct
    let (+) = Pervasives.(+)
  end
  
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

module String = struct
  let (+) = Pervasives.(^)
end

module Loaded = struct
  module Num = Num
  module String = String
  val %overloaded (+) : 'a -> 'a -> 'a
end

module Test = struct
  open Loaded
  let () = 
    assert (1 + 2 = 3);
    assert (1.2 + 3.4 = 4.6);
    assert ("hello" + "world" = "helloworld");
    prerr_endline "Ok!"
end

let _ = (Loaded.(+) : int -> int -> int)

module Loaded2 = struct
  include (struct
            val %overloaded (+) : 'a -> 'a -> 'a
          end : sig
            val %overloaded (+) : 'a -> 'a -> 'a
          end)
  module Num = Num
  module String = String
end        

let _ = (Loaded2.(+) : int -> int -> int)
