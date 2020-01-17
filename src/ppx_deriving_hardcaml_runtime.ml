open Base

module Array = struct
  include Array

  let for_ length ~f =
    for i = 0 to length - 1 do
      f i
    done
  ;;
end

module Int = Int
module Interface = Interface
module List = List

let concat = String.concat
