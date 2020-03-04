///
///
///

namespace Io.QuinnJR

module ProblemSet1

  module Problem17

    let revlists xs =
      List.map (fun l -> List.rev l) xs

  module Problem18
    let interleave xs ys =
      let rec loop a b acc =
        match a, b with
        | e, [] | [], e -> List.rev acc @ e
        | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

      loop xs ys []


[<EntryPoint>]
let main args =

open ProblemSet1.Problem17
open ProblemSet1.Problem18

[<EntryPoint>]
let main _ =
  printf "-- Problem Set 1 --\n\n"
  printf "-- Problem 17 --"

  let rev = revlists [[0;1;1];[3;2];[];[5]]

  printfn "%A" rev
  0
