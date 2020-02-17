// Write an F# function interleave(xs,ys) that interleaves two lists

module Interleave =
  let interleave xs ys =
    let rec loop a b acc =
      match a, b with
      | e, [] | [], e -> List.rev acc @ e
      | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

    loop xs ys []

open Interleave

[<EntryPoint>]
let main args =

  let list1 = [1;2;3]
  let list2 = [4;5;6]

  let list_final = interleave list1 list2

  printfn "First List: %A\nSecond List: %A\nFinal List: %A" list1 list2 list_final

  0
