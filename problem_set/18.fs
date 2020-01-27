// Write an F# function interleave(xs,ys) that interleaves two lists

let interleave xs ys =
  let rec loop a b acc =
    match a, b with
    | [], e | e, [] -> acc @ [e]
    | ahead::atail, bhead::btail -> loop atail btail (b :: a :: acc)
  loop xs ys []

[<EntryPoint>]
let main args =

  let list1 =
  let list2 =

  let list_final = interleave list1 list2

  printfn "First List: %A\nSecond List: %A\nFinal List: %A" list1 list2 list_final

  0
