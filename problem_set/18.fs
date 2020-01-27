// Write an F# function interleave(xs,ys) that interleaves two lists

let interleave xs ys =
  let rec loop a b acc =
    match a, b with
    | [], [] -> acc
    | ahead::atail, bhead::btail -> loop atail btail (b :: a :: acc)
  loop xs ys [] |> List.rev

[<EntryPoint>]
let main args =

  let list1 = [1;2;3]
  let list2 = [4;5;6]

  let list_final = interleave list1 list2

  printfn "First List: %A\nSecond List: %A\nFinal List: %A" list1 list2 list_final

  0
