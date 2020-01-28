// Write an F# function shuffle xs that takes an even-length list, cuts it into
// two equal-sized pieces, and then interleaves the pieces:

let interleave xs ys =
  let rec loop a b acc =
    match a, b with
    | e, [] | [], e -> List.rev acc @ e
    | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

  loop xs ys []

let rec gencut (i: int) (xs: int list) =
  let rec accumulator = function
  | 0, a, b -> (List.rev a, b)
  | n, a, [] -> a, []
  | n, a, bhead::btail -> accumulator ((n-1), (bhead::a), btail)

  accumulator (i, [], xs)

let cut (input: int list) = gencut ((List.length input / 2)) input

let shuffle xs =
  let ha, hb = cut xs
  interleave ha hb

[<EntryPoint>]
let main args =
  let initial = [1;2;3;4;5;6;7;8]

  let shuffled = shuffle initial

  printfn "Initial list: %A" initial
  printfn "Final list: %A" shuffled

  0
