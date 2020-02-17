// Write an F# function cut xs that cuts a list into two equal parts.

module Cut =
  let rec gencut (i: int) (xs: int list) =
    let rec accumulator = function
    | 0, a, b -> (List.rev a, b)
    | n, a, [] -> a, []
    | n, a, bhead::btail -> accumulator ((n-1), (bhead::a), btail)

    accumulator (i, [], xs)

  let cut (input: int list) = gencut ((List.length input / 2)) input

open Cut

[<EntryPoint>]
let main args =
  let initial = [1;2;3;4;5;6]

  let final = cut initial

  printfn "Initial list: %A" initial
  printfn "Final list: %A" final

  0
