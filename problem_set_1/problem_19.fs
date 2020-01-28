// Write an F# function cut xs that cuts a list into two equal parts.

let rec gencut (i: int) (xs: int list) =
  match xs with
  | head::tail ->
    let a, b = gencut (i + 1) tail
    head::a, b
  | a -> [], a

let cut (input: int list) = gencut ((List.length input) / 2) input

[<EntryPoint>]
let main args =
  let initial = [1;2;3;4;5;6]

  let final = cut initial

  printfn "Initial list: %A" initial
  printfn "Final list: %A" final

  0
