// Write an F# function cut xs that cuts a list into two equal parts.

let rec cut in =
  let gencut i xs = function
  | head::tail when i < in.Length -> gencut (i + 1) (head::xs) tail
  | x -> (List.rev xs), x

[<EntryPoint>]
let main args =
  let initial = [1;2;3;4;5;6]

  let final = cut initial

  printfn "Initial list: "
