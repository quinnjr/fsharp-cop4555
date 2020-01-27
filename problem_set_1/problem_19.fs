// Write an F# function cut xs that cuts a list into two equal parts.

let rec cut in =
  let gencut i xs =
    let a = xs.[.. i]
    let b = xs.[(i + 1) ..]
    a, b




[<EntryPoint>]
let main args =
  let initial = [1;2;3;4;5;6]

  let final = cut initial

  printfn "Initial list: "
