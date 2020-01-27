// Write an F# function revlists xs that takes a list of lists xs and reverses
// all the sub-lists

let revlists xs =
  List.map (fun l -> List.rev l) xs

[<EntryPoint>]
let main args =
  let rev = revlists [[0;1;1];[3;2];[];[5]]

  printfn "%A" rev
  0
