let revlists xs =
  List.map (fun l -> List.rev l) xs

[<EntryPoint>]
let main args =
  let rev = revlists [[0;1;1];[3;2];[];[5]]

  printfn "%A" rev
  0
