// Project to show the uses of the option type.

let rec check_list = function
| [] -> None
| [x] -> Some(sprintf "%A" x)
| _::xs -> check_list xs

let printNone xs = printfn "The last element of %A is %s." xs "\"Invalid Input\""
let printSome xs x = printfn "The last element of %A is %s." xs x

[<EntryPoint>]
let main args =

  let list1 = []
  let list2 = ["cat"]
  let list3 = [1..5]

  match check_list list1 with
  | None -> printNone list1
  | Some(x) -> printSome list1 x

  match check_list list2 with
  | None -> printNone list2
  | Some(x) -> printSome list2 x

  match check_list list3 with
  | None -> printNone list3
  | Some(x) -> printSome list3 x

  0
