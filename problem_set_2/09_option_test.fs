// Project to show the uses of the option type.

let list_print = function
| (xs, None) -> printfn "The last element of %A is %s." xs "\"Invalid Input\""
| (xs, Some(value)) -> printfn "The last element of %A is %A." xs value

let check_list (xs: 'a list): 'a option =
  if xs.IsEmpty then None else Some(xs.Item(xs.Length - 1))

let rec check_list2 = function
| [] -> None
| [x] -> Some(x)
| x::xs -> check_list2 xs

[<EntryPoint>]
let main args =

  let li1 = []
  let li2 = ["cat";]
  let li3 = [1;2;3;4;5]

  list_print (li1, check_list li1)
  list_print (li2, check_list li2)
  list_print (li3, check_list li3)

  list_print li1, check_list2 li1

  0
