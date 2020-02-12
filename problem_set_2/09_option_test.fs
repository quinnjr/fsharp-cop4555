// Project to show the uses of the option type.

let list_print xs x =
  match x with
  | None -> printfn "The last element of %A is %s." xs "\"Invalid Input\""
  | Some(value) -> printfn "The last element of %A is %A." xs value

let rec check_list (xs: 'a list): 'a =
  match xs with
  | []::_ -> None
  | x::[] -> Some(x)
  | _::xs -> check_list xs

let main args =

  let li1 = []
  let li2 = ["cat";]
  let li3 = [1;2;3;4;5]

  let res1 = check_list li1
  (*
  let res2 = check_list li2
  let res3 = check_list li3
  *)

  list_print li1 res1

  0
