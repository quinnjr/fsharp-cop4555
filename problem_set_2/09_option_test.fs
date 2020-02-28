// Project to show the uses of the option type.

type IntOrString = IsInt of int | IsString of string

let rec check_list = function
| [] -> None
| [x] ->
  match x with
  | IsInt(i) -> Some(sprintf "%i" i)
  | IsString(s) -> Some s
| _::xs -> check_list xs

[<EntryPoint>]
let main args =

  let print_check_list xs =
    match check_list xs with
    | None -> printfn "The last element of %A is %s." xs "\"Invalid Input\""
    | Some(x) -> printfn "The last element of %A is %s." xs x

  print_check_list []
  print_check_list ["cat"]
  print_check_list [1 .. 5]

  0
