// A program to find the largest number in an unsorted list.

// Recursively find the largest value in the list
let rec find_largest (big: int) (list: int list) =
  match list with
  // Reassign to the previously assigned attribute, deleting the
  // value previously store in big, then recurse
  | l::t ->
    let big = if l > big then l else big
    find_largest big t
  // Return big
  | [] -> big

[<EntryPoint>]
let main args =
  let test_list = [5;55;66;101;4;323;67;99;102;53;90;83;67;35;76]

  let result =
    match test_list with
    | l::t -> find_largest l test_list
    | [] -> invalidArg "list" "The list supplied is empty"

  printfn "The largest value is %d" result

  0
