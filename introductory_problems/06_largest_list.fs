// A program to find the largest number in an unsorted list.

// Recursively find the largest value in the list
// - Using a previous largest value and the next index in the list, determine
//   if more values are still in the list to be checked. Return the largest previous
//   value if utside the bounds of the list.
//
// - Check to see if the next item in the list is greater in value than the
//   previous value. If so, call find_largest with the new largest value and the
//   next index.
let rec find_largest (prev: int) (next: int) (list: int list) =
  match next <> list.Length with
  | true ->
    let next_item = list.Item(next)
    match prev > list.Item(next) with
    | true -> find_largest prev (next+1) list
    | false ->
      find_largest next_item (next+1) list
  | false -> prev

[<EntryPoint>]
let main args =
  let test_list = [5;55;66;101;4;323;67;99;102;53;90;83;67;35;76]
  let result = find_largest 0 1 test_list

  printfn "The largest value is %d" result

  0
