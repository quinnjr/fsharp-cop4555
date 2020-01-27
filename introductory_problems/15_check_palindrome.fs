// Write a function that tests whether a string is a palindrome.

let rec palindrome str =
  match str with
  | [] -> true
  | head::tail when (tail.Item(tail.Length - 1)) <> head -> palindrome tail
  | _ -> false
    // let last = tail.Item(tail.Length - 1)
    // if head <> last then false else palindrome tail

[<EntryPoint>]
let main args =
  printf "Input a word to check as being a palindrome: "
  let word = Console.ReadLine()
