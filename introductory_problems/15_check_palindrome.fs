// Write a function that tests whether a string is a palindrome.

open System

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
  let word_list = [ for c in word -> c ]

  printfn "%A" word_list

  match palindrome word_list with
  | true -> printfn "%s is a palindrome."
  | false -> printfn "%s is not a palindrome."

  0
