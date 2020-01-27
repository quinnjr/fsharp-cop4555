// Write a function that tests whether a string is a palindrome.

open System

let rec palindrome (str: char list) =
  if str.Length = 1 then true
  else
    match str with
    | [] -> true
    | head::tail ->
      let last = tail.Item(tail.Length - 1)
      if head <> last then false else palindrome (tail.[.. (tail.Length - 2)])

[<EntryPoint>]
let main args =
  printf "Input a word to check as being a palindrome: "

  let word = Console.ReadLine()
  let word_list = [ for c in word -> c ]

  let res = palindrome word_list

  if res then printfn "%s is a palindrome." word
  else printfn "%s is not a palindrome." word

  0
