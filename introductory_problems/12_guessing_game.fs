// Write a guessing game where the user has to guess a secret number. After
// every guess the program tells the user whether their number was too
// large or too small. At the end the number of tries needed should be printed.
// It counts only as one try if they input the same number multiple times
// consecutively.

open System

// Parse input from commandline while gracefully handling invalid
// input.
let rec parse_input () =
  printf "Guess a secret number: "
  match Console.ReadLine() |> System.Int32.TryParse with
  | (true, n) -> n
  | _ -> printfn "Error: could not parse input"; parse_input ()

let rec guess (s: int) (gl: int list) =

  // Add a new value to the guess list if the value is not already
  // present in the list
  let rec new_list i l =
    match l with
    | head::tail -> if i = head then gl else new_list i tail
    | [] -> gl @ [i]

  match parse_input () with
  | i when i = s ->
    printfn "You guessed the secret number!"
    printfn "It took you %d guesses to guess the secret." (gl.Length + 1)
    0
  | i when i < s ->
    printfn "Supplied guess is smaller than the secret. Try again."
    let nl = new_list i gl
    guess s nl
  | i when i > s ->
    printfn "Supplied guess is larger than the secret. Try again."
    let nl = new_list i gl
    guess s nl
  | _ -> failwith "Here be dragons."

[<EntryPoint>]
let main args =
  let rnd = System.Random()
  let secret = rnd.Next(1, 20)

  guess secret []
