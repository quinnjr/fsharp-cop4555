// Write a guessing game where the user has to guess a secret number. After
// every guess the program tells the user whether their number was too
// large or too small. At the end the number of tries needed should be printed.
// It counts only as one try if they input the same number multiple times
// consecutively.

open System

[<EntryPoint>]
let main args =
  let rnd = System.Random()
  let secret = rnd.Next(20)
  let guesses = []

  let rec check_guess i list = function
  | [] -> i @ list
  | head::tail -> if head <> i then check_guess i tail else list

  let loop_me =
    printf "Guess a secret number: "
    let guess = Console.ReadLine() |> int
    match guess with
    | _ -> printfn "You guessed the secret number!"; guesses
    | secret <> guess ->
      if secret < guess then
        printfn "Supplied guess is smaller than the secret. Try again."
      else
        printfn "Supplied guess is larger than the secret. Try again."
      check_guess guess
      loop_me

  loop_me

  0
