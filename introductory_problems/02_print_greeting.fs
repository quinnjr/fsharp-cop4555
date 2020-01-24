// Print greeting example
//
// Program asks user for input on the command line and awaits input.
// Upon input, Program reads the line.
// If input is sized (more than 0 characters), program prints a greeting message.

[<EntryPoint>]
let main args =
  printfn "What is your name?: "
  // Wait for user input
  let input = System.Console.ReadLine();

  // Match case for proper input
  match input.Length > 0 with
  | true ->
    // Print the greeting message
    printfn "Hello %s!" input
  | false ->
    // Print an error for no proper input
    eprintfn "Too few arguments specified"

  0
