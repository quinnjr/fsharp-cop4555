(*
  Print an accept message when the input is valid and completely consumed.
  Generate appropriate error messages for incorrect symbols, not enough input,
  and too much input.
*)

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

let test_program prog =
  let res = program |> S
  match result with
  | [] -> failwith "Early termination or missing EOF"
  | x::xs -> if x = EOF then accept() else error()

[<EntryPoint>]
let main _ =
  test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
  test_program [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
  test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
