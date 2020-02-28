(*
  Record
    - Create a record type for Name, Credits and GPA.
    - Create a record instance with the values "Jones", 109, 3.85.
*)

type Name = {
  First: string;
  Middle: Option<string>;
  Last: string;
  Prefix: Option<string>;
  Suffix: Option<string>;
}

type GPA = {
  Numberical: float;
  Letter: string;
}

type Credits = {
  Hours: int;
}

[<EntryPoint>]
let main _ =
  let student = { Name.First = "Jones", Credits.Hours = 109,
    GPA.Numberical = 3.85 }

  printfn "The record instant requested is %A" Student

  0
