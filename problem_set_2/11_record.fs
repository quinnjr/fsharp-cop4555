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
  Numerical: float;
  Letter: string;
}

type Credits = {
  Hours: int;
}

type Student = {
  Name: Name;
  Credits: Credits;
  GPA: GPA;
}

[<EntryPoint>]
let main _ =
  let student: Student = {
    Name = {
      First = "";
      Middle = None;
      Last = "Jones";
      Prefix = None;
      Suffix = None;
    };
    Credits = {
      Hours = 109;
    };
    GPA = {
      Numerical = 3.85;
      Letter = "";
    };
  }

  printfn "The record instant requested is %A" student

  0
