///
///
///

namespace ProblemSet2

(*
  To this end, define an F# function curry f that converts an uncurried
  function to a curried function, and an F# function uncurry f that does
  the opposite conversion.
*)
module Problem01 =

  let uncurry f = (fun (a, b) -> f a b)

  let curry f = (fun a -> fun b -> f (a, b))

  let test () =

    printfn "-- Problem 01 --"

    let add a b = a + b

    let plus = uncurry add

    printfn "%A" (plus (2, 3))

    let cplus = curry plus
    let plus3 = cplus 3
    printfn "%A" (plus3 10)

(*
  Create a discriminated union for Coordinates that can be a Tuple, Threeple or
  Fourple that represent tuples of size two, three and four. The type for the
  union should be polymorphic.

  Instantiate a Tuple of integers, a Threeple of floats and a Fourple of strings.

  Create a function that has a parameter of a binary function and Coordinate.

  Apply the function to the Coordinate like List.reduce.

  Call the function with (+) for each of the Coordinates in part (b).

  Call the function with (-) for the numeric Coordinates in part (b).

  Be sure that your function implements the normal associativity for (-).
*)
(*
module Problem02 =

  type Coordinate<'T> =
    | Tuple of 'T * 'T
    | Triple of 'T * 'T * 'T
    | Quadruple of 'T * 'T * 'T * 'T
    static member reduce f coord: 'T =
      match coord with
      | Tuple (x, y) -> f(y, f(x, Unchecked.defaultof<'T>))
      | Triple (x, y, z) -> f(z, f(y, f(x, Unchecked.defaultof<'T>)))
      | Quadruple (w, x, y, z) -> f(z, f(y, f(x, f(w, Unchecked.defaultof<'T>))))

  let test () =
    printfn "-- Problem 02 --"
    let tupl = Coordinate.Tuple (1, 2)
    let tripl = Coordinate.Triple (1.0, 2.0, 3.0)
    let quadrpl = Coordinate.Quadruple ("0", "1", "2", "3")

    printfn "%A" tupl
    printfn "%A" tripl
    printfn "%A" quadrpl

    printfn "%A" <| Coordinate.reduce (fun acc elem -> acc + elem) tupl
    printfn "%A" <| Coordinate.reduce (fun acc elem -> acc - elem) tupl

    printfn "%A" <| Coordinate.reduce (fun acc elem -> acc + elem) tripl
    printfn "%A" <| Coordinate.reduce (fun acc elem -> acc - elem) tripl

    printfn "%A" <| Coordinate.reduce (fun acc elem -> acc + elem) quadrpl
    printfn "%A" <| Coordinate.reduce (fun acc elem ->
                      String.collect (fun ch ->
                        if ch <> char(elem)
                        then string(ch)
                        else ""
                      ) acc
                    ) quadrpl

(*
  Print an accept message when the input is valid and completely consumed.
  Generate appropriate error messages for incorrect symbols, not enough input,
  and too much input.
*)
module Problem03 =
  type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

  let S = function
  | tok::toks ->
    let next = List.head toks
    if tok = IF then

    else if tok = BEGIN then

    else if tok = PRINT

    else
      failwith "Program missing valid start symbol"
  | _ -> failwith "Invalid symbol encountered"

  let L = function
  | tok::toks ->
  | _ -> failwith "Program is too _"

  let test_program program =
    let res = program |> S
    match result with
    | [] -> failwith "Early termination or missing EOF"
    | x::xs ->
      if x = EOF then
        printfn "Syntax tree accepted. A program is you."
      else
        failwith "Syntax tree rejected. Not a program is you."

  let test () =
    printfn "-- Problem 03 --"
    [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
    |> test_program
    [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    |> test_program
    [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
    |> test_program


(*
  Implement a syntax checker using functional programming and immutable data
  for the unambiguous grammar for arithmetic expressions, from the Notes on
  Programming Language Syntax.
*)
module Problem04 =

  let test () =
        printfn "-- Problem 04 --"
*)
(*
  Given vectors u = (u1, u2,..., un) and v = (v1, v2,..., vn),
  the inner product of u and v is defined to be u1*v1 + u2*v2 + ... + un*vn.
  Write a curried F# function inner that takes two vectors represented as
  int list and returns their inner product.
*)
module Problem05 =
  (* let rec sigma = function
  | ([], []) | (_, []) | ([], _) -> 0I
  | ([x], [y]) -> x * y
  | (x::xs, y::ys) -> (x * y) + sigma (xs, ys) *)

  let rec sigma acc = function
  | xs, ys when List.length xs <> List.length ys ->
    failwith "Exception: Lists are of unequal length"
  | ([], []) | (_, []) | ([], _) -> acc
  | (x::xs, y::ys) -> sigma ((x * y) + acc) (xs, ys)

  let rec inner xs ys = sigma 0I (xs, ys)

  let test () =
    printfn "-- Problem 05 --"
    printfn "%A" <| inner [1I..50000I] [50001I..100000I]

module Problem06 =
  open ProblemSet1

  let private reducer = (fun acc elem -> acc + elem)

  let rec private inner acc = function
  | xs, ys when (List.length xs) <> (List.length ys) ->
    failwith "Exception: Lists are of unequal length"
  | [], [] -> acc
  | x::xs', y::ys' ->
    inner ([List.reduce reducer x; List.reduce reducer y]::acc) (xs', ys')

  let multiply xs ys = inner [] (xs, (Problem24.transpose ys))

  let test () =
    printfn "-- Problem 06 --"
    printfn "%A" <| multiply [[1;2;3];[4;5;6]] [[0;1];[3;2];[1;2]]
    // expected [[9; 11]; [21;26]]

module Problem08 =
  (*          https://lukemerrett.com/timing-a-function-in-fsharp/            *)
  open System.Diagnostics

  type TimedOperation<'T> = {
    millisecondsTaken: System.TimeSpan;
    returnedValue: 'T
  }

  let timeOperation<'T> (func: unit -> 'T): TimedOperation<'T> =
    let timer = new Stopwatch()
    timer.Start()
    let returnValue = func()
    timer.Stop()
    { millisecondsTaken = timer.Elapsed;
      returnedValue = returnValue }
  (*                                                                          *)

  let rec fold f a = function
  | []    -> a
  | x::xs -> fold f (f a x) xs

  let rec foldBack f xs a =
    match xs with
    | []    -> a
    | y::ys -> f y (foldBack f ys a)

  let flatten1 xs = fold (@) [] xs
  let flatten2 xs = foldBack (@) xs []

  let test () =
    let xs = [[0];[1];[2];[3];[4];[5];[6];[7];[8];[9];[10];[11];[12];[13];[14];[15];[16];[17];[18];[19];]

    let t1 = timeOperation (fun () -> flatten1 xs)
    let t2 = timeOperation (fun () -> flatten2 xs)

    printfn "-- Problem 08 --"

    printfn "Value of t1: %A" t1.returnedValue
    printfn "Elapsed time of flatten1: %A" t1.millisecondsTaken
    printfn "Value of t2: %A" t2.returnedValue
    printfn "Elapsed time of flatten2: %A" t2.millisecondsTaken
    printfn "Elapsed time of flatten1 vs flatten2: %A" (t1.millisecondsTaken - t2.millisecondsTaken)

module Problem09 =
  let rec check_list = function
  | [] -> None
  | [x] -> Some(sprintf "%A" x)
  | _::xs -> check_list xs

  let printNone xs = printfn "The last element of %A is %s." xs "\"Invalid Input\""
  let printSome xs x = printfn "The last element of %A is %s." xs x

  let test () =
    printfn "-- Problem 09 --"
    let list1 = []
    let list2 = ["cat"]
    let list3 = [1..5]

    match check_list list1 with
    | None -> printNone list1
    | Some(x) -> printSome list1 x

    match check_list list2 with
    | None -> printNone list2
    | Some(x) -> printSome list2 x

    match check_list list3 with
    | None -> printNone list3
    | Some(x) -> printSome list3 x

(*
module Problem10
  let test () =
*)

module Problem11 =
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

  let test () =
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

    printfn "-- Problem 11 --"
    printfn "The record instant requested is %A" student

module Problem12 =

  type BTree<'a> =
    | Empty
    | Node of value: 'a * left: BTree<'a> * right: BTree<'a>
    static member remove value tree =
      let rec rimraf v = function
      | Empty -> Empty
      | Node(v', left, right) when v < v' ->
        let left' = rimraf v left
        Node(v', left', right)
      | Node(v', left, right) when v > v' ->
        let right' = rimraf v right
        Node(v', left, right')
      | Node(_, Empty, Empty) -> Empty
      | Node(_, left, Empty) -> left
      | Node(_, Empty, right) -> right
      | Node(_, left, right) ->
        let (Node(v', _, _)) = BTree.predecessor left
        let left' = rimraf v' left
        Node(v', left', right)

      rimraf value tree
    static member private predecessor tree =
      match tree with
      | Empty -> Empty
      | Node (_, _, Empty) -> tree
      | Node (_, _, right) -> BTree.predecessor right


  let test () =
    printfn "-- Problem 12 --"

    let tree =
      Node(10,
        Node(5,
          Node(2, Empty, Empty),
          Node(7, Empty, Empty)
        ),
        Node(20,
          Node(15, Empty, Empty),
          Node(30,
            Node(17, Empty, Empty),
            Node(25, Empty, Empty)
          )
        )
      )

    printfn "Removing element 30 from tree %A" tree
    printfn "%A" <| BTree.remove 30 tree

(*
module Problem13
  let test () = *)
