///
///
///

namespace ProblemSet2
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
*)
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
  | ([], []) | (_, []) | ([], _) -> acc
  | (x::xs, y::ys) -> sigma ((x * y) + acc) (xs, ys)

  let rec inner xs ys =
    if List.length xs <> List.length ys then
      failwith "Exception: Lists are of unequal length"
    // else sigma (xs, ys)
    else sigma 0I (xs, ys)

  let test () = printfn "%A" <| inner [1I..50000I] [50001I..100000I]


module Problem06 =
  open ProblemSet1.Problem24

  let rec inner xs ys =
    if List.length xs <> List.length ys then
      failwith "Exception: Lists are of unequal length"
    else
      let rec sigma acc = function
      | ([], []) | (_, []) | ([], _) -> acc
      | (x::xs, y::ys) -> sigma ((x * y) + acc) (xs, ys)
      sigma 0 (xs, ys)

  let multiply xs ys = inner xs (transpose ys)

  let test () =
    printfn "%A" <| multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]])

(*
module Problem07
  let test () =
*)
module Problem09
  let rec check_list = function
  | [] -> None
  | [x] -> Some(sprintf "%A" x)
  | _::xs -> check_list xs

  let printNone xs = printfn "The last element of %A is %s." xs "\"Invalid Input\""
  let printSome xs x = printfn "The last element of %A is %s." xs x

  let test () =
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

    printfn "The record instant requested is %A" student

module Problem12

  type BinarySearchTree<'T> =
  | Empty
  | Node of value: 'T * left: BinarySearchTree<'T> * right: BinarySearchTree<'T>
  static member remove (value: 'T) (tree: BinarySearchTree<'T>)  =
    let rec predecessor tr =
      match tr with
      | Empty -> Empty
      | Node (_, _, Empty) -> tree
      | Node (_, _, right) -> predecessor right

    let rec rimraf value tr =
      match tr with
      | Empty -> Empty
      | Node(value', left, right) when value < value' ->
      | Node(value', left, right) when value > value' ->
      | Node(_, Empty, Empty) -> Empty
      | Node(_, left, Empty) -> left
      | Node(_, Empty, right) -> right
      | Node(_, left, right) ->
        let Node(value', _, _) = predecessor left
        let left' = rimraf value' left
        Node(value', left' right)

  let test () =

    let tree = Node(10,
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

    BinarySeachTree.delete 30 tree

(*
module Problem13
  let test () = *)
