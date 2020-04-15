module ProblemSet3

open System.Diagnostics

open Parser.Parse

(*
  Build a simple tree.
    a. Create a discriminated union that can represent a linked list of integers.
    b. Write a non-tail recursive function that converts a list into a linked
       list of nodes.
    c. Write a tail-recursive function that converts a list into a linked list
       of nodes.
*)
module Problem01 =

  type LinkedList<'a> =
  | Empty
  | Cons of value: 'a * next: LinkedList<'a>

  module LinkedList =

    let rec fold fCons acc list: 'b =
      let recurse = fold fCons
      match list with
      | Empty -> acc
      | Cons(value, list) ->
        let newAcc = fCons acc value
        recurse newAcc list

    let rev list =
      let folder tail head = Cons(head, tail)
      fold folder Empty list

    let foldBack fCons list acc: 'b =
      let fCons' acc elem = fCons elem acc
      list |> rev |> fold fCons' acc

    let fromList list =
      let fCons value next = Cons (value, next)
      List.foldBack fCons list Empty

    let fromListNonTail list =
      let rec inner = function
      | [] -> failwith "Here be dragons"
      | [x] -> Cons (x, Empty)
      | x::xs -> Cons (x, (inner xs))
      inner list

  let test () =
    printfn "-- Problem 01 --"
    LinkedList.fromList [0..20] |> printfn "%A"
    LinkedList.fromListNonTail [0..20] |> printfn "%A"
(*
  Write a tail-recursive F# function interleave(xs,ys) that interleaves two
  lists. Assume that the two lists have the same length.
  Compare the timing of the recursive function from Problem Set 1 with this
  tail-recursive version. Time these examples in both versions.
*)
module Problem05 =

  let rec interleave (xs, ys) =
    let rec inner acc = function
    | [], [] -> acc
    | [], _ | _, [] -> failwith "List are not of the same length"
    | x::xs, y::ys -> inner (y::x::acc) (xs, ys)

    inner [] (xs, ys) |> List.rev

  let test () =
    printfn "-- Problem 05 --"

    printfn "%A" (interleave ([1..2..19999], [2..2..20000]))
    printfn "%A" (interleave ([1..2..199999], [2..2..200000]))

(*
  Generate an infinite sequence for the alternating series of 1/(2**n).
  Display the 5th through 15th numbers in the series. The numbers should
  display as the floating point version of the fractions.
  Repeat the exercise using an infinite stream.
*)
module Problem06 =

  let alternating1 = Seq.initInfinite (fun f -> (-1.0**(float)f)/(2.0**(float)f))

  type 'a stream = Cons of 'a * (unit -> 'a stream)

  let rec alternating f =
    Cons((-1.0**f)/(2.0**f), fun () -> alternating (f + 1.0))

  let rec take n (Cons(x, xf)) =
    if n = 0 then [] else x :: take (n-1) (xf())

  let test () =
    printfn "-- Problem 06 --"
    alternating1 |> Seq.skip 5 |> Seq.take 10 |> printfn "%A"
    alternating 5.0 |> take 10 |> printfn "%A"

(*
  Generate an infinite stream for the the natural numbers greater than zero
  that are divisible by each element in a list of at least one integer.
  Use filters on the infinite stream of natural numbers starting at one.
  Display the 20th through 30th numbers in the series.
  Repeat the exercise using an infinite sequence. Sequences also have a filter
  function, so it can be solved similarly to the infinite stream
  version. Just for fun, try to solve it without using the filter function.
  For both functions, be sure to dislay an appropriate error message if
  the list does not have any elements.
*)
module Problem07 =

  let is_divisible xs =
    let rec inner sx = function
    | [] -> sx
    | x::xs -> inner (Seq.filter (fun n -> (n % x) = 0) sx) xs
    | _ -> failwith "Improper number of list elements"

    let seq = Seq.initInfinite (fun idx -> idx + 1)

    inner seq xs

  let test () =
    printfn "-- Problem 07 --"

    is_divisible [2;3;21;10] |> printfn "%A"

(*
  Create a tail-recursive function that has a big integer as input and
  calculates 2I raised to that power.
*)
module Problem08 =

  let calc_power count =
    let rec inner acc = function
    | 1 -> acc * 2I
    | n -> inner (acc * 2I) (n-1)
    inner 1I count

  let test () =
    printfn "-- Problem 08 --"
    calc_power 5 |> printfn "%A"

(*
 Write a non-recursive fibonacci function using imperative F#. Compare the
 timing with a tail-recursive fibonacci.
*)
module Problem11 =

  let fib n =
    let rec inner (x, y) i =
      if i < n then inner (x+y, x) (i+1I) else x
    inner (0I, 1I) 0I

  // rewrite with ref and pointers
  let fib_imp n =
    Seq.init (int n) id
    |> Seq.fold (fun (x, y) items -> (x+y, x)) (0I, 1I)

  let test () =
    printfn "-- Problem 11 --"
    let timer = new Stopwatch()
    timer.Start()
    for i in 0I..200I do
      fib_imp i |> ignore
    timer.Stop()

    printfn "Time of imperative = %A" timer.ElapsedMilliseconds

    let timer = new Stopwatch()
    timer.Start()
    for i in 0I..200I do
      fib i |> ignore
    timer.Stop()

    printfn "Time of recursive = %A" timer.ElapsedMilliseconds

(*
  Using Imperative F#, create a record type for a student. The record will
  have a function that returns the student's GPA, a function that adds credit hours
  and a function that adds grade points. Initialize an instance of the record
  with appropriate functions and values. Use the instance to add grade points and
  credits several times, and display the GPA.
*)
(*
module Problem12 =

  type Student =
    {
      credit_hours: int;
      grade_points: float list;
    }
    member this.AddGradePoints points =
      this.grade_points <- points :: this.grade_points
    member this.AddCreditHours hours =
      this.credit_hours <- this.credit_hours + hours
    member this.GPA () =
      let len = List.length this.grade_points
      let final_grade = List.reduce (fun acc elem -> acc + elem) this.grade_points
      final_grade/(float) len

  let test () =
    printfn "-- Problem 12 --"
    let student: Student = { credit_hours = 0; grade_points = [] }

    student.AddCreditHours 4
    student.AddGradePoints 3.01

    student.AddCreditHours 1
    student.AddGradePoints 2.90

    student.AddCreditHours 4
    student.AddGradePoints 3.40

    student.AddCreditHours 3
    student.AddGradePoints 2.76

    student.GPA () |> printfn "Student's GPA: %A"
*)
(*
  Refer to the notes on Canvas: lecture notes on the PCF language and implement
  an interpreter that processes programs from the PCF language.
  Use the parser.fsx or parser.fs code to parse the input into an abstract tree.
  You are to write an F# function interp that takes an abstract syntax tree
  represented as a term and returns the result of evaluating it, which will
  also be a term.
*)
module Problem16 =

  let rec interp = function
  | ERROR s -> ERROR (sprintf "An error occurred: %A" s)
  | APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _) | (_, ERROR s) -> ERROR s
    | (NUM n, _) | (_, NUM n) -> if n > 0 then NUM n else NUM 0
    | (BOOL b, _) | (_, BOOL b) -> if b then BOOL true else BOOL false
    | (SUCC, NUM n) -> if n >= 0 then NUM (n+1) else NUM 0
    | (SUCC, v) -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM n) -> if n > 0 then NUM (n - 1) else NUM 0
    | (PRED, v) -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    | (ISZERO, NUM n) -> if n = 0 then NUM 1 else NUM 0
    | (ISZERO, v) -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
    | (REC (e1,e2), _) -> ERROR "Not yet implemeneted"
    | (ID e1, _) -> ERROR "Not yet implemented"
    | (FUN (e1, e2), _) -> ERROR "Not yet implemented"
  | NUM n -> NUM n
  | BOOL b -> BOOL b
  | SUCC -> SUCC
  | PRED -> PRED
  | IF (b, e1, e2) ->
    match interp b with
    | BOOL b -> if b then interp(e1) else interp(e2)
    | _ -> ERROR "if statement was not valid."
  | _ -> failwith "Not implemented"

  let interpfile filename = filename |> parsefile |> interp

  let interpstr sourcecode = sourcecode |> parsestr |> interp

  let test () =
    printfn "-- Problem 16 --"

    interpstr "succ 0" |> printfn "%A"

(*
  Declare type measures for seconds, microseconds, milliseconds, and nanoseconds.
  Declare constants for the number of seconds in each of the other types.
  Create functions that convert seconds to each of the other types. What is the
  principal type of each function?
  Create functions that convert each of the other types to seconds. What is the
  principal type of each function?
  Convert 5000 milliseconds to seconds and then to microseconds.
  Convert 0.00000009 seconds to microseconds and to nanoseconds.
*)
module Problem18 =
  module Time =
    [<Measure>] type Seconds
    [<Measure>] type Milliseconds
    [<Measure>] type Microseconds
    [<Measure>] type Nanoseconds

    let MillisecondsPerSecond = 1000.0<Milliseconds/Seconds>
    let MicrosecondsPerSecond = 1000000.0<Microseconds/Seconds>
    let NanosecondsPerSecond = 1000000000.0<Nanoseconds/Seconds>

    let SecondsToMilliseconds (n: float<Seconds>) = n * MillisecondsPerSecond
    let SecondsToMicroseconds (n: float<Seconds>) = n * MicrosecondsPerSecond
    let SecondsToNanoseconds (n: float<Seconds>) = n * NanosecondsPerSecond
    let MillisecondsToSeconds (n: float<Milliseconds>) = n / MillisecondsPerSecond
    let MicrosecondsToSeconds (n: float<Microseconds>) = n / MicrosecondsPerSecond
    let NanosecondsToSeconds (n: float<Nanoseconds>) = n * NanosecondsPerSecond

  open Time
  let test () =
    printfn "-- Problem 18 --"
    Time.SecondsToMilliseconds 9.0<Time.Seconds> |> printfn "9 Seconds to Milliseconds: %A"
    Time.SecondsToMicroseconds 9.0<Time.Seconds> |> printfn "9 Seconds to Microseconds: %A"
    Time.SecondsToNanoseconds 9.0<Time.Seconds> |> printfn "9 Seconds to Nanoseconds: %A"

    Time.MillisecondsToSeconds 9.0<Time.Milliseconds> |> printfn "9 Milliseconds to Seconds: %A"
    Time.MicrosecondsToSeconds 9.0<Microseconds> |> printfn "9 Microseconds to Seconds: %A"
    Time.NanosecondsToSeconds 9.0<Nanoseconds> |> printfn "9 Nanoseconds to Seconds: %A"

    Time.MillisecondsToSeconds 5000.0<Time.Milliseconds>
    |> Time.SecondsToMicroseconds
    |> printfn "5000 Milliseconds to Microseconds: %A"

    Time.SecondsToMicroseconds 0.00000009<Seconds>
    |> Time.MicrosecondsToSeconds
    |> Time.SecondsToNanoseconds
    |> printfn "0.00000009 seconds to nanoseconds: %A"
