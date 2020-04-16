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
module Problem03 =

    P -> A (P) A
    P -> B (P) B
    P -> BAR


  type Tokens = A | B | BAR

  type Tree =
  | BR_A of Tree
  | BR_B of Tree
  | BR_Empty

  let rec tokenizer = function
  | "" -> []
  | str ->
    let t = match str.Chars 0 with
            | 'a' | 'A' -> A
            | 'b' | 'B' -> B
            | '|' -> BAR
            | v -> failwith (sprintf "Letters other than a, b, and the vertical bar are not supported. Received '%A'" v)
    t :: tokenizer(str.Substring 1)

  let rec parser = function
  | [] -> failwith "No tokens provided"
  | A::xs -> let (tree, tail) = P
             let tail = tail |> eat A
             BR_A(tree)
  | B::xs -> let (tree, tail) = P
             let tail = tail |> eat B
             BR_B(tree)
  | BAR::xs -> xs
  *)

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

  type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)
  let rec upfrom n = Cons(n, fun () -> upfrom(n+1))

  let rec filter f (Cons(x, xsf)) =
    if f x then Cons (x, fun () -> filter f (xsf()))
    else filter f (xsf())

  let rec take n (Cons(x, xsf)) =
    if n <= 0 then [] else x :: take (n-1) (xsf ())

  let rec skip n (Cons(x, xsf)) =
    match n with
    | 0 -> upfrom x
    | _ -> skip (n-1) (xsf())

  let is_divisible_stream lst =
    let rec inner acc = function
    | [] -> acc
    | x::xs -> let mul = filter (fun z -> (z % x) = 0) acc
               inner mul xs
    inner (upfrom 1) lst

  let is_divisible xs =
    let rec inner sx = function
    | [] -> sx
    | x::xs -> inner (Seq.filter (fun n -> (n % x) = 0) sx) xs
    | _ -> failwith "Improper number of list elements"

    let seq = Seq.initInfinite (fun idx -> idx + 1)

    inner seq xs

  let test () =
    printfn "-- Problem 07 --"

    is_divisible_stream [2;3;21;10] |> skip 20 |> take 10 |> printfn "%A"
    is_divisible [2;3;21;10] |> Seq.skip 20 |> Seq.take 10 |> printfn "%A"

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
    let rec inner (x, y) = function
    | 0 -> 0
    | 1 -> 1
    | i ->
      inner ((x+y),(i+1)) (i-1)
    inner (0, 1) n

  let rec fib_imp = function
  | 0 -> 0
  | 1 -> 1
  | n ->
    let fib1 = ref 0
    let fib2 = ref 1
    let res = ref 0
    let count = ref n

    while !count >= 2 do
      res := !fib1 + !fib2
      fib1 := !fib2
      fib2 := !res
      count := !count - 1

    !res

  let test () =
    printfn "-- Problem 11 --"
    let timer = new Stopwatch()
    timer.Start ()
    fib_imp 2000000 |> ignore
    timer.Stop ()

    printfn "Time of imperative = %A" timer.ElapsedMilliseconds

    let timer = new Stopwatch()
    timer.Start ()
    fib 2000000 |> ignore
    timer.Stop ()

    printfn "Time of recursive = %A" timer.ElapsedMilliseconds

(*
  Using Imperative F#, create a record type for a student. The record will
  have a function that returns the student's GPA, a function that adds credit hours
  and a function that adds grade points. Initialize an instance of the record
  with appropriate functions and values. Use the instance to add grade points and
  credits several times, and display the GPA.
*)

module Problem12 =

  type Student =
    {
      credit_hours: int ref;
      grade_points: float list ref;
    }
    member this.AddGradePoints points =
      this.grade_points := points :: !this.grade_points
    member this.AddCreditHours hours =
      this.credit_hours := !this.credit_hours + hours
    member this.GPA () =
      let final_grade = List.reduce (fun acc elem -> acc + elem) !this.grade_points
      final_grade/(float) (List.length !this.grade_points)

  let test () =
    printfn "-- Problem 12 --"
    let student: Student = { credit_hours = ref 0; grade_points = ref [] }

    student.AddCreditHours 4
    student.AddGradePoints 3.01

    student.AddCreditHours 1
    student.AddGradePoints 2.90

    student.AddCreditHours 4
    student.AddGradePoints 3.40

    student.AddCreditHours 3
    student.AddGradePoints 2.76

    printfn "Student is: %A" student
    student.GPA () |> printfn "Student's GPA: %A"

(*
  An interesting use of first-class functions and ref cells in F# is to create
  a monitored version of a function:

  > let makeMonitoredFun f =
        let c = ref 0
        (fun x -> c := !c+1; printf "Called %d times.\n" !c; f x);;

    val makeMonitoredFun : ('a -> 'b) -> ('a -> 'b)

  > let msqrt = makeMonitoredFun sqrt;;
    val msqrt : (float -> float)

  > msqrt 16.0 + msqrt 25.0;;
    Called 1 times.
    Called 2 times.
    val it : float = 9.0


  First, explain why F# does not allow the following declaration:

    let mrev = makeMonitoredFun List.rev

  Now suppose we rewrite the declaration using the technique of eta expansion:

    let mrev = fun x -> (makeMonitoredFun List.rev) x

  Does this solve the problem? Explain why or why not.
*)
module Problem15 =

  let makeMonitoredFun f =
    let c = ref 0
    (fun x -> c := !c+1; printf "Called %d times.\n" !c; f x)

  let mrev = makeMonitoredFun List.rev
  let mrev2 = fun x -> (makeMonitoredFun List.rev) x

  let test () =
    printfn "-- Problem 15 --"
    mrev [1..10] |> printfn "%A"
    mrev2 [1..10] |> printfn "%A"

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
    | NUM n -> if n = 1 then interp(e1)
               else if n = 0 then interp(e2)
               else ERROR (sprintf "'if' statement integer value must be 0 or 1, not '%A'" n)
    | v -> ERROR (sprintf "'if' statement needs a boolean argument, not '%A'" v)
  | ISZERO -> ISZERO

  let interpfile filename = filename |> parsefile |> interp

  let interpstr sourcecode = sourcecode |> parsestr |> interp

  let test () =
    printfn "-- Problem 16 --"

    interpstr "succ 0" |> printfn "%A"
    interpstr "succ 1" |> printfn "%A"
    interpstr "pred 0" |> printfn "%A"
    interpstr "pred 10" |> printfn "%A"
    interpstr "succ (succ (succ 0))" |> printfn "%A"
    interpstr "iszero succ" |> printfn "%A"
    interpstr "succ pred 7" |> printfn "%A"
    interpstr "succ (pred 7)" |> printfn "%A"
    interpfile "src/if.txt" |> printfn "%A"
    interpfile "src/complex1.txt" |> printfn "%A"
    interpfile "src/complex2.txt" |> printfn "%A"
    interpfile "src/complex3.txt" |> printfn "%A"
    interpfile "src/complex4.txt" |> printfn "%A"

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
    [<Measure>] type Milliseconds = Seconds^3
    [<Measure>] type Microseconds = Seconds^6
    [<Measure>] type Nanoseconds = Seconds^9

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
