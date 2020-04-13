module ProblemSet3

open System.Diagnostics

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
      | [x] -> Cons (x, Empty)
      | x::xs -> Cons (x, (inner xs))
      inner list

  let test () =
    printfn "-- Problem 01 --"
    LinkedList.fromList [0..20] |> printfn "%A"
    LinkedList.fromListNonTail [0..20] |> printfn "%A"

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

module Problem07 =

  let is_divisible xs =
    let rec inner sx = function
    | [] -> sx
    | x::xs -> inner (Seq.filter (fun n -> (n % x) = 0) sx) xs

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
module Problem12 =

  type Student =
    {
      mutable credit_hours: int;
      mutable grade_points: float list;
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

module Problem15 =

  let makeMonitoredFun f =
    let c = ref 0
    (fun x -> c := !c+1; printf "Called %d times.\n" !c; f x)

  let test () =
    (fun x -> (makeMonitoredFun List.rev) x)

module Problem16 =

//  let interp token =

  let test () =
    printfn "-- Problem 16 --"

module Problem18 =

  type Time () =
    static member private Seconds = 1.0
    static member private Milliseconds = 0.0001
    static member private Microseconds = 0.0000001
    static member private Nanoseconds = 0.0000000001
    static member SecondsToMilliseconds (n: float) = n * Time.Milliseconds
    static member SecondsToMicroseconds (n: float) = n * Time.Microseconds
    static member SecondsToNanoseconds (n: float) = n * Time.Nanoseconds
    static member MillisecondsToSeconds (n: float) = n / Time.Milliseconds
    static member MicrosecondsToSeconds (n: float) = n / Time.Microseconds
    static member NanosecondsToSeconds (n: float) = n / Time.Nanoseconds


  let test () =
    printfn "-- Problem 18 --"
    Time.SecondsToMilliseconds 9.0 |> printfn "9 Seconds to Milliseconds: %A"
    Time.SecondsToMicroseconds 9.0 |> printfn "9 Seconds to Microseconds: %A"
    Time.SecondsToNanoseconds 9.0 |> printfn "9 Seconds to Nanoseconds: %A"

    Time.MillisecondsToSeconds 9.0 |> printfn "9 Milliseconds to Seconds: %A"
    Time.MicrosecondsToSeconds 9.0 |> printfn "9 Microseconds to Seconds: %A"
    Time.NanosecondsToSeconds 9.0 |> printfn "9 Nanoseconds to Seconds: %A"

    Time.MillisecondsToSeconds 5000.0
    |> Time.SecondsToMicroseconds
    |> printfn "5000 Milliseconds to Microseconds: %A"

    Time.SecondsToMicroseconds 0.00000009
    |> Time.MicrosecondsToSeconds
    |> Time.SecondsToNanoseconds
    |> printfn "0.00000009 seconds to nanoseconds: %A"
