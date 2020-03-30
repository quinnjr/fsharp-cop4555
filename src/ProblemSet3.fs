(*

*)

module ProblemSet3

open System.Diagnostics

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


  let test () =
    let test_list = [0..20];

    printfn "%A" (LinkedList.fromList test_list)

module Problem05 =

  let rec interleave (xs, ys) =
    let rec inner acc = function
    | [], [] -> acc
    | [], _ | _, [] -> failwith "List are not of the same length"
    | x::xs, y::ys -> inner (y::x::acc) (xs, ys)

    inner [] (xs, ys) |> List.rev

  let test () =
    let list1 = [0..5]
    let list2 = [10..15]

    printfn "%A" (interleave (list1, list2))

module Problem06 =

  let alternating1 = Seq.initInfinite (fun f -> (-1.0**(float)f)/(2.0**(float)f))

(*  type 'a alternatingStream = Nil | Cons of 'a * (unit -> 'a stream)

  let rec alternating f =
    Cons((-1.0**(float)f)/(2**(float)f), alternating (f + 1)) *)

  let test () =

    alternating1 |> Seq.skip 5 |> Seq.take 10 |> printfn "%A"
    // printfn "%A" (alternating 10)

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
    let timer = new Stopwatch()
    timer.Start()
    for i in 0I..200I do
      printfn "fib_imp of %A => %A" i (fib_imp i)
    timer.Stop()

    printfn "Time of imperative = %A" timer.ElapsedMilliseconds

    let timer = new Stopwatch()
    timer.Start()
    for i in 0I..200I do
      printfn "fib of of %A => %A" i (fib i)
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

  type Student = {
    credit_hours: int;
    grade_points: list float
  }
  member this.AddGradePoints points =
    this.grade_points = this.grade_points :: points
  member this.AddCreditHours hours =
    this.credit_hours = this.credit_hours + hours
  member this.GPA () =
    let len = List.length this.grade_points
    let final_grade = List.reduce (fun acc elem -> acc + elem) this.grade_points6
    final_grade / (float) len

  let test () =
    let student = Student { credit_hours: 0; grade_points: [] }

    student.AddCreditHours 4
    student.AddGradePoints 3.01

    student.AddCreditHours 1
    student.AddGradePoints 2.90

    student.AddCreditHours 4
    student.AddGradePoints 3.40

    student.AddCreditHours 3
    student.AddGradePoints 2.76

    printfn "Student's GPA: %A" student.GPA ()
