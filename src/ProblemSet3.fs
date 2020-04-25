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
    | n when n = 1I -> acc
    | n -> inner (acc * 2I) (n-1I)

    if count <> 0I then inner 1I count else 1I

  let test () =
    printfn "-- Problem 08 --"
    calc_power 0I |> printfn "%A"
    calc_power 1I |> printfn "%A"
    calc_power 2I |> printfn "%A"
    calc_power 4I |> printfn "%A"
    calc_power 16I |> printfn "%A"
    calc_power 256I |> printfn "%A"
    calc_power 1024I |> printfn "%A"
    calc_power 32768I |> printfn "%A"
    calc_power 65536I |> printfn "%A"

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

module Problem13 =

  type Stack<'a> = {
    push: 'a -> unit;
    pop: unit -> unit;
    top: unit -> 'a;
    isEmpty: unit -> bool;
  }

  let stack init =
    let stk = ref init
    {
      push = fun n -> stk := n :: (!stk);
      pop = fun () -> stk := List.tail (!stk);
      top = fun () -> List.head (!stk);
      isEmpty = fun () -> List.isEmpty (!stk);
    }

  let factorial n =
    let s = stack [1] // 0! = 1
    for i = 1 to n do
      s.push i

    let out = ref 1 // 0! = 1

    while (s.isEmpty ()) <> true do
      out := !out * (s.top ())
      s.pop ()
    !out

  let test () =
    printfn "-- Problem 13 --"
    factorial 0 |> printfn "%A"
    factorial 1 |> printfn "%A"
    factorial 2 |> printfn "%A"
    factorial 3 |> printfn "%A"
    factorial 4 |> printfn "%A"
    factorial 5 |> printfn "%A"

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
  | REC (e1, e2) -> ERROR "Not yet implemeneted"
  | ID e1 -> ERROR "Not yet implemented"
  | FUN (e1, e2) -> ERROR "Not yet implemented"

  let interpfile filename = filename |> parsefile |> interp

  let interpstr sourcecode = sourcecode |> parsestr |> interp

  let rec subst e x t =
    match e with
    | ERROR s -> ERROR s
    | ID y when y = x -> interp t
    | APP (e1, e2) -> APP (e1, subst e2 x t)
    | FUN (e1, e2) -> FUN(e1, subst e2 x t)
    | IF (b, e1, e2) -> IF (b, subst e1 x t, subst e2 x t)
    | _ -> e

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

    subst (NUM 6) "a" (NUM 3) |> printfn "%A"
    subst (BOOL true) "a" (NUM 3) |> printfn "%A"
    subst SUCC "a" (NUM 3) |> printfn "%A"
    subst (IF (BOOL true, FUN ("a", APP (SUCC, ID "a")), FUN ("b", APP (SUCC, ID "a")))) "a" (NUM 3) |> printfn "%A"

    interpfile "src/twice.txt" |> printfn "%A"

module Problem17 =

  type typ = VARIABLE of string | INTEGER | BOOLEAN | ARROW of typ * typ

  et rec typ2str = function
  | VARIABLE a -> "'" + a
  | INTEGER    -> "int"
  | BOOLEAN    -> "bool"
  | ARROW (ARROW (t1, t2), t3) -> "(" + typ2str (ARROW (t1, t2)) + ") -> " + typ2str t3
  | ARROW (t1, t2) -> typ2str t1 + " -> " + typ2str t2

  let I (t : typ) = t

  let rec unify (t1, t2) =
    let rec replace (a, t) = function
    | VARIABLE b     -> if a = b then t else VARIABLE b
    | ARROW (t1, t2) -> ARROW (replace (a, t) t1, replace (a, t) t2)
    | t1             -> t1

    let rec occurs = function
    | (a, VARIABLE b)          -> (a = b)
    | (a, ARROW (t1, t2))      -> occurs (a, t1) || occurs (a, t2)
    | (a, _)                   -> false

    match (t1, t2) with
    | (VARIABLE a, t) ->
        if t = VARIABLE a then I                  //noting to do
        elif occurs (a, t) then                   //causes infinite recursion
          failwith (sprintf "circularity: cannot unify %A and %A" a t)
        else replace (a, t)                       //subsitute all Variable a with t
    | (t, VARIABLE a)    -> unify (VARIABLE a, t) //a is a variable, t is not, unify t to a
    | (INTEGER, INTEGER) -> I                     //nothing to do
    | (BOOLEAN, BOOLEAN) -> I                     //nothing to do
    | (ARROW (t3, t4), ARROW (t5, t6)) ->
        let s1 = unify (t3, t5)                   //unify left side
        let s2 = unify (s1 t4, s1 t6)             //unify right with new left
        s2 << s1                                  //compose s1 to s2
    | _ -> failwith (sprintf "mismatch: cannot unify %A and %A" t1 t2)

  let emptyenv x = failwith ("identifier " + x + " is unbound")

  let update env (x : string) (t : typ) = fun y ->
      if y = x then t                             //return new value for x
      else env y                                  //return value from env

  // We start by building the infinite stream of all type variables.
  type 'a stream = Cons of 'a * (unit -> 'a stream)

  //type variables only have one letter
  let rec upfrom n =
    let letters = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
                   "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]
    Cons (VARIABLE (letters.Item (n % 26) + if n/26 = 0 then "" else string (n/26)),
          fun () -> upfrom (n+1))

  let alltypevars = upfrom 0

  /// newtypevar() generates a new type variable, and reset() resets back to 'a.
  let (newtypevar, reset) =                       //returns tuple
    let vars = ref alltypevars                    //mutable
    let hd (Cons (x, xsf)) = x
    let tl (Cons (x, xsf)) = xsf()
    ((fun () ->
        let next = hd (!vars) in vars := tl (!vars)
        next),                                    //next available var
     (fun () -> vars := alltypevars))             //reset to all vars

  /// Milner's Algorithm W
  let rec W (env, e) =
    match e with
    | NUM n  -> (I, INTEGER)
    | BOOL b -> (I, BOOLEAN)
    | IF (e1, e2, e3) ->
        let (s1, t1) = W (env, e1)
        let s2 = unify (t1, BOOLEAN)
        let (s3, t2) = W (s2 << s1 << env, e2)
        let (s4, t3) = W (s3 << s2 << s1 << env, e3)
        let s5 = unify (s4 t2, t3)
        (s5 << s4 << s3 << s2 << s1, s5 t3)

  /// infer e finds the principal type of e
  let infer e =
    reset ();
    let (s, t) = W (emptyenv, e)
    printf "The principal type is\n %s\n" (typ2str t)

  let test () =
    infer (NUM 12)
    infer (BOOL true)
    infer (IF(BOOL true, NUM 1, NUM 2))
    infer (IF(BOOL true, IF(BOOL true, NUM 1, NUM 2), IF(BOOL false, NUM 3, NUM 4)))

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

(*
module ProblemEx1 =
  type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF
  type OPERATION = ADD|SUB|MUL|LPAREN|RPAREN

  let advance toks = List.tail toks

  let eat t toks =
    if ((List.head toks) = t) then
      advance toks
    else
      failwith "Invalid syntax supplied"

  let rec S = function
  | tok::toks ->
    match tok with
    | IF ->
      let next = advance toks
      next |> eat(THEN) |> S |> eat(ELSE) |> S
    | BEGIN ->
      let next = advance toks
      S next |> L
    | PRINT ->
      let next = advance toks
      E next
    else
      failwith "Program missing valid start symbol"
  | _ -> failwith "Invalid symbol encountered"

  let rec L = function
  | tok::toks ->
    match tok with
    | END ->
      advance toks
    | SEMICOLON ->
      advance toks |> S |> L
    | _ -> failwith "Program is too short"
  | _ -> failwith "Invalid syntax"

  let rec E = function
  | tok::toks ->
    eat ID toks

  let test_program program =
    let res = program |> S
    match res with
    | [] -> failwith "Early termination or missing EOF"
    | x::xs ->
      if x = EOF then
        printfn "Syntax tree accepted. A program is you."
      else
        failwith "Syntax tree rejected. Not a program is you."

  let test () =
    printfn "-- Problem 03 --"
    test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
    test_program [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
    test_program [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
	  test_program [ID;SUB;ID;MUL;ID;EOF]
	  test_program [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF] *)

module ProblemEx3 =

  let rev list =
    let rec inner acc = function
    | [] -> acc
    | x::xs -> inner (x::acc) xs
    inner [] list

  let map f list =
    let rec inner acc = function
    | [] -> rev acc
    | x::xs -> inner (f(x)::acc) xs
    inner [] list

  let test () =
    printfn "-- Problem EX 3 --"
    let timer = new Stopwatch()
    timer.Start ()
    map (fun x -> x + 1) [1;2;3;4;5] |> printfn "%A"
    timer.Stop ()
    printfn "Time of my List.map is = %A" timer.ElapsedMilliseconds

    let timer = new Stopwatch()
    timer.Start ()
    List.map (fun x -> x + 1) [1;2;3;4;5] |> printfn "%A"
    timer.Stop ()
    printfn "Time of FSharp's List.map is = %A" timer.ElapsedMilliseconds

module ProblemEx4 =

  let rev list =
    let rec inner acc = function
    | [] -> acc
    | x::xs -> inner (x::acc) xs
    inner [] list

  let rec rev2 = function
  | [] -> []
  | x::xs -> (rev2 xs) @ [x]

  let test () =
    printfn "-- Problem EX 3 --"
    let timer = new Stopwatch()
    timer.Start ()
    rev [1 .. 1000] |> printfn "%A"
    timer.Stop ()
    printfn "Time of my List.rev is = %A" timer.ElapsedMilliseconds

    let timer = new Stopwatch()
    timer.Start ()
    rev2 [1 .. 1000] |> printfn "%A"
    timer.Stop ()
    printfn "Time of List.rev2 is = %A" timer.ElapsedMilliseconds

(*
  Write a tail recursive function, with an integer parameter n, that
  calculates the sum of the odd numbers less than n.
*)
module ProblemEx7 =

  let sum n =
    let rec inner acc = function
    | 0 -> acc
    | x when (x % 2) <> 0 -> inner (acc + x) (x-1)
    | x when (x % 2) = 0 -> inner acc (x-1)
    inner 0 n

  let test () =
    printfn "-- Problem EX 7 --"
    sum 10 |> printfn "The sum of all odd number less than 10 is: %A"

(*
  Write a tail recursive function, with an integer parameter n, that calculates
  the sum of the multiples of three that are less than n.
*)
module ProblemEx8 =

  let sum n =
    let rec inner acc = function
    | 0 -> acc
    | x when (x % 3) <> 0 -> inner acc (x-1)
    | x when (x % 3) = 0 -> inner (acc + x) (x-1)
    inner 0 n

  let test () =
    printfn "-- Problem EX 8 --"
    sum 10 |> printfn "The sum of all multiples of 3 less than 10 is: %A"

(*
  Write a tail recursive function, with an integer parameter n, that calculates
  the sum of the divisors of n (including n).
*)
module ProblemEx9 =

  let sum n =
    let rec inner acc = function
    | 0 -> acc
    | x when (n % x) = 0 -> inner (acc+x) (x-1)
    | x when (n % x) <> 0 -> inner acc (x-1)
    inner 0 n

  let test () =
    printfn "-- Problem EX 9 --"
    sum 20 |> printfn "The sum of the divisors of 20 is: %A"

module ProblemEx1011121314 =

  type 'a stream = Cons of 'a * (unit -> 'a stream)

  // Generate an infinite sequence for the series 1/(3*n), for n > 0.
  let ProblemEx10 = Seq.initInfinite (fun n -> if n > 0 then (1.0/(3.0* float(n))) else 0.0)
  let ProblemEx11 = Seq.initInfinite (fun n -> if n > 0 then (1.0/(float(n) * float(n))) else 0.0)
  let rec ProblemEx12 n =
    Cons(n, (fun () -> ProblemEx12 (n + 23)))
  let rec ProblemEx13 f =
    Cons(f, (fun () -> ProblemEx13 (f * (2.0/3.0))))
  let rec ProblemEx14 (t, u) =
    Cons((t, u), (fun () -> ProblemEx14 (t + 2, u - 0.01)))

  let test () =
    printfn "-- Problem EX 10, 11, 12, 13, 14 --"
    ProblemEx10 |> printfn "%A"
    ProblemEx11 |> printfn "%A"
    ProblemEx12 10 |> printfn "%A"
    ProblemEx13 10.0 |> printfn "%A"
    ProblemEx14 (5, 10.0) |> printfn "%A"

(*
  Define `thrice f` as f >> f >> f. Predict the values of `twice thrice succ 0`
  and `thrice twice succ 0`.
*)
module ProblemEx15 =

  let thrice f = f >> f >> f
  let twice f = f >> f

  let test () =
    printfn "-- Problem EX 15 --"
    printfn "The value of `twice thrice succ 0` should be 9"
    printfn "The value of `thrice twice succ 0` should be = 8"

module ProblemEx17 =
  module Distance =
    [<Measure>] type Feet
    [<Measure>] type Inch
    [<Measure>] type Meter
    [<Measure>] type Centimeter

    let InchPerFeet = 0.0833333<Inch/Feet>
    let InchPerMeter = 0.0254<Inch/Meter>
    let InchPerCentimeter = 2.54<Inch/Centimeter>
    let MeterPerCentimeter = 100<Meter/Centimeter>
    let MeterPerFeet = 3.28084<Meter/Feet>

    let InchesToFeet n = n * InchPerFeet
    let InchesToMeters n = n * InchPerMeter
    let InchesToCentimeters n = n * InchPerCentimeter
    let FeetToInches n = n / InchPerFeet
    let FeetToMeters n = n / MeterPerFeet
    let CentimetersToInches n = n / InchPerCentimeter
    let MetersToCentimeters n = n * MeterPerCentimeter

  open Distance
  let test () =
    printfn "-- Problem EX 17 --"
    FeetToInches 5.5 |> printfn "5.5 feet is %A inches"
    InchesToFeet 100.0 |> printfn "100 inches is equal to %A feet"
    InchesToCentimeters 100.0 |> printfn "100 inches is equal to %A centimeters"
