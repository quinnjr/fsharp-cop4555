
module ProblemSet3

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

  type 'a alternatingStream = Nil | Cons of 'a * (unit -> 'a stream)

  let rec alternating f =
    Cons((-1.0**(float)f)/(2**(float)f), alternating (f + 1))

  let test () =

    alternating1 |> Seq.skip 5 |> Seq.take 10 |> printfn "%A"
    // printfn "%A" (alternating 10)
