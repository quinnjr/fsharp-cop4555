
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

  let rec interleave xs, ys =
    let rec inner acc = function
    | [], li | li, [] -> acc :: List.rev li
    | x::xs, y::ys -> inner(b :: a :: acc) (xs, ys)
    inner [] a, b |> List.rev

  let test () =
    let list1 = [0..5]
    let list2 = [10..15]

    printfn "%A" (interleave list1, list2)
