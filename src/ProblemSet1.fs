///
///
///

namespace ProblemSet1

(*
  Write an F# function revlists xs that takes a list of lists xs and reverses
  all the sub-lists.
*)
module Problem17 =
  let revlists xs =
    List.map (fun l -> List.rev l) xs

  let test () =
    printfn "-- Problem 17 --"
    printfn "%A" <| revlists [[0;1;1];[3;2];[];[5]]

(*
  Write an F# function interleave(xs,ys) that interleaves two lists.
*)
module Problem18 =
  let interleave xs ys =
    let rec loop a b acc =
      match a, b with
      | e, [] | [], e -> List.rev acc @ e
      | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

    loop xs ys []

  let test () =
    printfn "-- Problem 18 --"

    let list1 = [1;2;3]
    let list2 = [4;5;6]

    let list_final = interleave list1 list2

    printfn "First List: %A\nSecond List: %A\nFinal List: %A" list1 list2 list_final

(*
  Write an F# function cut xs that cuts a list into two equal parts.
*)
module Problem19 =
  let rec gencut (i: int) (xs: int list) =
    let rec accumulator = function
    | 0, a, b -> (List.rev a, b)
    | n, a, [] -> a, []
    | n, a, bhead::btail -> accumulator ((n-1), (bhead::a), btail)

    accumulator (i, [], xs)

  let cut (input: int list) = gencut ((List.length input / 2)) input

  let test () =
    printfn "--Problem 19 --"
    let initial = [1;2;3;4;5;6]
    printfn "Initial list: %A" initial
    printfn "Final list: %A" <| cut initial

(*
  Write an F# function shuffle xs that takes an even-length list, cuts it into
  two equal-sized pieces, and then interleaves the pieces:
*)
module Problem20 =
  open Problem18
  open Problem19

  let shuffle xs =
    let ha, hb = cut xs
    interleave ha hb

  let test () =
    printfn "-- Problem 20 --"
    let initial = [1;2;3;4;5;6;7;8]
    printfn "Initial list: %A" initial
    printfn "Final list: %A" <| shuffle initial

(*
  Write an F# function countshuffles n that counts how many calls
  to shuffle on a deck of n distinct "cards" it takes to put the deck back
  into its original order.
*)
module Problem21 =
  open Problem20

  let rec countaux deck target =
    match deck <> target with
    | false -> 0
    | true -> 1 + countaux (shuffle deck) target

  let countshuffles (n: int) =
    let target = [ 0 .. n ]
    let deck = shuffle target
    1 + countaux deck target

  let test () =
    printfn "-- Problem 21 --"
    printfn "Number of shuffles on a deck of 4 cards: %d" (countshuffles 4)
    printfn "Number of shuffles on a deck of 52 cards: %d" (countshuffles 52)

(*
  Write an uncurried F# function cartesian (xs, ys) that takes as input two
  lists xs and ys and returns a list of pairs that represents the Cartesian
  product of xs and ys. (The pairs in the Cartesian product may appear in any
  order).
*)
module Problem22 =
  let rec cart_rec = function
  | [],_ | _, [] -> []
  | x::xs, ys -> (List.map (fun y -> (x, y)) ys) :: cart_rec (xs, ys)

  let cartesian xs ys = cart_rec (xs, ys)

  let test () =
    printfn "-- Problem 22 --"
    printfn "%A" <| cartesian ["a"; "b"; "c"] [1; 2]

(*
  An F# list can be thought of as representing a set, where the order of the
  elements in the list is irrelevant. Write an F# function powerset such that
  powerset set returns the set of all subsets of set. For example,
*)
module Problem23 =
  let rec powerset = function
  | [] -> [[]]
  | x::xs -> List.collect (fun y -> [y; x::y]) (powerset xs)

  let test () =
    printfn "-- Problem 23 --"
    let pow = List.filter (fun e -> List.length(e) <> 0) (powerset [1..3])
    printfn "%A" pow

(*
  An m-by-n matrix can be represented in F# as a list of m rows,
  each of which is a list of length n. For example, the first matrix
  above is represented as the list [[1;2;3];[4;5;6]].

  Write an efficient F# function to compute the transpose of an m-by-n matrix.
*)
module Problem24 =
  (*
    We work recursively on a matrix list M, looking at the matrix as its
    first list (x::xs) and the tail of the matrix ys.

    If the matrix is empty or the head of the matrix is empty, return an empty
    list.

    If we still have a matrix to transpose, use List.head as the transformation
    function on M to get the xs element by itself. Append the transposistion of
    the tail of M (ys) which has been transormed with List.map.

    Finally, return the new transposed list of lists.
  *)
  let rec transpose = function
  | [] | []::_ -> []
  | (_::_)::_ as M -> List.map List.head M :: transpose(List.map List.tail M)

  let test () =
    printfn "-- Problem 24 --"
    printfn "%A" <| transpose [[1;2;3];[4;5;6]]
