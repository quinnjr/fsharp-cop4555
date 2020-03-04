///
///
///

namespace io.QuinnJR

module ProblemSet1

  (*
    Write an F# function revlists xs that takes a list of lists xs and reverses
    all the sub-lists.
  *)
  module Problem17
    let revlists xs =
      List.map (fun l -> List.rev l) xs

  (*
    Write an F# function interleave(xs,ys) that interleaves two lists.
  *)
  module Problem18
    let interleave xs ys =
      let rec loop a b acc =
        match a, b with
        | e, [] | [], e -> List.rev acc @ e
        | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

      loop xs ys []

  (*
    Write an F# function cut xs that cuts a list into two equal parts.
  *)
  module Problem19
    let rec gencut (i: int) (xs: int list) =
      let rec accumulator = function
      | 0, a, b -> (List.rev a, b)
      | n, a, [] -> a, []
      | n, a, bhead::btail -> accumulator ((n-1), (bhead::a), btail)

      accumulator (i, [], xs)

    let cut (input: int list) = gencut ((List.length input / 2)) input

  (*
    Write an F# function shuffle xs that takes an even-length list, cuts it into
    two equal-sized pieces, and then interleaves the pieces:
  *)
  module Problem20
    let shuffle xs =
      let ha, hb = cut xs
      interleave ha hb

  (*
    Write an F# function countshuffles n that counts how many calls
    to shuffle on a deck of n distinct "cards" it takes to put the deck back
    into its original order.
  *)
  module Problem21
    let rec countaux deck target =
      match deck <> target with
      | false -> 0
      | true -> 1 + countaux (shuffle deck) target

    let countshuffles (n: int) =
      let target = [ 0 .. n ]
      let deck = shuffle target
      1 + countaux deck target

  (*
    Write an uncurried F# function cartesian (xs, ys) that takes as input two
    lists xs and ys and returns a list of pairs that represents the Cartesian
    product of xs and ys. (The pairs in the Cartesian product may appear in any
    order).
  *)
  module Problem22
    let rec cart_rec = function
    | [],_ | _, [] -> []
    | x::xs, ys -> (List.map (fun y -> (x, y)) ys) :: cart_rec (xs, ys)

    let cartesian xs ys = cart_rec (xs, ys)

  (*
    An F# list can be thought of as representing a set, where the order of the
    elements in the list is irrelevant. Write an F# function powerset such that
    powerset set returns the set of all subsets of set. For example,
  *)
  module Problem23
    let rec powerset = function
    | [] -> [[]]
    | x::xs -> List.collect (fun y -> [y; x::y]) (powerset xs)


open ProblemSet1.Problem17
open ProblemSet1.Problem18
open ProblemSet1.Problem19
open ProblemSet1.Problem20
open ProblemSet1.Problem21
open ProblemSet1.Problem22
open ProblemSet1.Problem23
open ProblemSet1.Problem24

[<EntryPoint>]
let main _ =
  printfn "-- Problem Set 1 --\n"

  printfn "-- Problem 17 --"

  let rev = revlists [[0;1;1];[3;2];[];[5]]

  printfn "%A" rev

  printfn "-- Problem 18 --"

  let list1 = [1;2;3]
  let list2 = [4;5;6]

  let list_final = interleave list1 list2

  printfn "First List: %A\nSecond List: %A\nFinal List: %A" list1 list2 list_final

  printfn "--Problem 19 --"

  let initial = [1;2;3;4;5;6]

  let final = cut initial

  printfn "Initial list: %A" initial
  printfn "Final list: %A" final

  printfn "-- Problem 20 --"

  let initial = [1;2;3;4;5;6;7;8]

  let shuffled = shuffle initial

  printfn "Initial list: %A" initial
  printfn "Final list: %A" shuffled

  printfn "-- Problem 21 --"

  printfn "Number of shuffles on a deck of 4 cards: %d" (countshuffles 4)
  printfn "Number of shuffles on a deck of 52 cards: %d" (countshuffles 52)

  printfn "-- Problem 22 --"

  printfn "%A" <| cartesian ["a"; "b"; "c"] [1; 2]

  printfn "-- Problem 23 --"

  printfn "%A" <| powerset [1 .. 3]

  0
