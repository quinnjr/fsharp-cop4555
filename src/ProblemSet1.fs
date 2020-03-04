///
///
///

namespace io.QuinnJR

module ProblemSet1

  module Problem17
    let revlists xs =
      List.map (fun l -> List.rev l) xs

  module Problem18
    let interleave xs ys =
      let rec loop a b acc =
        match a, b with
        | e, [] | [], e -> List.rev acc @ e
        | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

      loop xs ys []

  module Problem19
    let rec gencut (i: int) (xs: int list) =
      let rec accumulator = function
      | 0, a, b -> (List.rev a, b)
      | n, a, [] -> a, []
      | n, a, bhead::btail -> accumulator ((n-1), (bhead::a), btail)

      accumulator (i, [], xs)

    let cut (input: int list) = gencut ((List.length input / 2)) input

  module Problem20
    let shuffle xs =
      let ha, hb = cut xs
      interleave ha hb

  module Problem21
    let rec countaux deck target =
      match deck <> target with
      | false -> 0
      | true -> 1 + countaux (shuffle deck) target

    let countshuffles (n: int) =
      let target = [ 0 .. n ]
      let deck = shuffle target
      1 + countaux deck target

  module Problem22
    let rec cart_rec = function
    | [],_ | _, [] -> []
    | x::xs, ys -> (List.map (fun y -> (x, y)) ys) :: cart_rec (xs, ys)

    let cartesian xs ys = cart_rec (xs, ys)

  module Problem23
    let rec powerset = function
    | [] -> [[]]
    | x::xs -> List.collect (fun y -> [y; x::y]) (powerset xs)


open ProblemSet1.Problem17
open ProblemSet1.Problem18
open ProblemSet1.Problem19
open ProblemSet1.Problem20
open ProblemSet1.Problem21

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
