(*
  Write an F# function countshuffles n that counts how many calls
  to shuffle on a deck of n distinct "cards" it takes to put the deck back
  into its original order.
*)

module Shuffle =
  let interleave xs ys =
    let rec loop a b acc =
      match a, b with
      | e, [] | [], e -> List.rev acc @ e
      | ahead::atail, bhead::btail -> loop atail btail (bhead :: ahead :: acc)

    loop xs ys []

  let rec gencut (i: int) (xs: int list) =
    let rec accumulator = function
    | 0, a, b -> (List.rev a, b)
    | n, a, [] -> a, []
    | n, a, bhead::btail -> accumulator ((n-1), (bhead::a), btail)

    accumulator (i, [], xs)

  let cut (input: int list) = gencut ((List.length input / 2)) input

  let shuffle xs =
    let ha, hb = cut xs
    interleave ha hb

open Shuffle

let countaux (deck : list 'a) (target : list 'a) : int =
  let count_internal (d : list 'a) (acc : int) : int =
    if target <> d then count_internal((shuffle d) acc+1) else acc

  count_internal deck 0

let countshuffles n =
  let target = [ 0 .. n ]
  let deck = shuffle target
  countaux deck target

[<EntryPoint>]
let main args =
  printfn "Number of shuffles on a deck of 4 cards: %d" (countshuffles 4)

  0
