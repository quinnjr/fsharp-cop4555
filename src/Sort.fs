namespace ProblemSet2

module Sort =
  let rec private split pivot = function
  | [] -> [], []
  | [x] when x < pivot -> [x], []
  | [x] when x >= pivot -> [], [x]
  | x::xs -> let (left, right) = split pivot xs
             if x < pivot then (x::left, right) else (left, x::right)

  let rec quicksort = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> let (left, right) = split x xs
             quicksort left @ x :: quicksort right
