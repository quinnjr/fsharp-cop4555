// Write a program that prints the next 30 leap years.

// A leap year is exactly divisible by for except for centry years.
// For centry years, a leap year must be divisible by 400.
let is_leap_year year =
  match year with
  | y when (y % 4) = 0 || ((y % 100) = 0 && (y % 400) = 0) -> true
  | _ -> false

[<EntryPoint>]
let main args =
  let start_year = 2020
  let end_year = start_year + 30

  let rec leap_years year li =
    if year = (end_year + 1) then li
    else
      let guard = is_leap_year year
      match year with
      | y when guard ->
        let li = li @ [y]
        leap_years (year + 1) li
      | y -> leap_years (year + 1) li
      | _ -> li

  printfn "Leap years from %d to %d" start_year end_year

  let lst = leap_years 2020 []

  for y in lst do
    printf "%d " y

  0
