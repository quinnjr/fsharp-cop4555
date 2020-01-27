// Write a program that prints the next 30 leap years.

// A leap year is exactly divisible by for except for centry years.
// For centry years, a leap year must be divisible by 400.
let is_leap_year y = function
| (y % 4) = 0 | ((y % 100) = 0 & (y % 400)) = 0 -> true
| _ -> false

[<EntryPoint>]
let main args =
  let start_year = 2020
  let end_year = start_year + 30
  let leap_years = [
    for y in start_year .. end_year -> if is_leap_year y then y else null
  ]

  printfn "Leap years from %d to %d" start_year end_year

  for y in leap_years do
    printf "%d " y

  0
