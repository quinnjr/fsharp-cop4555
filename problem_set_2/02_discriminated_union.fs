(*
Create a discriminated union for Coordinates that can be a Tuple, Threeple or
Fourple that represent tuples of size two, three and four. The type for the
union should be polymorphic.

Instantiate a Tuple of integers, a Threeple of floats and a Fourple of strings.

Create a function that has a parameter of a binary function and Coordinate.

Apply the function to the Coordinate like List.reduce.

Call the function with (+) for each of the Coordinates in part (b).

Call the function with (-) for the numeric Coordinates in part (b). Be sure
that your function implements the normal associativity for (-).
*)

type Coordinate<'a> =
| Tuple of 'a * 'a
| Triple of 'a * 'a * 'a
| Quadruple of 'a * 'a * 'a * 'a

let reduce_coordinate f (t: Coordinate<'a>) =
  match t with
  | Tuple (x, y) -> f(x, f(y, Unchecked.defaultof<'a>))
  | Triple (x, y, z) -> f(x, f(y, f(z, Unchecked.defaultof<'a>)))
  | Quadruple (w, x, y, z) -> f(w, f(x, f(y, f(z, Unchecked.defaultof<'a>))))

[<EntryPoint>]
let main _ =

  let tupl = Coordinate.Tuple (1, 2)
  let tripl = Coordinate.Triple (1.0, 2.0, 3.0)
  let quadrpl = Coordinate.Quadruple ("0", "1", "2", "3")

  let tupl_red1 = reduce_coordinate (fun acc elem -> acc + elem) tupl
  let tupl_red2 = reduce_coordinate (fun acc elem -> acc - elem) tupl

  let tripl_red1 = reduce_coordinate (fun acc elem -> acc + elem) tripl
  let tripl_red2 = reduce_coordinate (fun acc elem -> acc - elem) tripl

  printfn "%A" tupl
  printfn "%A" tripl
  printfn "%A" quadrpl

  printfn "%A" tupl_red1
  printfn "%A" tupl_red2

  0
