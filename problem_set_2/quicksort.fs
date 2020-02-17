module Project2

let quicksort = function
| [] -> []
| x::xs ->
    let sm = List.filter(fun e -> e < x)
    let lg = List.filter(fun e -> e >= x)
    sm::x::lg

