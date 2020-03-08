Joseph R. Quinn
\#6103509
COP4555-U02
Github U/N: quinnjr
Github Repo: https://github.com/2001-cop4555-u02/effort-1-quinnjr

# Essay Questions

7. For the function `oddeven`, the function call by itself is of O(1) difficulty. Breaking the function down using a recursive method makes the actual implementation of the function of O(ln n) complexity. Looking further at the implementation of the matched values, the then block appends the value of the if-then statement to the recursive call of while the else block prepends the recursive call to the current value. Appending in F# takes O(n) (since it must use a loop to correctly append the data to the List in memory) time at least, while the prepend only takes O(ln n) from the recursive function.

  Therefore, the asymptotic time complexity of `oddeven` is O(n).

14.
  a. In memory, the value of variable i is equal to 5. In memory, the value of j is equal to 8. i is then modified to be the value of 2 multiplied by the value of j in memory, plus the value of i prior to the reassignment in memory (5). The result is that i in memory is now equal to 21. The value of j in memory stays at 8.
  b. In memory, the value of i is 3. In memory, the value of j is 8. If the value of 2 multiplied by the value of i is greater than j then i in memory is assigned the value of 2 multiplied by j. If the value of 2 multiplied by the in-memory value of i is less than the value f j, then j in memory is set to 2 multiplied by i. The result of running this if-then-else expression is that i in memory remains at the value of 3 while the value of j in memory is altered to be 6 (2 * 3).

# Github Summary

0. Prior to beginning work on the current problem set, commits starting at 49b15ed on February 17th through commit 383d8eb addressed problems that were incomplete from the previous assignment. Problems 21, 22, 23, and 24 are mostly solved during this timeframe (with additional work being done on number 24 at a later date for modular use in Problem set 2).

1. Problem 1 is solved between commits 82ff9a1 and e50d7c2. Problem \#1 asked for a curry and uncurry function to be made for uncurrying and currying F# functions regardless of their initial implementation and in a polymorphic way.

  My implementation of curry has the function signiture of `val curry : f:('a * 'b -> 'c) -> a:'a -> b:'b -> 'c` while my implementation of uncurry has the function signiture of `val uncurry : f:('a -> 'b -> 'c) -> a:'a * b:'b -> 'c`

2.
