
# Open questions

## Exercise 4
Happy is more efficient at handling left-recursive grammars (although the documentation suggests right-recursive grammars are also possible), which is different from parser combinators which need right-recursive grammars to parse. This seems to imply that happy constructs their parse trees from right to left instead of left to right.

## Exercise 10
If the recursive call is in the middle of the command stack it's size will keep growing. As opposed to putting a recursive call at the end of the stack. This is because if a recursive call is evaluated in the middle of the stack it won't evaluate the rest till everything in the recursive call is evaluted. This means that the stack size will keep growing. It is possible that this problem is somewhat mitigated due to Haskell lazyness (though in the interactive function this won't matter as we print the stack every iteration).