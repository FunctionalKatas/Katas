Exercise:
    1. Implement a binary search tree ADT called BinTree
    2. Implement an insert function that adds a value to a BinTree.
       Values should be ordered inside the tree (but the tree doesn't need to be balanced)
    3. Implement a length function for BinTree that counts the number of nodes
    4. Implement a map function for BinTree that applies a function
       to the value in every node and preserves the tree structure
    5. Implement a fold function for BinTree that can reduce a BinTree to a single value
    6. Implement an asList function that converts the BinTree to a standard Haskell list type
    7. BONUS: Make your tree into a Key-Value store

----------------------------------------

First we declare our ADT, `BinTree`. It has a single type parameter `a` which
will be the type of the values contained within the nodes, and two data
constructors:

    - Node, which has three parameters: A left (BinTree a), a value of type `a`
      and a right (BinTree a).
    - Empty, a nullary data constructor.

> data BinTree a = Node (BinTree a) a (BinTree a)
>                | Empty
>                deriving Show

For part 2, we know our values have to be ordered inside of the tree. This
tells us something about the type of values we can `insert`, name that they
have to be members of the Ord typeclass. We can make this assertion in our
type signature like this:

> insert :: Ord a => a -> BinTree a -> BinTree a

And then it's just a matter of writing the function body:

If we're inserting into the Empty node, is simple: A new Node with our value
v contained in it, and Empty as both the left and right subtrees. We use
pattern matching to write this case: The second argument of the insert
function is the value `Empty` rather than a free parameter, which means 
this defintion of the insert function will only get invoked if the second
parameter is Empty we pass when evaluating it is also Empty.

> insert v Empty = Node Empty v Empty

With only one pattern match the function is considered "partial", meaning
it doesn't have a result for all possible input values. Trying to evaluate
a partial function with a value for which it is not defined results in an
error. Since we don't want errors, we had best define `insert` for the
alternative case: when the second parameter is a `Node` value.

Here, our second parameter is again using pattern matching on the second
parameter. In this case, we're using another nice feature of Haskell's
pattern matching, called "destructuring". We state the second parameter
as (Node l a r). `l`, `a` and `r` free parameters, but when we evaluate
the function `insert` with a Node as the second argument, the pattern
matching system will automatically "bind" those parameters, meaning we
can use them in our function body without any fuss using things like
"getters". It works like this:

If we call:

    insert 10 (Node Empty 5 (Node Empty 6 Empty))

The pattern matching system sees that

    Node Empty 5 (Node Empty 6 Empty)

Is of the same format as our pattern: (Node l a r)

    Node Empty 5 (Node Empty 6 Empty)
           ^   ^          ^
    Node   l   a          r

And so performs a binding step that means that inside our function body
we can use the name `l` to refer to `Empty`, `a` to refer to `5`, and `r`
to refer to `(Node Empty 6 Empty)`. Handy!

Another nice feature we're using in this function definition is "guards".
Guards are boolean conditions you can put into your function definition
along with specifying multiple function bodies. When the function is
evaluated the guards are checked and the appropriate function body is
selected. Think of them like a nice syntax for a `case` statement or 
ifs (Haskell has both of those constructs too, though!). If multiple
guards are true, then the first one read from top-to-bottom is used.

In this case our guards decide whether we insert into the left sub-tree
or the right sub-tree, depending on whether our insert value `v` is bigger
or smaller than the value in the current node, `a`. We recursively call
`insert` on the appropriate sub-tree depending on the result of the comparison.

> insert v (Node l a r) | v >= a = Node l a (insert v r)
>                       | v <  a = Node (insert v l) a r

For the third problem we have to implement `map` for our tree, which I'm going
to call `mapTree`. `map` should apply some function to the value in every node
of the tree, and result in a tree with exactly the same structure as before but
with the values updated by the results of the function application. This is
easier to do than it sounds! (pattern matching to the rescue again)

Our type signature here is a little bit more complicated than before. It says:

"I take a function that takes a value of type `a` and returns a value of type `b`,
a BinTree containing `a`s, and return a BinTree containing `b`s"

That sounds like what we're looking for!

> mapTree :: (a -> b) -> BinTree a -> BinTree b

Then, since we're going to use recursion, we define our base case. Since the
Empty node is exactly what it says it is -- empty --, there's no value to
apply our function to, so the result is still just `Empty`. We say `_` in
a pattern match when we don't care what the value there is (since we're not
going to use it) but you don't HAVE to do that.

> mapTree _ Empty = Empty

Then, for our recursive case we use a destructuring pattern match to pull apart
the BinTree parameter into the same `l`, `a` and `r` components as before. In
the body of the function we make a new Node, with its left-subtree equal to
the result of recursively applying `mapTree` with the function `f` to the left
sub-tree of the input node, the value as the result of applying `f` to the value
contained in the input node, and the right sub-tree equal to the result of
recursively applying `mapTree` with the function `f` to the right sub-tree of
the input node.

> mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

For part 5 we have to implement `fold`. Fold goes by various names in other
languages: fold, reduce, and inject are common. We want, when given a binary
operator and an initial value, to take the value in every node of the tree
and combine them using the binary operator we passed, starting with the inital
value we've provided. The result is a single value of the same type as the
initial value.

> foldTree :: (a -> b -> a) -> a -> BinTree b -> a

Our type signature says exactly that: given a binary function that takes
a value of type `a` and a value of type `b` returns a value of type `a`,
a value of type `a`, and a BinTree containing values of type `b`, we return
a value of type `a`.

Once again we're going to use recursion and pattern matching: in the empty
case the result of applying foldTree is just to return the initial value, `z`.

> foldTree _ z Empty = z

In the recursive case however we see another new piece of syntax, a "where"
clause. "where" lets you split up your expression by binding sub-expressions
to names, and then using those names to refer to them elsewhere. If we rewrote
this expression **without** using "where", it would have looked like this:

    foldTree f (foldTree f z l `f` a) r

Which is a little bit harder to read. Another new thing appearing here is backticks
around a function. When you put backticks around a function that takes two arguments
Haskell treats it like an infix operator. In other words:

    foldTree f z l `f` a      is equal to      f (foldTree f z l) a

With that in mind, we declare that we recursively apply foldTree to the left sub-tree,
then combine that value with the value in the current node using the function `f`, to
get the expression I've called "leftFold", and then we use this value as the initial
value when calling foldTree recursively on the right sub-tree. This means we're using
the initial value parameter `z` as an accumulator that we increase as we traverse the
different parts of the tree.

> foldTree f z (Node l a r) =  foldTree f leftfold r
>     where leftfold = foldTree f z l `f` a

For the final part of the exercise we have to conver the tree to a list. There are a
lot of ways of doing this, but a nice example of a functional approach to problem
solving is to reuse the functions we've already written.

We know we can use mapTree to apply a function to every element in the tree, maybe
changing its value or type.

We also know we can use foldTree to apply a function to every node in the tree,
combining them into a single value.

We can combine these functions to turn a tree into a list easily. First, we map
the `return` function on every element of the tree. `return` is a polymorphic
function from the Monad type class that is polymorphic on all of its arguments.
That sounds scary, but try not to let it hurt your brain too much. All we need
to know is that lists are a Monad, and that by applying the `return` function
we are turning every element in the tree into a single element list.

So the result of applying:

    mapTree return (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) :: BinTree [Int]

Is the tree:

    Node (Node Empty [3] Empty) [4] (Node Empty [5] Empty)

The reason we have to specify that type annotation is that on its own, the type inference
engine can't tell what type we can't `return` to resolve to. By annotating it as
"BinTree [Int]" the compiler now knows we want the results to be lists.

Once we have applied that step and have a tree full of lists, we can fold the (++) operator
over the tree, with an initial value of the empty list: []. (++) is list concatenation! The
result of that is to turn the tree:

    Node (Node Empty [3] Empty) [4] (Node Empty [5] Empty)

Into the expression:

    [3] ++ [4] ++ [5]

Which then evaluates to:

    [3,4,5]

Exactly what we wanted! Here's how that looks in practice:

> asList :: BinTree a -> [a]
> asList t = foldTree (++) [] (mapTree return t)
