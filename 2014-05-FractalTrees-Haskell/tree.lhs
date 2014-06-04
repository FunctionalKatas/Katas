> import Data.Monoid

We'll start by defining a point as a tuple of two integers representing its 
coordinates on a cartesian plain.

>
> type Point = (Int, Int)

Then, using that point type, we can define an algebraic data type `Shape`.
Since for our purposes we're only going to need a "line" type this is quite
simple, since a line can be fully represented with just two Points.

> data Shape = Line Point Point

Then we'll fill out the Shape type with two extra data constructors,
Compound and Nil. Compound is a structurally recursive data constructor
that lets you glue two shapes together into a single compound shape.

Nil on the other hand is a vacuous shape that doesn't exist. You might
wonder why we need it, then! It's really an implementation detail. Since
we have compound, which lets us join two shapes together, the Shape type
forms a semigroup. Nil forms the identity element of the binary operation
Compound, lifting Shape from being a semigroup to being a...

>            | Compound Shape Shape
>            | Nil
>            deriving (Show)

Monoid!

> instance Monoid Shape where
>     mempty = Nil
>     mappend = Compound

In a similar fashion we'll define a Transform data type to represent affine
transformations that can be applied to shapes. The only thing particularly
nuanced here is the fact that the Scale constructor takes a Point as a parameter
rather than a scalar value. The benefit of this is to allow for non-linear scaling.

> data Transform = Scale Point
>                | Translate Point
>                | Compose Transform Transform
>                | Identity
>                deriving (Show)

> instance Monoid Transform where
>     mempty = Identity
>     mappend = Compose

With the Shape and Transform data types in hand we can go ahead and define
a transform function which, when given a transformation and a shape yields
a new shape.

> transform :: Transform -> Shape -> Shape
> transform (Translate (x, y)) (Line (x1, y1) (x2, y2)) =
>     Line (x1 + x, y1 + y) (x2 + x, y2 + y)
> transform (Scale (x, y)) (Line (x1, y1) (x2, y2)) =
>     Line (x1 * x, y1 * y) (x2 * x, y2 * y)
> transform (Compose t1 t2) s =
>     transform t2 $ transform t1 s
> transform t (Compound s1 s2) =
>     Compound (transform t s1) (transform t s2)

And we'll take a moment to create a utility function `invert` which takes
a translation and yields its dual. This will come in handy later, since
the graphs we're trying to draw axially symmetrical so we can just generate
one set of transformations and then mirror them to create the other half.

> invert :: Transform -> Transform
> invert (Translate (x, y)) = Translate (-x, -y)
> invert (Scale (x, y)) = Scale (-x, -y)
> invert (Compose t1 t2) = Compose (invert t1) (invert t2)

When it comes time to render the graph we've been drawing we'll need some
way of deciding whether a "pixel" is part of the graph or not, which is
where the `Inside` function comes in. We'll describe a point as being inside
a line if the point is:

    a) Within a "bounding box" drawn around the two points of the line.
    b) Colinear with the two points of the line.

Checking for colinearity is much easier than it sounds, since the cross
product of two colinear vectors is always 0.

> inside :: Point -> Shape -> Bool
> (x, y) `inside` Line (x1, y1) (x2, y2) =
>     isInside where
>         isInside = bounded && colinear
>         colinear = crossProduct == 0
>         crossProduct = cp1 - cp2
>         cp1 = (y - y1) * (x2 - x1)
>         cp2 = (x - x1) * (y2 - y1)
>         bounded = if x1 /= x2 then (within x1 x x2) else (within y1 y y2)
>         within a b c = (a <= b && b <= c) || (c <= b && b <= a)

> p `inside` Compound s1 s2 = p `inside` s1 || p `inside` s2
> p `inside` Nil = False

At that point we've got all of the tools we need to finish the Kata. We
start by defining a "unit Y", one of the Y shaped segments of the grah
with all the edges being of length 1. This is nice and easy to work with,
since we can scale it up and position it as we need to.

> unitY :: Shape
> unitY = Line (0, 0) (0, 1) <> Line (0 ,1) (-1, 2) <> Line (0, 1) (1, 2)

Then we define a function to create our coordinate space, and functions
to render our space and any shapes inside of it as strings.

The `space` function takes a width and a height and returns a 2-dimensional
list of points representing all points on the plane represented by the space,
with its origin at the top left corner.

> space :: Int -> Int -> [[Point]]
> space w h = [ [(x, y) | x <- [0..w]] | y <- [0..h] ]

The `draw` function serves as the driving function for rendering. When given
a Shape it yields a list of strings that can be printed to give the fully
rendered output. To accomplish that we create a 100 x 63 space as defined in
the Kata and then map the draw' helper over every element (necessitating a
doubled call to map to cover both dimensions). The draw' helper simply
checks if a point is inside the Shape and converts the point in the space
to the appropriate character based on the result.

> draw :: Shape -> [String]
> draw s = map (map $ draw' s) (space 100 63)
>   where draw' s p = if p `inside` s then '*' else '_'

> tree n = transform baseTranslation $ tree' n


> tree' 0 = transform (Scale (16, 16)) unitY
> tree' n = newYs <> tree' (n - 1)
>           where newYs = foldl1 (<>) $ zipWith transform (translationFactors n) newYs'
>                 newYs' = take (2 ^ n) $ repeat newY
>                 newY = transform (Scale scaleFactor <> Translate (0, positionY)) unitY
>                 positionY = foldl1 (+) (take n $ heights)
>                 scaleFactor = (factor, factor)
>                 factor = 2 ^ (4 - n)

> translationFactors n = (map invert (translationFactors' n)) ++ translationFactors' n
> translationFactors' n = take (2 ^ (n - 1)) $ [ Translate (x, 0) | x <- [f, 3*f..] ]
>     where f = 2 ^ (5 - n)

> heights = reverse $ map (2^) [0..5]
> baseTranslation = Translate (50, 0)
> 
> test n = mapM_ putStrLn (reverse $ draw $ tree n)
