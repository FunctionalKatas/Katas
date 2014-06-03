type Point = (Int, Int)

data Shape = Line Point Point
           | Compound Shape Shape
           deriving (Show)

s1 ||| s2 = Compound s1 s2

data Transform = Scale Point
               | Translate Point
               | Compose Transform Transform
               deriving (Show)

t1 <+> t2 = Compose t1 t2

transform :: Transform -> Shape -> Shape
transform (Translate (x, y)) (Line (x1, y1) (x2, y2)) =
    Line (x1 + x, y1 + y) (x2 + x, y2 + y)
transform (Scale (x, y)) (Line (x1, y1) (x2, y2)) =
    Line (x1 * x, y1 * y) (x2 * x, y2 * y)
transform (Compose t1 t2) s =
    transform t2 $ transform t1 s
transform t (Compound s1 s2) =
    Compound (transform t s1) (transform t s2)

invert :: Transform -> Transform
invert (Translate (x, y)) = Translate (-x, -y)
invert (Scale (x, y)) = Scale (-x, -y)
invert (Compose t1 t2) = Compose (invert t1) (invert t2)

inside :: Point -> Shape -> Bool
p@(x, y) `inside` Line (x1, y1) (x2, y2) =
    isInside where
        isInside = colinear && bounded
        colinear = crossProduct == 0
        crossProduct = cp1 - cp2
        cp1 = (y - y1) * (x2 - x1)
        cp2 = (x - x1) * (y2 - y1)
        within a b c = (a <= b && b <= c) || (c <= b && b <= a)
        bounded = if x1 /= x2 then (within x1 x x2) else (within y1 y y2)

p `inside` Compound s1 s2 = p `inside` s1 || p `inside` s2

unitY :: Shape
unitY = Line (0, 0) (0, 1) ||| Line (0 ,1) (-1, 2) ||| Line (0, 1) (1, 2)

space :: Int -> Int -> [[Point]]
space w h = [ [(x, y) | x <- [0..w]] | y <- [0..h] ]

draw :: Shape -> [String]
draw s = map (map (draw' s)) (space 100 63)

draw' :: Shape -> Point -> Char
draw' s p = if p `inside` s then '*' else '_'

tree n = transform baseTranslation $ tree' n

baseTranslation = Translate (50, 0)

tree' 0 = transform (Scale (16, 16)) unitY
tree' n = newYs ||| tree' (n - 1)
          where newYs = foldl1 (|||) (zipWith (transform) (translationFactors n) newYs')
                newYs' = take (2 ^ n) $ repeat newY
                newY = transform (Scale scaleFactor <+> Translate (0, positionY)) unitY
                positionY = foldl1 (+) (take n $ heights)
                scaleFactor = (factor, factor)
                factor = 2 ^ (4 - n)


translationFactors n = (map invert (translationFactors' n)) ++ translationFactors' n
translationFactors' n = take (2 ^ (n - 1)) $ [ Translate (x, 0) | x <- [f, 3*f..] ]
    where f = 2 ^ (5 - n)

heights = reverse $ map (2^) [0..5]

test n = mapM_ putStrLn (reverse $ draw $ tree n)
