module Main (main) where

main :: IO ()
main = putStr heart

heart :: String
heart = map heartMask loveScreen
        where screen = do
                       y <- [1..30]
                       x <- [1..80]
                       return (x, y)
              loveScreen = zip screen love
              heartMask :: ((Integer, Integer), Char) -> Char
              heartMask ((x, y), c) = case x of
                                         80 -> '\n'
                                         x' -> if inHeart x' y then c else ' '

love :: String
love = "love" ++ love

-- courtesy of http://hrmshaker.blogspot.ie/2010/12/equation-for-love.html
inHeart :: Integer ->  Integer -> Bool
inHeart xi yi = ((((x/40) - 1))^2 + ( (4 /3)*(((31 - y)/15) - 3/4)  - (sqrt $ ((abs((x/40) - 1)))))^2) < 1
                where x = fromIntegral xi :: Double
                      y = fromIntegral yi :: Double
