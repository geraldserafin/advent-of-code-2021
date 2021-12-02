-- assuming that the input is correct
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

translate :: (String, Int) -> [Int]
translate ("forward", b) = [b, 0]
translate ("up", b) = [0, -b]
translate ("down", b) = [0, b]

calcNext :: [Int] -> [Int] -> [Int]
calcNext [x,y] [_,_,c] = [x, x*c, y] 

main :: IO()
main = do
  f <- readFile "input.txt"
  let xs = map ((\ [x, y] -> (x, read y)) . words) (lines f) :: [(String, Int)]
  print $ foldl (\acc x -> zipWith (+) acc (calcNext (translate x) acc)) [0, 0, 0] xs
  