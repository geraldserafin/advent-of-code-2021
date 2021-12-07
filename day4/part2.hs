import Data.List.Split ( splitOn )
import Data.List ( transpose )

remove :: Eq a => a -> [[a]] -> [[a]]
remove element = map (filter (/= element))

sol :: [[[Int]]] -> [Int] -> [[[Int]]] -> Int -> ([[[Int]]], Int)
sol _ [] r _ = (r, 0)
sol b (n:ns) r l =
  if length y == l -- result lenght == initial length
    then (y, n)
    else sol (filter (\y -> length y == 10) x) ns y l -- removing winning boards from input
  where 
    x = map (filter (not . null) . remove n) b -- removing empty lines
    y = r ++ filter (\y -> length y < 10) x -- adding winning boards to the result

main :: IO ()
main = do
  f <- readFile "./input.txt"
  -- XD this line is wierd ik
  let boards = map (map (map read . words) . splitOn "\n") $ tail $ splitOn "\n\n" f :: [[[Int]]]
  let numbers = map read $ splitOn "," $ head $ lines f :: [Int]
  let shapedBoards = zipWith (++) boards $ map transpose boards
  let result = sol shapedBoards numbers [] (length shapedBoards)
  
  print $ snd result * (sum (map sum $ last $ fst result) `div` 2)
