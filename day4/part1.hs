import Data.List.Split ( splitOn )
import Data.List ( transpose )

remove :: Eq a => a -> [[a]] -> [[a]]
remove element = map (filter (/= element))

sol :: [[[Int]]] -> [Int] -> Int -> ([[[Int]]], Int)
sol _ [] _ = ([], 0)
sol b (n:ns) l =
  if not (null y) -- if y have any elements
    then (y, n)
    else sol (filter (\y -> length y == 5) x) ns l -- removing winning boards from input
  where 
    x = map (filter (not . null) . remove n) b -- removing empty lines
    y = filter (\y -> length y < 5) x -- adding winning boards to the result

main :: IO ()
main = do
  f <- readFile "./input.txt"
  -- XD this line is wierd ik
  let boards = map (map (map read . words) . splitOn "\n") $ tail $ splitOn "\n\n" f :: [[[Int]]]
  let numbers = map read $ splitOn "," $ head $ lines f :: [Int]
  let shapedBoards = boards ++ map transpose boards
  let result = sol shapedBoards numbers (length shapedBoards)
  
  print $ snd result * sum (map sum $ last $ fst result)
