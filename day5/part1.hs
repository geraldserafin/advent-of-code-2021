{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List.Split ( splitOn )
import Data.List ( sort, group )

frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

getLines :: Eq a => [[[a]]] -> [[[a]]]
getLines lines = filter horizontal lines ++ filter vertical lines
  where 
    horizontal [[x1,_],[x2,_]] = x1 == x2
    vertical [[_,y1],[_,y2]] = y1 == y2

calculateStep :: [[[Int]]] -> [[Int]]
calculateStep = map step
  where 
    step [[x1,y1],[x2,y2]] = [signum(x2 - x1), signum(y2 - y1)]

generatePoints :: [[[Int]]] -> [[Int]] -> [[[Int]]]
generatePoints = zipWith points 
  where
    points line step = takeWhile (/=end) $ iterate nextStep beg
      where
        beg = head line
        end = [head (last line) + head step, last (last line) + last step]
        nextStep [x, y] = [x + head step, y + last step]

main :: IO ()
main = do
  f <- readFile "./input.txt"
  let input = map (map (map read . splitOn ",") . splitOn " -> ") $ lines f :: [[[Int]]]
  let lines = getLines input
  let steps = calculateStep lines
  let allPoints = concat $ generatePoints lines steps
  print $ length $ filter (\x -> fst x > 1) $ frequency allPoints
