import Data.List.Split ( splitOn )
import Data.List ( sort, group )

frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

nextGeneration :: [(Int, Int)] -> [(Int, Int)]
nextGeneration fishes = 
  if not (null births) 
    then old ++ [(sum $ map fst births, 6)] ++ [(sum $ map fst births, 8)]
    else old
  where
    births = filter (\(_, y) -> y == (-1)) fishes
    old = filter (\(x, y) -> y /=(-1)) fishes

solution :: [(Int, Int)] -> Int -> Int
solution fishes 0  = sum $ map fst fishes
solution fishes day = solution next (day-1)
  where
    next = nextGeneration $ map (\(x,y) -> (x, y-1)) fishes

main :: IO ()
main = do
  f <- readFile "./input.txt" 
  let initial = frequency $ map read $ splitOn "," f :: [(Int, Int)]
  print $ solution initial 256