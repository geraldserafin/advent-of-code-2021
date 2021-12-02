solution :: Int -> [Int] -> Int
solution y [] = y
solution y (x:xs) =
  if x < head xs
    then solution (y+1) xs
    else solution y xs

main :: IO()
main = do
  f <- readFile "day1.txt"
  let xs = map read $ lines f
  print $ solution 0 xs