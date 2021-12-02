pair :: [Int] -> [(Int, Int, Int)]
pair x = zip3 x (tail x) (drop 2 x)

solution :: Int -> [Int] -> Int
solution y [] = y
solution y (x:xs) =
  if x < head xs
    then solution (y+1) xs
    else solution y xs

main :: IO()
main = do
  f <- readFile "day1.txt"
  let xs = pair $ map read $ lines f
  print $ solution 0 (map (\(x,y,z) -> x+y+z) xs) 