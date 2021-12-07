bitArrayToDec :: [Int] -> Int
bitArrayToDec = foldl (\acc bit -> acc * 2 + bit) 0

mostCommonBit :: [[Int]] -> [Int]
mostCommonBit xs = map mapFn $ foldl foldlFn acc xs
  where 
    mapFn x = if 2*x >= length xs then 0 else 1
    foldlFn x acc = zipWith (+) acc x
    acc = replicate 12 0

sol :: [[Int]] -> (Int -> Int) -> Int -> [Int]
sol [xs] y i = xs
sol xs f i = sol (fil mcb xs) f (i+1) 
  where
    mcb = f $ mostCommonBit xs !! i
    fil x = filter (\y -> y !! i == x)

main :: IO ()
main = do
  f <- readFile "./input.txt"
  let xs = map (map $ read . (: [])) $ lines f :: [[Int]]
  print $ bitArrayToDec(sol xs (1-) 0) * bitArrayToDec(sol xs id 0)
