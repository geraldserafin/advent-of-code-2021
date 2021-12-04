import Data.Vector.Unboxed (zipWith5)
mostCommonBit :: [[Int]] -> [Int]
mostCommonBit xs = map mapFn (foldl foldlFn acc xs)
  where 
    mapFn x = round $ fromIntegral x / fromIntegral (length xs)
    foldlFn x acc = zipWith (+) acc x
    acc = replicate 12 0

bitArrayToDec :: [Int] -> Int 
bitArrayToDec xs = snd $ foldr foldrFn (0,0) xs
  where
    foldrFn x (acc, s) = (acc+1, if x == 0 then s else s+2^acc)

main :: IO()
main = do
  f <- readFile "./input-part1.txt"
  let xs = map (map $ read . (: [])) $ lines f :: [[Int]]
  print $ bitArrayToDec (map (1-) $ mostCommonBit xs) * bitArrayToDec (mostCommonBit xs)
