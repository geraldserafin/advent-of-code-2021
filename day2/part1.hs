translate :: (String, Int) -> [Int]
translate ("forward", b) = [b, 0]
translate ("up", b) = [0, -b]
translate ("down", b) = [0, b]

main :: IO()
main = do
  f <- readFile "input.txt"
  let xs = map ((\ [x, y] -> (x, read y)) . words) (lines f) :: [(String, Int)]
  print $ foldl (zipWith (+)) [0, 0] (map translate xs)
  