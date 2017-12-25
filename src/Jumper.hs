module Jumper () where


replaceAt :: [a] -> Int -> (a -> a) -> [a]
replaceAt coll pos f =
  case nth coll pos of
    Just v ->
      take pos coll ++ [f v] ++ drop (pos + 1) coll
    Nothing ->
      coll


nth :: [a] -> Int -> Maybe a
nth coll pos =
  case take 1 $ drop pos $ coll of
    [v] ->
      Just v
    [] ->
      Nothing


solve2 :: Int -> Int
solve2 value =
  value + (if value >= 3 then -1 else 1)


doSolve :: [Int] -> Int -> Int -> Int
doSolve instructions position acc = 
  if position >= (length instructions) then
    acc
  else
    case nth instructions position of
      Just v ->
        doSolve (replaceAt instructions position solve2) (position + v) (acc + 1)
      Nothing ->
        -1


solve :: [String] -> Int
solve strings =
  doSolve (map read strings) 0 0 


main :: IO ()
main = do
  fh <- readFile "data/input5"
  ls <- return $ lines fh
  solution <- return $ solve ls
  return ()