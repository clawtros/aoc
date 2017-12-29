module Cycle (main) where


import Data.List
import Data.Function ((&))
import Control.Monad.State.Lazy



getCycleLength :: State ([Int], [Int], ([Int] -> [Int]), Int) Int
getCycleLength = do
  (tortoise, hare, f, count) <- get
  if tortoise == hare then
    return count
  else
    do
      put (f tortoise, (f . f) hare, f, count + 1)
      getCycleLength


redistribute :: [Int] -> [Int]
redistribute blocks =
  let
    l = length blocks - 1
    ib = zip [0..l] blocks
    (imax, v) = foldl
      (\(idx, val) (idx', val') ->
         if val < val' then (idx', val') else (idx, val)) (-1, -1) ib
    (q, r) =
      quotRem v l
    balance = 
      (take l $ repeat q) ++ [r]
  in
    map (\(i, va) -> if i == imax then 0 else va) ib
    & zip balance
    & map (\(a,b) -> a + b)


initState :: [Int] -> ([Int], [Int], ([Int] -> [Int]), Int)
initState blocks =
  (blocks, redistribute blocks, redistribute, 0)


solve :: [Int] -> Int
solve blocks =
  evalState getCycleLength $ initState blocks

  
main :: IO ()
main = do
  blocks <- readFile "data/input6" >>= return . map read . lines
  putStrLn . show