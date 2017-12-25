module Lib
    ( solve3
    ) where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Function ((&))
import qualified Ulam


splitLine :: String -> [Int]
splitLine line =
  let
    splitStrings = T.split (\c -> c == '\t') $ T.pack line
  in
    map (\s -> read (T.unpack s) :: Int) splitStrings


solve3 :: IO ()
solve3 = do
  fh <- readFile "data/input3"
  ls <- return $ lines fh
  spl <- return $ map splitLine ls
  putStrLn $ show $ sum $ map grabDivisors spl
  return ()


divides' :: Int -> Int -> Maybe (Int, Int)
divides' n d =
  if mod n d == 0 then
    Just $ (maximum [n, d], minimum [n, d])
  else
    Nothing


anyDivide :: [Int] -> Int -> [Maybe (Int, Int)]
anyDivide _ints n =
  let
    ints = filter (\x -> x /= n) _ints
    dn = divides' n
    divides = map dn ints
  in
    divides


divideOrZero :: Maybe (Int, Int) -> Int
divideOrZero x =
  case x of
    Just (q, r) ->
      quot q r
    Nothing ->
      0
      
grabDivisors :: [Int] -> Int
grabDivisors ints =
  map (anyDivide ints) ints
  & map (\n -> map divideOrZero n)
  & map sum
  & sum


