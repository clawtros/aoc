module Jumper (main) where

import Data.List
import Control.Monad.State.Lazy


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


data JumpState = JumpState { instructions :: [Int]
                           , position :: Int
                           , steps :: Int }
  deriving (Show)

nextState :: JumpState -> JumpState
nextState s =
  case nth (instructions s) (position s) of
    Just v ->
      let
        ni = replaceAt (instructions s) (position s) solve2
        np = (position s) + v
        na = (steps s) + 1
      in 
        JumpState { instructions = ni
                  , position = np
                  , steps = na
                  }
    Nothing ->
      initState (instructions s)
  

nextM :: State JumpState Int
nextM = do
  s <- get
  if (position s) < (length $ instructions s) then
    do 
      put $ nextState s
      nextM
  else
    do return $ steps s
    

initState :: [Int] -> JumpState
initState instructions =
  JumpState { instructions = instructions
            , position = 0
            , steps = 0
            }


solve :: [Int] -> Int
solve instructions =
  evalState nextM (initState instructions)


main :: IO ()
main = do
  fh <- readFile "data/input5"
  ls <- return $ lines fh
  solution <- return $ solve $ map read ls
  putStrLn $ show solution
  return ()