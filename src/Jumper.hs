module Jumper (main) where

import Data.List
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V


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


data JumpState = JumpState { instructions :: V.Vector Int
                           , position :: Int
                           , steps :: Int
                           , goal :: Int }
  deriving (Show)


nextState :: JumpState -> JumpState
nextState s =
  let
    pos = position s
    (ni, np) = 
      runST $ do
        is <- V.thaw $ instructions s
        v <- M.read is pos
        M.write is (solve2 v) pos
        nv <- V.freeze is
        return (nv, pos + v)

    na = (steps s) + 1
  in 
    JumpState { instructions = ni
              , position = np
              , steps = na
              , goal = goal s
              }
  

nextM :: State JumpState Int
nextM = do
  s <- get
  if (position s) < (goal s) then
    do 
      put $ nextState s
      nextM
  else
    return $ steps s
    

initState :: [Int] -> JumpState
initState instructions =
  JumpState { instructions = V.fromList instructions
            , position = 0
            , steps = 0
            , goal = length instructions
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