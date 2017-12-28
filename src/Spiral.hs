module Spiral (spiralV) where

import Data.List (length, unfoldr)
import Data.Function ((&))
import qualified Data.Map as Map
import Control.Monad.State.Lazy


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [(x + i, y + j) | i <- [-1..1]
                  , j <- [-1..1]]


storeValueAt :: Map.Map (Int, Int) Int -> (Int, Int) -> Map.Map (Int, Int) Int
storeValueAt storage position =
  let    
    value =
      neighbours position
      & map (\x -> Map.lookup x storage )
      & map (\x ->
               case x of
                 Just v -> v
                 Nothing -> 0)
      & sum
  in
    Map.insert position value storage


initialMap :: Map.Map (Int, Int) Int
initialMap =
  Map.fromList [ ((0, 0), 1)
               , ((1, 0), 1)
               ]


data SpiralState =
  SpiralState { _x :: Int
              , _y :: Int
              , _dx :: Int
              , _dy :: Int
              , _acc :: [(Int, Int)]
              }


initState :: SpiralState
initState =
  SpiralState { _x = 0
              , _y = 0
              , _dx = 0
              , _dy = -1
              , _acc = []
              }


valFromState :: SpiralState -> [(Int, Int)]
valFromState =
  _acc


nextM :: Int -> State SpiralState [(Int, Int)]
nextM n | n < 0 = error "x must be positive"
        | n == 0 = do
            s <- get
            return $ _acc s
        | otherwise = do
            s <- get
            put $ nextState s
            nextM $ n - 1


nextState :: SpiralState -> SpiralState
nextState state =
  let
    x = _x state
    
    y = _y state

    dx = _dx state
    
    dy = _dy state

    (newDx, newDy) =
      if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y) then
        (-dy, dx)
      else
        (dx, dy)

  in
    SpiralState { _x = x + newDx
                , _y = y + newDy
                , _dx = newDx
                , _dy = newDy
                , _acc = _acc state ++ [(_x state, _y state)]
                }
  
      
spiral :: Int -> [(Int, Int)]
spiral until =
  evalState (nextM until) initState


populateMap :: Int -> Map.Map (Int, Int) Int
populateMap until =
  foldl storeValueAt initialMap
  $ drop (length $ Map.keys initialMap)
  $ spiral until


defaultN :: Int -> Maybe Int -> Int
defaultN def mayb =
  case mayb of
    Just v -> v
    Nothing -> def


spiralV :: Int -> IO ()
spiralV limit = do
  mapM_ (putStrLn . show) $ Map.toList $ (populateMap limit)
  return ()