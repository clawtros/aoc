module Dupecheck (main) where

import Data.List (length, sortBy)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Function ((&))


addAnagram ::  M.Map T.Text Int -> T.Text -> M.Map T.Text Int 
addAnagram store word =
  M.insertWith (+) (T.pack $ sortBy compare $ T.unpack word) 1 store


addInc :: M.Map T.Text Int -> T.Text -> M.Map T.Text Int 
addInc store word =
  M.insertWith (+) word 1 store


countOccurrences words =
  foldl addAnagram M.empty words


validateLine :: [T.Text] -> Bool
validateLine line =
  countOccurrences line
  & M.elems
  & all (\v -> v == 1)


main :: IO ()
main = do
  fh <- readFile "data/input4"
  ls <- return $ lines fh
  spl <- return $ map (
    \l ->
      T.pack l
      & T.split (\c -> c == ' ') 
    ) ls
  goodlines <- return $ length $ filter id (map validateLine spl)
  (putStrLn . show) goodlines
  return ()


