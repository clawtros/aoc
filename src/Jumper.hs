module Jumper (main) where

import Control.Monad.Loops
import Data.IORef
import Data.Array.MArray
import Data.Array.IO

main :: IO ()
main = do
    program <- readFile "data/input5" >>= return . map read . lines
    let size = length program
    memory <- newListArray (0, size) program :: IO (IOUArray Int Int)
    address <- newIORef 0
    stepCount <- newIORef 0

    iterateWhile id $ do
        a <- readIORef address
        if a < 0 || a >= size then
            return False
        else do
            o <- readArray memory a
            writeIORef address $ a + o
            writeArray memory a $ if o >= 3 then o - 1 else o + 1
            modifyIORef stepCount (+ 1)
            return True

    readIORef stepCount >>= putStrLn . show