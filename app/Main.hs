module Main where

import Life
import Control.Monad (unless)

main :: IO ()
main = do
    loop glider

-- Game "loop" of Life
loop :: [Bool] -> IO ()
loop xs = do
    print "---------------------------------"
    -- If all cells have died, stop iterating
    unless (checkComplete xs) $ do
        ppBoard xs
        let updatedBoard = update xs
        -- If the previous board is equal to the updated board, no progress left
        if not (updatedBoard == xs)
            then loop $ update xs
            else loop $ take 25 $ repeat False