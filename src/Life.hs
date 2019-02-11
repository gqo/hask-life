module Life
( glider
, ppBoard
, update
, checkComplete
) where

import Data.List.Split (chunksOf)
import Data.Tuple (swap)

-- glider is a seed board for Life
-- Referenced https://en.wikipedia.org/wiki/Glider_(Conway's_Life)
glider = [True,False,True,False,False
        ,False,True,True,False,False
        ,False,True,False,False,False
        ,False,False,False,False,False
        ,False,False,False,False,False]

-- boardSize is the width/length of the game board
boardSize = fromIntegral (floor $ sqrt $ fromIntegral $ length glider) :: Int

boardArea = fromIntegral $ length glider :: Int

-- to2d takes 1d list and chunks it into 2d list for easier printing
to2d :: [a] -> [[a]]
to2d = chunksOf boardSize

-- stringBoard "stringifies" gameBoard
stringBoard :: Show a => [[a]] -> String
stringBoard [] = ""
stringBoard (x:xs) = (show x) ++ "\n" ++ stringBoard xs

-- ppBoard prints the game board in a pretty fashion
ppBoard :: Show a => [a] -> IO ()
ppBoard = putStr . stringBoard . to2d

toIndex :: (Int, Int) -> Int
toIndex (x,y) = x + (y*boardSize)

fromIndex :: Int -> (Int, Int)
fromIndex i = swap $ divMod i boardSize

-- validSingle checks if x or y val is on game board
validSingle :: Int -> Bool
validSingle x
    | x < 0 = False
    | x > boardLimit = False
    | otherwise = True
    where
        boardLimit = boardSize - 1

-- validCoord checks if co-ord given is on game board
validCoord :: (Int, Int) -> Bool
validCoord (x,y)
    | not $ validSingle x = False
    | not $ validSingle y = False
    | otherwise = True

-- allNeighbors generates list of all possible neighbors of co-ord
allNeighbors :: (Int,Int) -> [(Int,Int)]
allNeighbors (x,y) = (x-1,y-1):(x,y-1):(x+1,y-1):(x+1,y):(x+1,y+1):(x,y+1):(x-1,y+1):(x-1,y):[]

-- validNeighborCoords generates all neighbors of co-ord that are on game board
validNeighborCoords :: (Int,Int) -> [(Int,Int)]
validNeighborCoords (x,y) = filter validCoord $ allNeighbors (x,y)

-- validNeighbors generates list indexes of all valid neighbors
validNeighbors :: Int -> [Int]
validNeighbors i = map toIndex $ validNeighborCoords $ fromIndex i

-- aliveNeighbors generates count of alive neighbors
aliveNeighbors :: Int -> [Bool] -> Int
aliveNeighbors i xs = length $ filter ((!!) xs) $ validNeighbors i

-- nextStatusAlive generates next status (alive/dead) of an alive cell
nextStatusAlive :: Int -> [Bool] -> Bool
nextStatusAlive i xs
    | numAliveNeighbors < 2 = False
    | numAliveNeighbors > 3 = False
    | otherwise = True
    where 
        numAliveNeighbors = aliveNeighbors i xs

-- nextStatusDead generates next status (alive/dead) of a dead cell
nextStatusDead :: Int -> [Bool] -> Bool
nextStatusDead i xs
    | numAliveNeighbors == 3 = True
    | otherwise = False
    where
        numAliveNeighbors = aliveNeighbors i xs

-- nextStatus generates next status (alive/dead) of a cell
nextStatus :: Int -> [Bool] -> Bool
nextStatus i xs
    | ((!!) xs i) = nextStatusAlive i xs
    | otherwise = nextStatusDead i xs

-- update' keeps track of original board while generating new board
update' :: [Bool] -> [Bool] -> [Bool]
update' [] _ = []
update' newXs origXs = nextStatus index origXs : update' (tail newXs) origXs
        where
            index = boardArea - length newXs

-- update takes a game board and generates the next board after one turn of Life
update :: [Bool] -> [Bool]
update [] = []
update xs = nextStatus index xs : update' (tail xs) xs
        where
            index = boardArea - length xs

-- checkComplete checks if all cells have died on board
checkComplete :: [Bool] -> Bool
checkComplete [] = True
checkComplete xs = not $ elem True xs