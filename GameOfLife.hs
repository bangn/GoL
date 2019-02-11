module GameOfLife (
) where

import Control.Concurrent (threadDelay)
import Data.List (nub)

type Pos = (Int, Int)
type Board = [Pos] -- list of live pos

width :: Int
width = 10

height :: Int
height = 10

-- next generation = survivours + births
--  survivours =
--    pos where
--      is alive
--      live neighbours = 2 or 3
--
isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

neighbours :: Pos -> [Pos]
neighbours (x, y) = map wrap [(x-1, y)
                             ,(x-1, y+1)
                             ,(x, y+1)
                             ,(x+1, y+1)
                             ,(x+1, y)
                             ,(x+1, y-1)
                             ,(x, y-1)
                             ,(x-1, y-1)
                             ]

-- pos which is out of board need to be wrapped into board
wrap :: Pos -> Pos
wrap (x, y) = ((x-1) `mod` width + 1, (y-1) `mod` height + 1)

liveNeighbours :: Board -> Pos -> Int
liveNeighbours b p = length (filter (isAlive b) (neighbours p))

survivours :: Board -> [Pos]
survivours b = [p | p <- b, liveNeighbours b p `elem` [2,3]]

--  births =
--    pos where
--      is death
--      live neighbours = 3
--
--  only live pos can give birth to other postions
--    get all neighbours of live pos
isDeath :: Board -> Pos -> Bool
isDeath b p = not (isAlive b p)

neighboursOfLivePos :: Board -> [Pos]
neighboursOfLivePos b = nub (concatMap neighbours b)

births :: Board -> [Pos]
births b = [p | p <- neighboursOfLivePos b, isDeath b p, liveNeighbours b p == 3]

nextGen :: Board -> Board
nextGen b = survivours b ++ births b
