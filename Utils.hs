module Utils (
) where

life :: Board -> IO ()
life b = do
    cls
    showcells b
    threadDelay 1000000
    life (nextGen b)

cls :: IO ()
cls = putStr "\ESC[2J"

writeAt :: Pos -> String -> IO ()
writeAt p xs = do
    goto p
    putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells b = sequence_ [ writeAt p "0" | p <- b ]

block :: Board
block = [(2,2), (2,3), (3,2), (3,3)]

blinker :: Board
blinker = [(3,2), (3, 3), (3, 4)]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]
