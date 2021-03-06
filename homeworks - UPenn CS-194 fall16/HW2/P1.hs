{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

data Direction = R | U | L | D

data Coord = C Integer Integer

data State = S Coord Direction


player :: Coord -> Picture
player (C x y) = colored white (solidRectangle 1 1)

legalMove :: Coord -> Bool
legalMove (C x y)
  | maze x y == 2 || maze x y == 3 = True
  | otherwise                      = False
  

initialCoord :: Coord
initialCoord = C 0 1

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" && legalMove (adjacentCoord R c) = adjacentCoord R c
    | key == "Up"    && legalMove (adjacentCoord U c) = adjacentCoord U c
    | key == "Left"  && legalMove (adjacentCoord L c) = adjacentCoord L c
    | key == "Down"  && legalMove (adjacentCoord D c) = adjacentCoord D c
    | otherwise      = c
handleEvent _ c      = c


drawState :: Coord -> Picture
drawState c = (atCoord c (player c)) & pictureOfMaze

main :: IO ()
main = interactionOf initialCoord handleTime handleEvent drawState
