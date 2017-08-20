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


player2 :: State -> Picture 
player2 (S _ R)= colored white (solidRectangle 1 1)
player2 (S _ U)= colored green (solidRectangle 1 1)
player2 (S _ L)= colored blue (solidRectangle 1 1)
player2 (S _ D)= colored red (solidRectangle 1 1)
  


legalMove :: State -> Bool
legalMove (S (C x y) _)
  | maze x y == 2 || maze x y == 3 = True
  | otherwise                      = False
  

initialCoord :: State
initialCoord = S (C 0 1) R

atCoord :: State -> Picture -> Picture
atCoord (S (C x y) _) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> State -> State
adjacentCoord R (S (C x y) _) = S (C (x+1) y) R
adjacentCoord U (S (C x y) _) = S (C  x   (y+1)) U
adjacentCoord L (S (C x y) _) = S (C (x-1) y) L
adjacentCoord D (S (C x y) _) = S (C  x   (y-1)) D

nextMove :: Direction -> State -> State
nextMove nd (S (C x y) d)
  | legalMove (adjacentCoord nd (S (C x y) d)) = (adjacentCoord nd (S (C x y) d))
  | otherwise                                  = S (C x y) nd

handleTime :: Double -> State -> State
handleTime _ s = s

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Right" = nextMove R s
    | key == "Up"    = nextMove U s
    | key == "Left"  = nextMove L s
    | key == "Down"  = nextMove D s
    | otherwise      = s
handleEvent _ s      = s


drawState :: State -> Picture
drawState s = (atCoord s (player2 s)) & pictureOfMaze

main :: IO ()
main = interactionOf initialCoord handleTime handleEvent drawState
