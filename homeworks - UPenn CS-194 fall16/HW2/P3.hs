{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
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
player2 (S c R) = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)] 
          & path [(0,0),(0.3,-0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player2 (S c L) = scaled (-1) 1 (player2 (S c R)) -- Cunning!
player2 (S c U) = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)] 
          & path [(0,0),(-0.3,0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player2 (S c D) = translated 0 0.3 cranium
          & path [(0,0),(0.3,-0.05)] 
          & path [(0,0),(-0.3,-0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)
  


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
nextMove nd (S c d)
  | legalMove (adjacentCoord nd (S c d)) = (adjacentCoord nd (S c d))
  | otherwise                            = S c nd

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

resetableInteractionOf :: world -> (Double -> world -> world) -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableInteractionOf initial_w ht he ds = interactionOf initial_w ht  (\key w -> if key == (KeyPress "Esc") then (he key initial_w) else (he key w)) ds


main :: IO ()
main = resetableInteractionOf initialCoord handleTime handleEvent drawState
