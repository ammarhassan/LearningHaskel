import CodeWorld

wall:: Picture
wall = colored black (solidRectangle 1 1)

ground :: Picture
ground = colored green (solidRectangle 1 1)

storage :: Picture
storage = (colored black (solidCircle 0.25)) & (colored yellow (solidRectangle 1 1))

box :: Picture
box = colored yellow (solidRectangle 1 1)


drawTile :: Integer -> Picture
drawTile x
  | x == 1 = wall
  | x == 2 = ground
  | x == 3 = storage
  | x == 4 = box
  | otherwise = blank

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
  
drawrow :: Integer -> Integer -> Picture
drawrow numtiles rownum
  | numtiles == 0 = drawTile (maze 0 rownum )
  | otherwise = ((translated (fromIntegral (-numtiles)) 0 (drawTile (maze (-numtiles) rownum))) & 
           (drawrow (numtiles-1) rownum) & 
           (translated (fromIntegral numtiles) 0 (drawTile (maze numtiles rownum))))

drawMaze :: Integer -> Picture
drawMaze rownum
  | rownum == 0 = drawrow 10 0
  | otherwise = ((translated 0 (fromIntegral (-rownum)) (drawrow 10 (-rownum))) & 
           (drawMaze (rownum-1)) & 
           (translated 0 (fromIntegral rownum) (drawrow 10 rownum)))
                 
pictureOfMaze :: Picture
pictureOfMaze = drawMaze 10

main :: IO ()
main = drawingOf pictureOfMaze