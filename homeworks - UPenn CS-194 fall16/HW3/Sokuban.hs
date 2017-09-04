{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 x2) (C y1 y2) = x1 == y1 && x2 == y2
  

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of 
  Box -> Ground
  t   -> t

inList :: List Coord -> Coord -> Bool
inList Empty _ = False
inList (Entry f li) c = eqCoord f c || inList li c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes li c = case inList li c of
  True  -> Box
  False -> noBoxMaze c

-- The state

data State = State Coord Direction (List Coord)


initialBoxes :: Tile -> List Coord
initialBoxes tile = go (-10) (-10) Empty
  where 
    go :: Integer -> Integer -> List Coord -> List Coord
    go i1 i2 list
      | i2 == 11 && i1 == 11  = list
      | i1 == 11              = go (-10) (i2 + 1) list
      | otherwise  = case maze (C i1 i2) of 
        Box -> go (i1 + 1) i2 (Entry (C i1 i2) list)
        _    -> go (i1 + 1) i2 list
    

initialState :: State
initialState = State (C 0 1) R (initialBoxes Box)

-- Event handling
moveBox :: Coord -> Coord -> List Coord -> List Coord
moveBox _ _ Empty            = Empty
moveBox from to (Entry c cs) = case eqCoord from c of
    True  -> (Entry to (moveBox from to cs))
    False -> (Entry c (moveBox from to cs))

isOk :: Tile -> Bool
isOk Ground  = True
isOk Storage = True
isOk _       = False

canMovePlayer :: Direction -> Coord -> List Coord -> Bool
canMovePlayer d c list = isOk (mazeWithBoxes list (adjacentCoord d c))

canMoveBox :: Direction -> Coord -> List Coord -> Bool
canMoveBox d c list = isOk (mazeWithBoxes list (adjacentCoord d (adjacentCoord d c)))


legalMove :: Direction -> Coord -> List Coord -> Bool
legalMove d c li = case inList li (adjacentCoord d c) of
  True  -> canMoveBox d c li && canMovePlayer d c (moveBox (adjacentCoord d c) (adjacentCoord d (adjacentCoord d c)) li)
  False -> canMovePlayer d c li

makeMove :: Direction -> State -> State
makeMove nextd (State c currentd list)
  | legalMove nextd c list = State (adjacentCoord nextd c) nextd (moveBox (adjacentCoord nextd c) (adjacentCoord nextd (adjacentCoord nextd c)) list)
  | otherwise              = State c nextd list


isWon :: State -> Bool
isWon (State c d Empty)        = True
isWon (State c d (Entry e es)) = case maze e of
  Storage -> True && isWon (State c d es)
  _       -> False
  

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s 
  | isWon s        = s
  | key == "Right" = makeMove R s
  | key == "Up"    = makeMove U s
  | key == "Left"  = makeMove L s
  | key == "Down"  = makeMove D s
  | otherwise      = s
handleEvent _ s    = s

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(-0.3,0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,0),(-0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (State c d li) = case isWon (State c d li) of
  True  -> winScreen & atCoord c (player d) & pictureOfBoxes li & pictureOfMaze
  False -> atCoord c (player d) & pictureOfBoxes li & pictureOfMaze

winScreen :: Picture
winScreen = scaled 3 3 (text "Game Won!")


-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-- The main function

main :: IO ()
main = runInteraction (resetable (withStartScreen  sokoban))
