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


data Coord = C Integer Integer deriving (Eq)

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord = undefined

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case (maze c) of
 Box -> Ground
 otherwise -> maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes cs c 
 | member c cs = Box
 | otherwise   = noBoxMaze c

member :: (Eq a) => a -> List a -> Bool
member _ Empty = False
member a (Entry a' as) 
 | a == a' = True
 | otherwise = member a as

-- The state

data State = State Direction Coord (List Coord)

getAllBoxes :: Integer -> Integer -> Integer -> Integer -> List Coord
getAllBoxes xMin xMax yMin yMax = listToList $ filter (\c -> (maze c) == Box) [(C x y) | x <- [xMin..xMax], y <- [yMin..yMax]]

listToList :: [a] -> List a
listToList [] = Empty
listToList (x:xs) = Entry x (listToList xs)

initialBoxes :: List Coord
initialBoxes = getAllBoxes (-10) 10 (-10) 10

initialState :: State
initialState = State R (C 1 1) initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress k) s
 | isWon s = s
 | k == "Right" = handleValidKey R s 
 | k == "Up" = handleValidKey U s 
 | k == "Left" = handleValidKey L s 
 | k == "Down" = handleValidKey D s 
handleEvent _ s = s

handleValidKey :: Direction -> State -> State
handleValidKey moveDir (State d c cs) = 
 let newCoord = (adjacentCoord moveDir c)
 in
   if member newCoord cs then handleBoxMove moveDir newCoord (State d c cs)
   else handleNoBoxMove moveDir newCoord (State d c cs)
   
handleBoxMove :: Direction -> Coord -> State -> State
handleBoxMove moveDir boxCoord (State d c cs) = 
 let moveBoxCoord = adjacentCoord moveDir boxCoord
     potentialTile = mazeWithBoxes cs moveBoxCoord
 in
  if potentialTile == Ground || potentialTile == Storage
  then State moveDir boxCoord (updateBoxes boxCoord moveBoxCoord cs)
  else (State moveDir c cs)

  
handleNoBoxMove :: Direction -> Coord -> State -> State
handleNoBoxMove d newCoord (State _ c cs) = 
 case (noBoxMaze newCoord) of
  Storage   -> (State d newCoord cs)
  Ground    -> (State d newCoord cs)
  otherwise -> (State d c cs)

updateBoxes :: Coord -> Coord -> List Coord -> List Coord
updateBoxes currentBoxCoord newBoxCoord (Entry c cs) = 
 if c == currentBoxCoord then (Entry newBoxCoord cs)
 else (Entry c (updateBoxes currentBoxCoord newBoxCoord cs))



-- Drawing

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
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
player R = colored red $ lettering ">"
player U = rotated (pi/2) $ player R
player L = rotated (pi/2) $ player U
player D = rotated (pi/2) $ player L

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (State d c cs) 
 | isWon (State d c cs) = winScreen
 | otherwise = atCoord c (player d) & pictureOfBoxes cs & pictureOfMaze

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
    
-- Winning
winScreen :: Picture
winScreen = scaled 3 3 (text "You won!")

isWon :: State -> Bool
isWon (State _ _ cs) = foldList (\c b -> isOnStorage c && b) True cs 

isOnStorage :: Coord -> Bool
isOnStorage c = noBoxMaze c == Storage

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ b Empty = b
foldList f b (Entry a as) = foldList f (f a b) as 


-- The main function

main :: IO ()
main = runInteraction (resetable (withStartScreen sokoban))

