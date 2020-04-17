import Text.Read
import qualified Data.Map as M

data Value = A | K | Q | J | Ten | Nine deriving (Show, Eq, Read, Ord)
data Suit = Heart | Diamond | Spade | Club deriving (Show, Eq, Read)
data Card = C Suit Value deriving (Show, Eq, Read, Ord)

instance Ord Suit where
 compare _ _ = EQ

data Player = P String Team [Card] deriving (Show, Eq, Ord)

data HandScore = HS Int Int
data GameScore = GS Int Int

playerOrder :: M.Map Player Player
playerOrder = M.fromList [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

type Team = Int
type CurrentPlayer = Player
type CurrentHand   = [(Player,Card)]
-- in normal hand, 4 turns happen. Control for ending the hand
type Turn = Int

data State = S Player Player Player Player CurrentPlayer Turn CurrentHand HandScore GameScore

p1,p2,p3,p4 :: Player
p1 = P "me"   1 [C Heart A,C Diamond J] 
p2 = P "you"  1 [C Club Nine] 
p3 = P "ok"   2 [C Spade Ten,C Club Q]
p4 = P "cool" 2 [C Heart K]

startState :: State
startState = S p1 p2 p3 p4 p2 0 [] (HS 0 0) (GS 0 0) 

main :: IO ()
main = handloop startState 

handloop :: State -> IO ()
handloop s = 
 do if (currentTurn s) < 4 
    then 
     do newState <- singleTurn s  
        handloop newState 
    else
     do scoreHand s
        putStrLn "hand over"
        return ()
   

singleTurn :: State -> IO State
singleTurn s = 
 do putStrLn $ "select card from your cards" ++ (show . getCards $ (currentPlayer s))
    stringInput <- getLine
    let readInput = readMaybe stringInput :: Maybe Card
    case readInput of
     Nothing -> do putStrLn "invalid"
                   return s
     Just c  -> if c `elem` (getCards $ currentPlayer s)
                 then
                  do
                   let newState = handleValidCardPlay c s
                   return newState
                 else
                  do putStrLn "you don't have that card"
                     return s

scoreHand :: State -> IO State 
scoreHand (S _ _ _ _ _ _ allCards _ _) = undefined 
 

getCards :: Player -> [Card]
getCards (P _ _ cs) = cs 

currentPlayer :: State -> Player
currentPlayer (S _ _ _ _ cp _ _ _ _) = cp

currentTurn :: State -> Turn 
currentTurn (S _ _ _ _ _ t _ _ _) = t

handleValidCardPlay :: Card -> State -> State
handleValidCardPlay c (S p1 p2 p3 p4 cp t ch hs gs) =  
 if      cp == p1 then S (removeCard c p1) p2 p3 p4 (playerOrder M.! cp) (t+1) ((p1,c) : ch) hs gs 
 else if cp == p2 then S p1 (removeCard c p2) p3 p4 (playerOrder M.! cp) (t+1) ((p2,c) : ch) hs gs 
 else if cp == p3 then S p1 p2 (removeCard c p3) p4 (playerOrder M.! cp) (t+1) ((p3,c) : ch) hs gs 
 else if cp == p4 then S p1 p2 p3 (removeCard c p4) (playerOrder M.! cp) (t+1) ((p4,c) : ch) hs gs 
 else error "current player not one of the four players"

removeCard :: Card -> Player -> Player
removeCard c (P t n cs) = P t n (remove c cs)
 where remove :: Card -> [Card] -> [Card]
       remove _ [] = []
       remove c (x:xs) = if x == c then xs else remove c xs

-- still need to build in off suit trump jacks
ordering :: Suit -> Suit -> Card -> Card -> Ordering
ordering trumpSuit leadSuit c1 c2 = 
 let s1 = getSuit c1
     s2 = getSuit c2
     v1 = getVal c1
     v2 = getVal c2
 in 
  if isTrump trumpSuit c1 && isTrump trumpSuit c2 then trumpOrdering trumpSuit c1 c1 
  else if isTrump trumpSuit c1 then GT
  else if isTrump trumpSuit c2 then LT
  else leadSuitOrdering leadSuit c1 c2 

leadSuitOrdering :: Suit -> Card -> Card -> Ordering
leadSuitOrdering = undefined

trumpOrdering :: Suit -> Card -> Card -> Ordering
trumpOrdering trump (C s1 v1) (C s2 v2) = 
 if v1 == v2 then 
  if s1 == trump then GT
  else LT
 else if v1 == J then GT
 else if v2 == J then LT
 else compare v1 v2

isTrump :: Suit -> Card -> Bool
isTrump trump (C s v) = 
 trump == s ||  
 case trump of
  Heart   -> (s == Diamond && v == J)
  Diamond -> (s == Heart   && v == J)
  Spade   -> (s == Club    && v == J)
  Club    -> (s == Spade   && v == J)


getSuit :: Card -> Suit
getSuit (C s _) = s

getVal :: Card -> Value
getVal (C _ v) = v

getName :: Player -> String
getName (P n _ _) = n

getTeam :: Player -> Team
getTeam (P _ t _) = t
