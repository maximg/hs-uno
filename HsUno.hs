
-- See also https://github.com/epsilonhalbe/Uno

data Symbol = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Reverse | Skip | Take2
    deriving (Eq, Enum, Show)

data Color = Red | Green | Blue | Yellow
    deriving (Eq, Enum, Show)

data UnoCard = Card Color Symbol | Black | BlackTake4
    deriving (Eq, Show)

card c s   = Card c s
black      = Black
blackTake4 = BlackTake4

type Deck = [UnoCard]
type Hand = [UnoCard]

deck =
    [card c s |
        c <- [Red .. Yellow],
        s <- Zero : [s' | s' <- [One .. Take2], _ <- [1,2]] ]
    ++ [x | x <- [Black, BlackTake4], _ <- [1..4]]

data Lead = LeadCard UnoCard | LeadColor Color | LeadPenalty Color Int

type Game = [GameState]
data GameState = GameState { gHands  :: [Hand],
                             gPlayer :: Int,
                             gDirection :: Int, -- +1 or -1
                             gLead, -- outcome of last player's move
                             -- document the move?
                             -- extract top card separately?
                             gDeck   :: Deck } -- TODO: model 2 parts of deck and the shuffle
    deriving (Eq, Show)

player gs = gHands gs !! gPlayer gs

isWin gs = player gs == []`

pickCard (x:xs) = ([x],xs) -- TODO
-- takeCard

nextPlayer n p dir = (p + n + dir) `mod` n

data Move = Move { mPlayCards :: [UnoCard], -- which cards to play
                   mTakeCards :: Int,       -- how many cards to take
                   mColor :: [Color],         -- the color to request for black cards
                   mDirection :: Int }      -- 1: the same, -1: reverse

isAllowedMove :: UnoCard -> Lead -> Bool
isAllowedMove (Card _ Skip) (LeadCard UnoCard _ Skip) = true
isAllowedMove _             (LeadCard UnoCard _ Skip) = false
isAllowedMove Black      (LeadCard UnoCard _ Take2) = false
isAllowedMove BlackTake4 (LeadCard UnoCard _ Take2) = false
isAllowedMove Black      _ = true
isAllowedMove BlackTake4 _ = true
isAllowedMove (Card c s) (LeadCard UnoCard c1 s1) = c == c1 || s == s1
isAllowedMove (Card c _) (LeadColor c1) = c == c1
isAllowedMove _ _ = false


allowedMoves :: Hand -> Lead -> [[UnoCard]]
allowedMoves hand lead = [ [x] | x <- hand, isAllowedMove x lead]

-- almost complete except for the small problem
getPenalty :: Lead -> Int
getPenalty (LeadPenalty n) = n
getPenalty (LeadCard UnoCard _ Take2) = 2 -- problem, we do not accumulate penalty
getPenalty _ = 0

-- stub, just pick the first possible move
selectMove :: GameState -> [[UnoCard]] -> Move
selectMove [x] = Move { mPlayCards = x, mTakeCards = 0, mColor = [], mDirection = 1 }
selectMove _ = Move { mPlayCards = [], mTakeCards = 1, mColor = [], mDirection = 1 }

-- stub, just select the next player
applyMove :: GameState -> Move -> GameState
applyMove gs@ GameState { gHands = h, gPlayer = p, gDirection = dir, gDeck = d } _ =     
    gs { gPlayer = nextPlayer (length h) p dir }


makeMove :: GameState -> GameState
makeMove gs = applyMove gs $ selectMove gs $ allowedMoves (player gs) (gLead gs)


play :: GameState -> [GameState]
play gs | isWin gs = [gs]
        | otherwise = gs : play (makeMove gs)

-- deal _ _ = tstg1
-- play n d = [gs | gs <- foldr move tstg1 [1..3], not isWin gs]

tstg1 = GameState [[black],[card Red Zero]] 0 1 Red 0 []

-- mapping card to card actions
-- cardAction :: UnoCard -> CardAction


-- Maybe vs []: compare `foldr` with `maybe`
-- Lenses?
