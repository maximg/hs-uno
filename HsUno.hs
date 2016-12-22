
-- See also https://github.com/epsilonhalbe/Uno

data Symbol = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Reverse | Skip | Take2
    deriving (Eq, Enum, Show)

data Color = Red | Green | Blue | Yellow | Any
    deriving (Eq, Enum, Show)

type Penalty = Int

data UnoCard = Card Color Symbol | Black Color Penalty
    deriving (Eq, Show)

card c s   = Card c s

type Deck = [UnoCard]
type Hand = [UnoCard]

type Direction = Int -- +1 CW, -1 CCW

deck =
    [card c s |
        c <- [Red .. Yellow],
        s <- Zero : [s' | s' <- [One .. Take2], _ <- [1,2]] ]
    ++ [x | x <- [Black Any 0, Black Any 4], _ <- [1..4]]

-- outcome of the last player's move
data Lead = LeadCard UnoCard | LeadColor Color
    deriving (Eq, Show)

data Move = PlayCard UnoCard | SkipMove p
    deriving (Eq, Show)

tryPlayCard :: Lead -> UnoCard -> Maybe Move

tryPlayCard (LeadCard Card _ Skip) c@(Card _ Skip) = Just PlayCard c
tryPlayCard (LeadCard Card _ Skip) _               = Nothing

tryPlayCard (LeadCard Card _ Take2) c@(Card _ Take2) = Just PlayCard c
tryPlayCard (LeadCard Card _ Take2) _                = Nothing

tryPlayCard _ c@(Black _ _)   = Just PlayCard c

tryPlayCard (LeadCard Card c1 s1) c@(Card c2 s2) | c1 == c2 || s1 == s2 = Just PlayCard c
                                                 | otherwise = Nothing

-- next rule covers reverse too
tryPlayCard (LeadColor c1)        c@(Card c2 _)  | c1 == c2 = Just PlayCard c
                                                 | otherwise = Nothing

tryPlayCard _ _ = Nothing


allowedMoves :: Hand -> Lead -> [Move]
allowedMoves hand lead = [ [x] | x <- hand, tryPlayCard lead x]


-- assume the move is allowed
nextLead :: Lead -> Move -> Lead
nextLead (LeadCard Card c Skip) (SkipMove _) = LeadColor c
nextLead (LeadCard Card _ Skip) (PlayCard c@(Card _ Skip)) = LeadCard c

nextLead (LeadCard Card c Take2) (SkipMove _) = LeadColor c
nextLead (LeadCard Card _ Take2) (PlayCard Card c Take2) = LeadColor c

nextLead _ (PlayCard Black c _) = LeadColor c

nextLead (LeadCard Card _ _) (PlayCard Card c Reverse) = LeadColor c

nextLead (LeadCard Card _ _) (PlayCard c@(Card _ _)) = LeadCard c

-- assume the move is allowed
nextDirection :: Direction -> Move -> Direction
nextDirection d (PlayCard card _ Reverse) = -d
nextDirection d _ = _

-- assume the move is allowed
nextPenalty :: Int -> Move -> Int
nextPenalty p (PlayCard Black _ v) = p+v
nextPenalty p (PlayCard Card _ Take2) = p+2
nextPenalty p (SkipMove _) = 0


nextPlayer n p dir = (p + n + dir) `mod` n



type Game = [GameState]
data GameState = GameState { gHands  :: [Hand],
                             gPlayer :: Int,
                             gDirection :: Direction,
                             gLead   :: Lead,
                             gPenalty:: Penalty,
                             gDeck   :: Deck,
                             gUsed   :: Deck }
    deriving (Eq, Show)

player gs = gHands gs !! gPlayer gs

isWin = null . player

-- stub, just pick the first possible move
selectMove :: GameState -> [Move] -> Move
selectMove _ [] = SkipMove 1
selectMove _ (x:_) = x

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

hand1 = [black]
nand2 = [card Red Zero]
tstg1 = GameState [hand1,hand2] 0 1 Red [] []

-- mapping card to card actions
-- cardAction :: UnoCard -> CardAction


-- Maybe vs []: compare `foldr` with `maybe`
-- Lenses?
