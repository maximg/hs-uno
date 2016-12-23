
import Data.Maybe
import Data.List

data Symbol = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Reverse | Skip | Take2
    deriving (Eq, Enum, Show)

data Color = Red | Green | Blue | Yellow | Any
    deriving (Eq, Enum, Show)

type Penalty = Int

data UnoCard = Card Color Symbol | Black Color Penalty
    deriving (Eq, Show)

card c s = Card c s
deck =
    [card c s |
        c <- [Red .. Yellow],
        s <- Zero : [s' | s' <- [One .. Take2], _ <- [1,2]] ]
    ++ [x | x <- [Black Any 0, Black Any 4], _ <- [1..4]]

data Lead = LeadCard UnoCard | LeadColor Color
    deriving (Eq, Show)

data Move = PlayCard UnoCard | SkipMove Penalty
    deriving (Eq, Show)

tryPlayCard :: Lead -> UnoCard -> Maybe Move

tryPlayCard (LeadCard (Card _ Skip)) c@(Card _ Skip) = Just (PlayCard c)
tryPlayCard (LeadCard (Card _ Skip)) _               = Nothing

tryPlayCard (LeadCard (Card _ Take2)) c@(Card _ Take2) = Just (PlayCard c)
tryPlayCard (LeadCard (Card _ Take2)) _                = Nothing

tryPlayCard _ c@(Black _ _)   = Just (PlayCard c)

tryPlayCard (LeadCard (Card c1 s1)) c@(Card c2 s2) | c1 == c2 || s1 == s2 = Just (PlayCard c)
                                                 | otherwise = Nothing

-- next rule covers reverse too
tryPlayCard (LeadColor c1)        c@(Card c2 _)  | c1 == c2 = Just (PlayCard c)
                                                 | otherwise = Nothing

tryPlayCard _ _ = Nothing

type Deck = [UnoCard]
type Hand = [UnoCard]

type Direction = Int -- +1 CW, -1 CCW

allowedMoves :: Hand -> Lead -> [Move]
allowedMoves hand lead = catMaybes $ do
    map (tryPlayCard lead) hand

nextLead' :: Lead -> Move -> Lead
nextLead' (LeadCard (Card c Skip)) (SkipMove _) = LeadColor c
nextLead' (LeadCard (Card _ Skip)) (PlayCard c@(Card _ Skip)) = LeadCard c

nextLead' (LeadCard (Card c Take2)) (SkipMove _) = LeadColor c
nextLead' (LeadCard (Card _ Take2)) (PlayCard (Card c Take2)) = LeadColor c

nextLead' (LeadCard (Black c _)) (SkipMove _) = LeadColor c
nextLead' _ (PlayCard (Black c _)) = LeadColor c

nextLead' (LeadCard (Card c Reverse)) (SkipMove _) = LeadColor c
nextLead' (LeadCard (Card _ _)) (PlayCard (Card c Reverse)) = LeadColor c

nextLead' (LeadCard (Card _ _)) (PlayCard c) = LeadCard c

nextLead' lead (SkipMove _) = lead

-- to make args consistent with nextDirection and nextPenalty. refactor later
nextLead = flip nextLead'

nextDirection :: Move -> Direction -> Direction
nextDirection (PlayCard (Card _ Reverse)) = (* (-1))
nextDirection _ = id

nextPenalty :: Move -> Penalty -> Penalty
nextPenalty (PlayCard (Black _ p))    = (+ p)
nextPenalty (PlayCard (Card _ Take2)) = (+ 2)
nextPenalty _ = (* 0)

-- a very simple select move strategy - make the first possible move or skip and take
selectMove :: [Move] -> Move
selectMove [] = SkipMove 1
selectMove (x:_) = x

-- TODO: account for penalty

applyMove :: Move -> (Hand, Deck) -> (Hand, Deck)
applyMove (PlayCard c) (hand, deck) = (delete c hand, deck)
applyMove (SkipMove n) (hand, deck) = (hand ++ (take n deck), drop n deck)

makeMove :: (Lead, Hand, Penalty, Direction, Deck) -> (Lead, Hand, Penalty, Direction, Deck, Move)
makeMove (lead, hand, penalty, direction, deck) =
    let move = selectMove $ allowedMoves hand lead
        (hand', deck') = applyMove move (hand, deck)
        lead'      = nextLead      move lead
        penalty'   = nextPenalty   move penalty
        direction' = nextDirection move direction
    in (lead', hand', penalty', direction', deck', move)

r1 = Card Red One
r2 = Card Red Two
r3 = Card Red Three
g4 = Card Green Four
blackR0 = Black Red 0
rSkip = Card Red Skip

makeMove (LeadCard r1, [r2], 0, 1, [r3])
makeMove (LeadCard r1, [g4], 0, 1, [r3])
makeMove (LeadCard rSkip, [g4], 0, 1, [r3])
makeMove (LeadCard blackR0, [g4], 0, 1, [r3])
