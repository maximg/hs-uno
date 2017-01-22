{-# LANGUAGE NamedFieldPuns #-}

module Uno.Game
( startGame
, allowedMoves
, makeMove
, Move (..)
, Lead (..)
, Hand
, Penalty
, Player (..)
, GameState (..)
) where

import Data.Maybe
import Data.List
import Data.List.Split
import Uno.Deck

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

type Hand = [UnoCard]

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

nextLead' _ (PlayCard c) = LeadCard c

nextLead' lead (SkipMove _) = lead

-- to make args consistent with nextDirection and nextPenalty. refactor later
nextLead = flip nextLead'

nextDirection :: Move -> [Player] -> [Player]
nextDirection (PlayCard (Card _ Reverse)) = reverse
nextDirection _ = id

nextPenalty :: Move -> Penalty -> Penalty
nextPenalty (PlayCard (Black _ p))    = (+ p)
nextPenalty (PlayCard (Card _ Take2)) = (+ 2)
nextPenalty _ = (* 0)

-- TODO: account for penalty

applyMove :: Move -> (Hand, Deck) -> (Hand, Deck)
applyMove (PlayCard c) (hand, deck) = (delete c hand, deck)
applyMove (SkipMove n) (hand, deck) = (hand ++ (take n deck), drop n deck)


data Player = Player { plName :: String
                     , plHand :: Hand
                     , plSelectMove :: Lead -> Hand -> IO Move
                     }

data GameState = GameState { gsLead :: Lead
                           , gsDirection :: [Player] -> [Player]
                           , gsPenalty :: Penalty
                           , gsDeck :: Deck
                           , gsUsed :: Deck
                           , gsMoves :: [Move]
                           }

makeMove :: Player -> GameState -> Move -> (Player, GameState)
makeMove player (GameState{gsLead, gsDirection, gsPenalty, gsDeck, gsUsed, gsMoves}) move =
    let (hand', deck') = applyMove move (plHand player, gsDeck)
        usedCard (PlayCard c) = [c]
        usedCard _ = []
        player' = player { plHand = hand' }
    in (player', GameState { gsLead      = nextLead      move gsLead
                           , gsPenalty   = nextPenalty   move gsPenalty
                           , gsDeck      = deck'
                           , gsDirection = nextDirection move
                           , gsUsed      = (usedCard move) ++ gsUsed
                           , gsMoves     = move:gsMoves
                           })

startGame :: Deck -> Int -> ([Hand], GameState)
startGame deck n = let
    hands = take n $ chunksOf 8 deck
    (lead:deck') = drop (n * 8) deck
    in (hands, GameState (LeadCard lead) id 0 deck' [lead] [])
