module Uno.Deck
( Symbol (..)
, Color (..)
, Penalty
, UnoCard (..)
, Deck
, deck
, shuffle
) where

import System.Random
import Data.List
import Data.Ord

data Symbol = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Reverse | Skip | Take2
    deriving (Eq, Enum, Show, Ord)

data Color = Red | Green | Blue | Yellow | Any
    deriving (Eq, Enum, Show, Ord)

type Penalty = Int

data UnoCard = Card Color Symbol | Black Color Penalty
    deriving (Eq, Show, Ord)

type Deck = [UnoCard]

deck =
    [Card c s |
        c <- [Red .. Yellow],
        s <- Zero : [s' | s' <- [One .. Take2], _ <- [1,2]] ]
    -- ++ [x | x <- [Black Any 0, Black Any 4], _ <- [1..4]]

shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle deck gen =
    let deckSize = length(deck)
        idx = take deckSize $ nub $ randomRs (1,deckSize) gen :: [Int]
    in map snd $ sortBy (comparing fst) $ zip idx deck
