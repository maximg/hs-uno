module Uno.Deck
( Symbol (..)
, Color (..)
, Penalty
, UnoCard (..)
, Deck
, deck
) where

data Symbol = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Reverse | Skip | Take2
    deriving (Eq, Enum, Show)

data Color = Red | Green | Blue | Yellow | Any
    deriving (Eq, Enum, Show)

type Penalty = Int

data UnoCard = Card Color Symbol | Black Color Penalty
    deriving (Eq, Show)

type Deck = [UnoCard]

deck =
    [Card c s |
        c <- [Red .. Yellow],
        s <- Zero : [s' | s' <- [One .. Take2], _ <- [1,2]] ]
    ++ [x | x <- [Black Any 0, Black Any 4], _ <- [1..4]]
