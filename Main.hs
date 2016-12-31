{-# LANGUAGE ExistentialQuantification #-}

import Data.List
import Data.Ord
import Data.Maybe
import System.Random
import Uno.Deck as UD
import Uno.Game


-- UI: generic item selector
selectItem header prompt items validateFx = do
    putStrLn header
    let numberedItems = zipWith (\n item -> show n ++ " - " ++ show item) [0..] items
    putStr $ unlines numberedItems
    putStrLn prompt
    numberString <- getLine
    let number = read numberString
    if (number < length items) && validateFx(items !! number)
        then do return $ items !! number
        else do putStrLn "Invalid choice, try again!"
                selectItem header prompt items validateFx


-- see http://stackoverflow.com/questions/12774056/haskell-list-of-instances-of-a-typeclass
-- and https://wiki.haskell.org/Existential_type#Dynamic_dispatch_mechanism_of_OOP
data PlayerD = forall a. Player a => PlayerD a
instance Player PlayerD where
    nameOf (PlayerD a) = nameOf a
    selectMove (PlayerD a) lead hand = selectMove a lead hand


data Human = Human { hName :: String }

instance Player Human where
    selectMove player lead hand = do
        let validateChoice card = (PlayCard card) `elem` (allowedMoves hand lead)
        selectedCard <- selectItem "Here are your cards:" "Which one do you want to play?" hand validateChoice
        return $ PlayCard selectedCard
    nameOf player = hName player

human n = PlayerD (Human n)

-- a very simple select move strategy - make the first possible move or skip and take
data DumbComputer = DumbComputer { dcName :: String }
instance Player DumbComputer where
    selectMove player lead hand = do
        return $ head $ allowedMoves hand lead
    nameOf player = dcName player

dumbComputer n = PlayerD (DumbComputer n)


handlePlayer :: PlayerD
               -> Hand
               -> GameState
               -> IO (Hand, GameState)

-- FIXME: mixes UI and game logic
handlePlayer player hand gs = do
    putStrLn $ "Top card is " ++ (show (gsLead gs))
    playerMove <- selectMove' $ allowedMoves hand (gsLead gs)
    return $ makeMove player (hand, gs) playerMove
    where
        selectMove' [] = do
            putStrLn $ "No allowed moves, player " ++ (nameOf player) ++ " skips and takes one"
            return $ SkipMove 1
        selectMove' _ = do
            playerMove <- selectMove player (gsLead gs) hand
            -- FIXME: double-check player's choice?
            putStrLn $ "Player " ++ (nameOf player) ++ " plays " ++ (show playerMove)
            return playerMove


playGame ((hand,player):rest) gs = do
    putStrLn $ unwords $ replicate 30 "-"
    if isJust (gsWinner gs) then do return $ fromJust $ gsWinner gs
                            else do
                                (hand', gs') <- handlePlayer player hand gs
                                playGame (rest ++ [(hand',player)]) gs'

main = do
    let nPlayers = 2
    putStrLn $ "Starting an Uno game with " ++ (show nPlayers) ++ " players"
    gen <- getStdGen

    let (hand1:hand2:_, gs) = startGame (shuffle UD.deck gen) nPlayers
    let players = [(hand1, human "Maxim")
                  ,(hand2, dumbComputer "MacBook")]

    winner <- playGame players gs
    putStrLn $ "Player " ++ winner ++ " wins!"
