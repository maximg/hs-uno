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



human n h = Player n h selectMove where
    selectMove lead hand = do
        let validateChoice card = (PlayCard card) `elem` (allowedMoves hand lead)
        selectedCard <- selectItem "Here are your cards:" "Which one do you want to play?" hand validateChoice
        return $ PlayCard selectedCard


-- a very simple select move strategy - make the first possible move or skip and take
dumbComputer n h = Player n h selectMove where
    selectMove lead hand = do
        return $ head $ allowedMoves hand lead


handlePlayer :: Player
               -> GameState
               -> IO (Player, GameState)

-- FIXME: mixes UI and game logic
handlePlayer player gs = do
    putStrLn $ "Top card is " ++ (show (gsLead gs))
    playerMove <- selectMove' $ allowedMoves (plHand player) (gsLead gs)
    return $ makeMove player gs playerMove
    where
        selectMove' [] = do
            putStrLn $ "No allowed moves, player " ++ (plName player) ++ " skips and takes one"
            return $ SkipMove 1
        selectMove' _ = do
            playerMove <- (plSelectMove player) (gsLead gs) (plHand player)
            -- FIXME: double-check player's choice?
            putStrLn $ "Player " ++ (plName player) ++ " plays " ++ (show playerMove)
            return playerMove


playGame (player:rest) gs = do
    putStrLn $ unwords $ replicate 30 "-"
    (player',gs') <- handlePlayer player gs
    if null (plHand player') then do return player'
                             else do
                                -- FIXME: handle direction changes
                                playGame (rest ++ [player']) gs'

main = do
    let nPlayers = 2
    putStrLn $ "Starting an Uno game with " ++ (show nPlayers) ++ " players"
    gen <- getStdGen

    let (hand1:hand2:_, gs) = startGame (shuffle UD.deck gen) nPlayers
    let players = [human "Maxim" hand1, dumbComputer "MacBook" hand2]

    winner <- playGame players gs
    putStrLn $ "Player " ++ (plName winner) ++ " wins!"
