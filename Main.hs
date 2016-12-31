import Data.List
import Data.Ord
import System.Random
import Uno.Deck as UD
import Uno.Game


-- generic item selector
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


data Human = Human { hName :: String }

instance Player Human where
    selectMove player lead hand = do
        let validateChoice card = (PlayCard card) `elem` (allowedMoves hand lead)
        selectedCard <- selectItem "Here are your cards:" "Which one do you want to play?" hand validateChoice
        return $ PlayCard selectedCard
    nameOf player = hName player


-- a very simple select move strategy - make the first possible move or skip and take
data DumbComputer = DumbComputer { dcName :: String }
instance Player DumbComputer where
    selectMove player lead hand = do
        return $ head $ allowedMoves hand lead
    nameOf player = dcName player


handlePlayer :: (Player a) => a
               -> Hand
               -> GameState
               -> IO (Hand, GameState)

handlePlayer player hand gs = do
    putStrLn $ "Top card is " ++ (show (gsLead gs))
    playerMove <- selectMove' $ allowedMoves hand (gsLead gs)
    return $ makeMove (hand, gs) playerMove
    where
        selectMove' [] = do
            putStrLn $ "No allowed moves, player " ++ (nameOf player) ++ " skips and takes one"
            return $ SkipMove 1
        selectMove' _ = do
            playerMove <- selectMove player (gsLead gs) hand
            -- FIXME: double-check player's choice?
            putStrLn $ "Player " ++ (nameOf player) ++ " plays " ++ (show playerMove)
            return playerMove


-- this is the game cycle
playGame (hand1:hand2:[], gs) = do
    let player1 = (Human "Maxim")
    let player2 = (DumbComputer "MacBook")

    putStrLn $ unwords $ replicate 30 "-"
    (hand1', gs') <- handlePlayer player1 hand1 gs
    if null hand1'
        then return (nameOf player1)
        else do
            (hand2', gs'') <- handlePlayer player2 hand2 gs'
            if null hand2'
                then return (nameOf player2)
                else playGame (hand1':hand2':[], gs'')

main = do
    let nPlayers = 2
    putStrLn $ "Starting an Uno game with " ++ (show nPlayers) ++ " players"
    gen <- getStdGen
    winner <- playGame $ startGame (shuffle UD.deck gen) nPlayers
    putStrLn $ "Player " ++ winner ++ " wins!"
