import Data.List
import Data.Ord
import System.Random
import Uno.Deck as UD
import Uno.Game

class Player a where
    nameOf :: a -> String
    selectMove :: a -> Lead -> Hand -> IO Move

data HumanPlayer = Human String

instance Player HumanPlayer where
    selectMove player lead hand = do
        let validateChoice card = (PlayCard card) `elem` (allowedMoves hand lead)
        putStrLn "Here are your cards:"
        let numberedHand = zipWith (\n card -> show n ++ " - " ++ show card) [0..] hand
        putStr $ unlines numberedHand
        putStrLn "Which one do you want to play?"
        numberString <- getLine
        let number = read numberString
        if (number < length hand) && validateChoice(hand !! number)
            then do return (PlayCard $ hand !! number)
            else do putStrLn "Invalid choice, try again!"
                    selectMove player lead hand
    nameOf (Human n) = n


-- a very simple select move strategy - make the first possible move or skip and take
data DumbComputerPlayer = DumbComputer String
instance Player DumbComputerPlayer where
    selectMove player lead hand = do
        return $ head $ allowedMoves hand lead

    nameOf (DumbComputer n) = n


handlePlayer :: (Player a) => a
               -> (Hand, Lead, Uno.Game.Direction, Penalty, Deck, Deck, [Move])
               -> IO (Hand, Lead, Uno.Game.Direction, Penalty, Deck, Deck, [Move])

handlePlayer player gs@(hand, lead, _, _, _, _, _) = do
    putStrLn $ "Top card is " ++ (show lead)
    playerMove <- selectMove' $ allowedMoves hand lead
    return $ makeMove gs playerMove
    where
        selectMove' [] = do
            putStrLn $ "No allowed moves, player " ++ (nameOf player) ++ " skips and takes one"
            return $ SkipMove 1
        selectMove' _ = do
            playerMove <- selectMove player lead hand
            putStrLn $ "Player " ++ (nameOf player) ++ " plays " ++ (show playerMove)
            return playerMove


-- hack - to avoid explicitly specifying type of playGame for now
toInt :: Int -> Int
toInt = id

-- this is the game cycle
playGame (hand1:hand2:[], lead, dir, penalty, deck, used, moves) = do
    let player1 = (Human "Maxim")
    let player2 = (DumbComputer "MacBook")

    putStrLn $ unwords $ replicate 30 "-"
    (hand1', lead', dir', penalty', deck', used', moves') <- handlePlayer player1 (hand1, lead, dir, penalty, deck, used, moves)
    if null hand1'
        then return (nameOf player1)
        else do
            (hand2', lead'', dir'', penalty'', deck'', used'', moves'') <- handlePlayer player2 (hand2, lead', dir', penalty', deck', used', moves')
            if null hand2'
                then return (nameOf player2)
                else playGame (hand1':hand2':[], lead'', dir'', penalty'', deck'', used'', moves'')

main = do
    let nPlayers = 2
    putStrLn $ "Starting an Uno game with " ++ (show nPlayers) ++ " players"
    gen <- getStdGen
    winner <- playGame $ startGame (shuffle UD.deck gen) nPlayers
    putStrLn $ "Player " ++ winner ++ " wins!"
