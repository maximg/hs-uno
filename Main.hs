
import Data.List
import Data.Ord
import System.Random
import Uno.Deck as UD
import Uno.Game

selectCard :: (Show a) => [a] -> IO a
selectCard cards = do
    putStrLn "Here are your cards:"
    let numberedHand = zipWith (\n card -> show n ++ " - " ++ show card) [0..] cards
    putStr $ unlines numberedHand
    putStrLn "Which one do you want to play?"
    numberString <- getLine     
    let number = read numberString
    if number < length cards
        then do putStrLn $ "You are playing " ++ show (cards !! number)
                return $ cards !! number
        else do putStrLn "Invalid choice, try again!"
                selectCard cards


-- a very simple select move strategy - make the first possible move or skip and take
selectFirstMove :: [Move] -> Move
selectFirstMove [] = SkipMove 1
selectFirstMove (x:_) = x


shuffleDeck :: (RandomGen g) => UD.Deck -> g -> UD.Deck
shuffleDeck deck gen =
    let deckSize = length(deck)
        idx = take deckSize $ nub $ randomRs (1,deckSize) gen :: [Int]
    in map snd $ sortBy (comparing fst) $ zip idx deck

-- hack - to avoid explicitly specifying type of playGame for now
toInt :: Int -> Int
toInt = id

-- this is the game cycle
playGame (hand1:hand2:[], lead, deck, dir, penalty, used, moves) = do
    putStrLn $ unwords $ replicate 30 "-"
    putStrLn $ "Top card is " ++ (show lead)
    playerMove <- selectCard hand1
    putStrLn $ "Player 1 plays " ++ (show playerMove)
    -- check the move
    -- if fails, ask to try again
    -- apply the move
    let (lead', hand1', penalty', dir', deck', moves') = makeMove (lead, hand1, penalty, dir, deck, moves) (\_ -> PlayCard playerMove)
    -- check for a win
    if null hand1'
        then return $ toInt 1
        else do
            -- make the move by the computer
            putStrLn $ "Top card is " ++ (show lead')
            let (lead'', hand2', penalty'', dir'', deck'', moves''@(move2:_)) = makeMove (lead', hand2, penalty', dir', deck', moves') selectFirstMove
            putStrLn $ "Player 2 plays " ++ (show move2)
            -- check for a win, repeat until a win
            if null hand2'
                then return $ toInt 2
                else playGame (hand1':hand2':[], lead'', deck'', dir'', penalty'', [], moves'')

main = do
    let nPlayers = 2
    putStrLn $ "Starting an Uno game with " ++ (show nPlayers) ++ " players"
    gen <- getStdGen
    winner <- playGame $ startGame (shuffleDeck UD.deck gen) nPlayers
    putStrLn $ "Player " ++ (show winner) ++ " wins!"
