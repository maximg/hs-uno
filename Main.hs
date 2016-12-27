
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

toInt :: Int -> Int
toInt = id
-- this is the game cycle
playGame (hand:hands, lead, deck, dir, penalty, used, moves) = do
    putStrLn $ "Top card is " ++ (show lead)
    playerMove <- selectCard hand
    putStrLn $ "Player selected " ++ (show playerMove)
    -- check the move
    -- if fails, ask to try again
    -- apply the move
    -- check for a win
    -- make move by the computer
    -- check for a win
    -- repeat until win
    let win = True
    if win
        then return $ toInt 1
        else playGame (hand:hands, lead, deck, dir, penalty, used, moves)

main = do
    let nPlayers = 2
    putStrLn $ "Starting an Uno game with " ++ (show nPlayers) ++ " players"
    winner <- playGame $ startGame UD.deck nPlayers
    putStrLn $ "The winner is player " ++ (show winner)  
