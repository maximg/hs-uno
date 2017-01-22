import Data.List
import Data.Ord
import Data.Maybe
import System.Random
import Uno.Deck as UD
import Uno.Game


-- UI: generic item selector
selectItem header prompt items showItem validateFx = do
    putStrLn header
    let numberedItems = zipWith (\n item -> show n ++ " - " ++ (showItem item)) [0..] items
    putStr $ unlines numberedItems
    putStrLn prompt
    numberString <- getLine
    let number = read numberString
    if (number < length items) && validateFx(items !! number)
        then do return $ items !! number
        else do putStrLn "Invalid choice, try again!"
                selectItem header prompt items showItem validateFx

-- for now use dedicated functions, later consider redefining 'show'
showColor Red    = "R"
showColor Green  = "G"
showColor Blue   = "B"
showColor Yellow = "Y"

showSymbol Zero      = "0 "
showSymbol One       = "1 "
showSymbol Two       = "2 "
showSymbol Three     = "3 "
showSymbol Four      = "4 "
showSymbol Five      = "5 "
showSymbol Six       = "6 "
showSymbol Seven     = "7 "
showSymbol Eight     = "8 "
showSymbol Nine      = "9 "
showSymbol Reverse   = "r "
showSymbol Take2     = "+2"
showSymbol Skip      = "s "

showCard :: UnoCard -> String
showCard (Card c s) = (showColor c) ++ (showSymbol s)

showLead (LeadCard c) = showCard c
showLead (LeadColor c) = showColor c

showMove (PlayCard c) = showCard c
showMove (SkipMove n) = "skips and takes " ++ (show n)


human n h = Player n h selectMove where
    selectMove lead hand = do
        let validateChoice card = (PlayCard card) `elem` (allowedMoves hand lead)
        selectedCard <- selectItem "Here are your cards:" "Which one do you want to play?" (sort hand) showCard validateChoice
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
    putStrLn $ "Top card is " ++ (showLead (gsLead gs))
    playerMove <- selectMove' $ allowedMoves (plHand player) (gsLead gs)
    return $ makeMove player gs playerMove
    where
        selectMove' [] = do
            putStrLn $ "No allowed moves, " ++ (plName player) ++ " skips and takes one"
            return $ SkipMove 1
        selectMove' _ = do
            playerMove <- (plSelectMove player) (gsLead gs) (plHand player)
            -- FIXME: double-check player's choice?
            putStrLn $ (plName player) ++ " plays " ++ (showMove playerMove)
            return playerMove


playGame (player:rest) gs = do
    putStrLn $ unwords $ replicate 30 "-"
    -- debug
    putStrLn $ (plName player) ++ ": " ++ (unwords $ sort $ map showCard $ plHand player)
    putStrLn $ "Deck: " ++ (unwords $ take 10 $ map showCard $ gsDeck gs)
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
