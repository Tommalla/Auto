import Data.Char
import Data.Maybe
import System.Exit
import System.IO
import Text.Read
import Auto


parseTrans :: [String] -> Maybe [(Maybe Int, Char, [Maybe Int])]  -- TODO FIXME get rid of the maybes!
parseTrans (state:chars:rest) = if (any (not . isUpper) chars) then Nothing else (Just (map (\c -> (readMaybe state, c, map (readMaybe) rest)) chars))
parseTrans _ = Nothing


maybeList :: Maybe [a] -> [Maybe a]
maybeList (Just l) = map (\elem -> Just elem) l
maybeList Nothing = [Nothing]


concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe l = if ((length filtered) /= (length l)) then Nothing else (Just (concat filtered))
    where
        filtered = catMaybes l


main = do
    handle <- openFile "test2.in" ReadMode
    let hGetLine' = hGetLine handle
    line <- hGetLine'
    let numStates = readMaybe line :: Maybe Int
    line <- hGetLine'
    let initStates = readMaybe line :: Maybe [Int]
    line <- hGetLine'
    let acceptingStates = readMaybe line :: Maybe [Int]
    
    input <- hGetContents handle
    let (word:inputLines) = (reverse . lines) input
    let transitions = concatMaybe (map (parseTrans . words) inputLines)
    if ((isNothing transitions) || (isNothing numStates) || (isNothing initStates) || (isNothing acceptingStates)) then (putStrLn "BAD INPUT" >> exitFailure) else return ()
    
    let auto = fromLists (maybeList (Just [1.. (fromJust numStates)])) (maybeList initStates) (maybeList acceptingStates) (fromJust transitions)
    if (any (not. isUpper) word) then (putStrLn "BAD INPUT" >> exitFailure) else return ()
    print (accepts auto word)