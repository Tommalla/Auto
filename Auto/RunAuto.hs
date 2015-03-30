import Data.Char
import Data.Maybe
import System.IO
import Text.Read
import Auto


parseTrans :: [String] -> Maybe [(Int, Char, [Int])]
parseTrans (state:chars:rest) = if ((any (not . isUpper) chars) || (isNothing maybeState) || (any (isNothing) maybeRest)) then Nothing else (Just (map (\c -> (fromJust maybeState, c, map (fromJust) maybeRest)) chars))
    where
        maybeState = readMaybe state
        maybeRest = map (readMaybe) rest
parseTrans _ = Nothing


concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe l = if ((length filtered) /= (length l)) then Nothing else (Just (concat filtered))
    where
        filtered = catMaybes l


main = do
    handle <- openFile "test1.in" ReadMode
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
    
    if ((isNothing transitions) || (isNothing numStates) || (isNothing initStates) || (isNothing acceptingStates) || (any (not . isUpper) word)) 
       then 
            putStrLn "BAD INPUT" 
       else do
            let auto = fromLists [1.. (fromJust numStates)] (fromJust initStates) (fromJust acceptingStates) (fromJust transitions)
            print (accepts auto word)