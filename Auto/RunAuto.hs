import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import Text.Read
import Auto


assertNatural :: Maybe Int -> Maybe Int
assertNatural (Just x) = if (x >= 0) then (Just x) else Nothing
assertNatural Nothing = Nothing


readMaybeNatural :: String -> Maybe Int
readMaybeNatural = assertNatural . readMaybe


parseTrans :: [String] -> Maybe [(Int, Char, [Int])]
parseTrans (state:chars:rest) = if ((any (not . isUpper) chars) || (isNothing maybeState) || (any (isNothing) maybeRest)) then Nothing else (Just (map (\c -> (fromJust maybeState, c, map (fromJust) maybeRest)) chars))
    where
        maybeState = readMaybeNatural state
        maybeRest = map (readMaybeNatural) rest
parseTrans _ = Nothing


concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe l = if ((length filtered) /= (length l)) then Nothing else (Just (concat filtered))
    where
        filtered = catMaybes l


printFailure :: IO ()
printFailure = putStrLn "BAD INPUT" 
        

runAuto :: FilePath -> IO ()
runAuto filename = do
    handle <- openFile filename ReadMode
    let hGetLine' = hGetLine handle
    line <- hGetLine'
    let numStates = readMaybeNatural line
    line <- hGetLine'
    let initStates = readMaybe line :: Maybe [Int]
    line <- hGetLine'
    let acceptingStates = readMaybe line :: Maybe [Int]
    
    input <- hGetContents handle
    let (word:inputLines) = (reverse . lines) input
    let transitions = concatMaybe (map (parseTrans . words) inputLines)
    
    if ((isNothing transitions) || (isNothing numStates) || (isNothing initStates) || (isNothing acceptingStates) || (any (not . isUpper) word) || (any (< 0) ((fromJust initStates) ++ (fromJust acceptingStates))))
       then 
            printFailure
       else do
            let auto = fromLists [1.. (fromJust numStates)] (fromJust initStates) (fromJust acceptingStates) (fromJust transitions)
            print (accepts auto word)
      
      
parse :: [FilePath] -> IO ()
parse [filename] = runAuto filename
parse _ = printFailure
      

main :: IO ()
main = getArgs >>= parse
    