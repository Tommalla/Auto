import System.IO
import Text.Read


main = do
    handle <- openFile "test0.in" ReadMode
    let hGetLine' = hGetLine handle
    line <- hGetLine'
    let numStates = readMaybe line :: Maybe Int
    line <- hGetLine'
    let initStates = readMaybe line :: Maybe [Int]
    line <- hGetLine'
    let acceptingStates = readMaybe line :: Maybe [Int]
    
    return 0