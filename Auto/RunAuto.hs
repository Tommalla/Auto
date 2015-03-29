import Text.Read
import System.Exit


getInt :: IO Int
getInt = do
    line <- getLine
    case readMaybe line of
        Just x -> return x
        Nothing -> putStrLn "BAD INPUT" >> exitWith (ExitFailure 1)


main = do
    nStates <- getInt
    

    print nStates
    