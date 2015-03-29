module Auto( Auto
           , accepts
           , emptyA, epsA, symA
           , leftA, rightA, sumA, thenA
           , fromLists, toLists
           ) where

           
import Data.List
           
           
data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

                  
accepts :: Eq q => Auto a q -> [a] -> Bool
accepts auto word = any (isAccepting auto) (foldl (\possible c -> (nub . concat . map (\v -> transition auto v c)) possible) (initStates auto) word)


emptyA :: Auto a ()
emptyA = A [] [] (\s -> False) (\_ _ -> [])


epsA :: Auto a ()
epsA = A [()] [()] (== ()) (\s c -> [])


symA :: Eq a => a -> Auto a Bool
symA c = A [True, False] [False] (id) (\s c' -> if ((not s) && (c' == c)) then [True] else []) 


leftA :: Auto a q -> Auto a (Either q r) 
leftA auto = A (map (Left) (states auto)) (map (Left) (initStates auto)) (either (isAccepting auto) (\_ -> False)) (either (\v c -> map (Left) (transition auto v c)) (\_ _ -> []))


rightA :: Auto a q -> Auto a (Either p q) 
rightA auto = A (map (Right) (states auto)) (map (Right) (initStates auto)) (either (\_ -> False) (isAccepting auto)) (either (\_ _ -> []) (\v c -> map (Right) (transition auto v c)))


thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA auto1 auto2 = A ((states leftA1) ++ (states rightA2)) (initStates leftA1) (isAccepting rightA2) (either (\v c -> (transition leftA1 (Left v) c) ++ (if (isAccepting auto1 v) then (foldl (\res w -> res ++ (transition rightA2 w c)) [] (initStates rightA2)) else [])) (\v -> transition rightA2 (Right v)))
    where
        leftA1 = leftA auto1
        rightA2 = rightA auto2


sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA auto1 auto2 = A ((states leftA1) ++ (states rightA2)) ((initStates leftA1) ++ (initStates rightA2)) (either (isAccepting auto1) (isAccepting auto2)) (either (\v c -> map (Left) (transition auto1 v c)) (\v c -> map (Right) (transition auto2 v c)))
    where
        leftA1 = leftA auto1
        rightA2 = rightA auto2


fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
fromLists s iS iA t = A s iS (foldl (\f v -> (\u -> (u == v) || (f u))) (\_ -> False) iA) (foldl (\f (v, c, neigh) -> (\u l -> if (u == v) && (c == l) then neigh else (f u l))) (\_ _ -> []) t) 


toLists :: (Enum a,Bounded a) => Auto a q -> ([q], [q], [q], [(q,a,[q])])
toLists auto = ((states auto), (initStates auto), (filter (isAccepting auto) (states auto)), 
        (filter (\(v, c, neigh) -> not (null neigh)) [(v, c, (transition auto v c)) | v <- (states auto), c <- (enumFrom minBound)]))