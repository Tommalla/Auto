module Auto( Auto
           , accepts
           , emptyA, epsA, symA
           , leftA, rightA, sumA, thenA
           , fromLists, toLists
           ) where

           
data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

                  
accepts :: Eq q => Auto a q -> [a] -> Bool
accepts auto word = foldl (\res v -> res || (isAccepting auto v) || (fst (dfs v word []))) False (initStates auto)
    where
--         dfs :: Eq q => q -> [a] -> [q] -> (Bool, [q])
        dfs u (c:wordTail) prevVis = foldl (\(tmpRes, tmpVis) w -> if tmpRes then (tmpRes, tmpVis) else (if (isAccepting auto w) then (True, tmpVis) else (if (not (elem w tmpVis)) then (dfs w wordTail (w:tmpVis)) else (tmpRes, tmpVis)))) (False, prevVis) (transition auto u c)
        dfs _ [] prevVis = (False, prevVis)


emptyA :: Auto a ()
emptyA = A [] [] (\s -> False) (\s c -> [])


epsA :: Auto a ()
epsA = A [()] [()] (== ()) (\s c -> []) 


symA :: Eq a => a -> Auto a Bool
symA c = A [True, False] [False] (id) (\s c' -> if ((not s) && (c' == c)) then [True] else []) 


leftA :: Auto a q -> Auto a (Either q r) 
leftA auto = A (map (Left) (states auto)) (map (Left) (initStates auto)) (either (isAccepting auto) (\_ -> False)) (either (\v c -> map (Left) (transition auto v c)) (\_ _ -> []))


rightA :: Auto a q -> Auto a (Either p q) 
rightA auto = A (map (Right) (states auto)) (map (Right) (initStates auto)) (either (\_ -> False) (isAccepting auto)) (either (\_ _ -> []) (\v c -> map (Right) (transition auto v c)))


fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
fromLists s iS iA t = A s iS (foldl (\f v -> (\u -> (u == v) || (f u))) (\_ -> False) iA) (foldl (\f (v, c, neigh) -> (\u l -> if (u == v) && (c == l) then neigh else (f u l))) (\_ _ -> []) t) 


toLists :: (Enum a,Bounded a) => Auto a q -> ([q], [q], [q], [(q,a,[q])])
toLists auto = ((states auto), (initStates auto), (filter (isAccepting auto) (states auto)), 
        (filter (\(v, c, neigh) -> not (null neigh)) [(v, c, (transition auto v c)) | v <- (states auto), c <- (enumFrom minBound)]))


thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA = undefined


sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA auto1 auto2 = A ((states leftA1) ++ (states rightA2)) ((initStates leftA1) ++ (initStates rightA2)) (either (isAccepting auto1) (isAccepting auto2)) (either (\v c -> map (Left) (transition auto1 v c)) (\v c -> map (Right) (transition auto2 v c)))
    where
        leftA1 = leftA auto1
        rightA2 = rightA auto2
