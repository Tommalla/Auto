{- Tomasz Zakrzewski, tz336079
 - JPP 2014/2015
 -}

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
emptyA = A [] [] (\_ -> False) (\_ _ -> [])


epsA :: Auto a ()
epsA = A [()] [()] (== ()) (\_ _ -> [])


symA :: Eq a => a -> Auto a Bool
symA c = A [True, False] [False] (id) (\s c' -> if (not s) && (c == c') then [True] else []) 


leftA :: Auto a q -> Auto a (Either q r)
leftA auto = A (map (Left) (states auto))
               (map (Left) (initStates auto))
               (either (isAccepting auto) (\_ -> False))
               (either (\v c -> map (Left) (transition auto v c)) (\_ _ -> []))


rightA :: Auto a q -> Auto a (Either p q) 
rightA auto = A (map (Right) (states auto))
                (map (Right) (initStates auto))
                (either (\_ -> False) (isAccepting auto))
                (either (\_ _ -> []) (\v c -> map (Right) (transition auto v c)))


thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA auto1 auto2 = A ((states leftA1) ++ (states rightA2))
                      (initStates leftA1)
                      (\v -> isAccepting rightA2 v || ((any (isAccepting auto2) (initStates auto2)) && (isAccepting leftA1 v)))
                      (\v c -> (transition leftA1 v c) ++ (transition rightA2 v c) ++ (if (isAccepting leftA1 v) then (concat [(transition rightA2 w c) | w <- initStates rightA2]) else []))
    where
        leftA1 = leftA auto1
        rightA2 = rightA auto2


sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA auto1 auto2 = A ((states leftA1) ++ (states rightA2))
                     ((initStates leftA1) ++ (initStates rightA2))
                     (either (isAccepting auto1) (isAccepting auto2))
                     (either ((transition leftA1) . Left) ((transition rightA2) . Right))
    where
        leftA1 = leftA auto1
        rightA2 = rightA auto2


fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
fromLists s iS iA t = A s iS (\v -> elem v iA) (\v c -> toNeighList (find (\(u, d, _) -> (u == v) && (c == d)) t))
    where
        toNeighList :: Maybe (a, b, [a]) -> [a]
        toNeighList (Just (_, _, t)) = t
        toNeighList Nothing = []


toLists :: (Enum a, Bounded a) => Auto a q -> ([q], [q], [q], [(q, a, [q])])
toLists auto = ((states auto),
                (initStates auto),
                (filter (isAccepting auto) (states auto)),
                (filter (\(v, c, neigh) -> not (null neigh)) [(v, c, (transition auto v c)) | v <- (states auto), c <- (enumFrom minBound)]))