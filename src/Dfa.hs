module Dfa where

import Prelude hiding (init)

data Bit = Z | O

-- | arbirtary arity DFA over {0,1} with states coming from s
data DFA s = DFA { transition :: [Bit] -> s -> s
                 , init       :: s
                 , final      :: s -> Bool 
                 }

-- | Checks if a DFA accepts at least one word (requires the DFA be of arity 0)
nonEmpty :: (Eq s) => DFA s -> Bool
nonEmpty d = dfs (init d) (const False)
  where
    dfs q seen = not (seen q) && (final d q || dfs q' (\x -> x == q || seen x))
      where
        q' = transition d [] q

-- | DFA intersection (propositional AND)
intersect :: DFA s -> DFA t -> DFA (s,t)
intersect d1 d2 = DFA delta q0 f
  where
    q0    = (init d1, init d2)
    f     = \(q1,q2) -> final d1 q1 && final d2 q2
    delta = \bs (q1, q2) -> (transition d1 bs q1, transition d2 bs q2)

-- | DFA disjoint union (propositional OR)
union :: DFA s -> DFA t -> DFA (s,t)
union d1 d2 = DFA delta q0 f
  where
    q0    = (init d1, init d2)
    f     = \(q1,q2) -> final d1 q1 || final d2 q2
    delta = \bs (q1, q2) -> (transition d1 bs q1, transition d2 bs q2)

-- | DFA complement (propositional NOT)
complement :: DFA s -> DFA s
complement d = DFA delta q0 f
  where
    q0    = init d
    f     = not . final d
    delta = transition d

-- | DFA track elimination (propositional EXISTS)
eliminate :: Int -> DFA s -> DFA [s] 
eliminate n d = DFA delta q0 f
  where
    q0    = [init d]
    f     = not . null . filter (final d) 
    delta bs qs = [transition d bs0 q | q <- qs] ++ [transition d bs1 q | q <- qs]
      where
        bs0 = take n bs ++ Z:drop n bs
        bs1 = take n bs ++ O:drop n bs
