module Dfa where

import Prelude hiding (init)

-- | DFA over {0,1} with states coming from s
data DFA s a = DFA { transition :: a -> s -> s
                   , init       :: s
                   , final      :: s -> Bool 
                   }

runDFA :: DFA s a -> [a] -> Bool
runDFA d = isFinal . transition' 
  where
    isFinal     = final d
    transition' = foldr (transition d) (init d) 

-- | DFA intersection (propositional AND)
intersect :: DFA s a -> DFA t a -> DFA (s,t) a
intersect d1 d2 = DFA delta q0 f
  where
    q0    = (init d1, init d2)
    f     = \(q1,q2) -> final d1 q1 && final d2 q2
    delta = \a (q1, q2) -> (transition d1 a q1, transition d2 a q2)

-- | DFA disjoint union (propositional OR)
union :: DFA s a -> DFA t a -> DFA (s,t) a
union d1 d2 = DFA delta q0 f
  where
    q0    = (init d1, init d2)
    f     = \(q1,q2) -> final d1 q1 || final d2 q2
    delta = \a (q1, q2) -> (transition d1 a q1, transition d2 a q2)

-- | DFA complement (propositional NOT)
complement :: DFA s a -> DFA s a
complement d1 = DFA delta q0 f
  where
    q0    = init d1
    f     = not . final d1
    delta = transition d1

-- | DFA track elimination (propositional EXISTS)
eliminate :: DFA s a -> Int -> DFA s a
eliminate d n = undefined

