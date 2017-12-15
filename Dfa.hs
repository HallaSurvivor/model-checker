{-# LANGUAGE ExistentialQuantification #-}
module Dfa 
  (Bit(..)
  ,DFA
  ,mkDFA
  ,nonEmpty
  ,intersect
  ,union
  ,complement
  ,eliminate
  ,extend
  ,runDFA
  ) where

import Prelude hiding (init)
import Data.List (transpose)

data Bit = Z | O

-- | arbirtary arity DFA over {0,1} with states coming from s (internal representation)
data DFAint s = DFAint { transition :: [Bit] -> s -> s
                       , init       :: s
                       , final      :: s -> Bool 
                       }

-- | Checks if a DFA accepts at least one word (requires the DFA be of arity 0)
nonEmpty' :: (Eq s) => DFAint s -> Bool
nonEmpty' d = dfs (init d) (const False)
  where
    dfs q seen = not (seen q) && (final d q || dfs q' (\x -> x == q || seen x))
      where
        q' = transition d [] q

-- | DFA intersection (propositional AND)
intersect' :: DFAint s -> DFAint t -> DFAint (s,t)
intersect' d1 d2 = DFAint delta q0 f
  where
    q0    = (init d1, init d2)
    f     = \(q1,q2) -> final d1 q1 && final d2 q2
    delta = \bs (q1, q2) -> (transition d1 bs q1, transition d2 bs q2)

-- | DFA disjoint union (propositional OR)
union' :: DFAint s -> DFAint t -> DFAint (s,t)
union' d1 d2 = DFAint delta q0 f
  where
    q0    = (init d1, init d2)
    f     = \(q1,q2) -> final d1 q1 || final d2 q2
    delta = \bs (q1, q2) -> (transition d1 bs q1, transition d2 bs q2)

-- | DFA complement (propositional NOT)
complement' :: DFAint s -> DFAint s
complement' d = DFAint delta q0 f
  where
    q0    = init d
    f     = not . final d
    delta = transition d

-- | DFA track elimination (propositional EXISTS)
eliminate' :: DFAint s -> DFAint [s] 
eliminate' d = DFAint delta q0 f
  where
    q0    = [init d]
    f     = not . null . filter (final d) 
    delta bs qs = [transition d bs0 q | q <- qs] ++ [transition d bs1 q | q <- qs]
      where
        bs0 = Z:bs
        bs1 = O:bs


-- Exposed!

data DFA = forall s . Eq s => DFA (DFAint s)

mkDFA :: (Eq s) => s -> ([Bit] -> s -> s) -> (s -> Bool) -> DFA
mkDFA q0 delta f = DFA $ DFAint delta q0 f

nonEmpty :: DFA -> Bool
nonEmpty (DFA d) = nonEmpty' d

intersect :: DFA -> DFA -> DFA
intersect (DFA d1) (DFA d2) = DFA $ intersect' d1 d2

union :: DFA -> DFA -> DFA
union (DFA d1) (DFA d2) = DFA $ union' d1 d2

complement :: DFA -> DFA
complement (DFA d) = DFA $ complement' d

eliminate :: DFA -> DFA
eliminate (DFA d) = DFA $ eliminate' d

-- Extend a DFA to act on a larger tape by ignoring unspecified inputs
extend :: DFA -> [Int] -> DFA
extend (DFA d) ns = DFA $ DFAint delta q0 f
  where
    q0 = init d
    f  = final d
    delta bs q = transition d (map (bs !!) ns) q

-- Mainly for testing
runDFA :: DFA -> [[Bit]] -> Bool
runDFA (DFA d) = isFinal . transition'
  where
    isFinal = final d
    transition' = foldl (flip (transition d)) (init d) . transpose
