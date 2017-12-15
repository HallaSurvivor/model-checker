module Structure where

import qualified Dfa

type Structure = Char -> Dfa.DFA

data Predicate = Atom Char
               | And Predicate Predicate
               | Or  Predicate Predicate
               | Not Predicate
               | Exists Predicate
               | ForAll Predicate

-- Requires the input have all quantififers out front, 
-- with the innermost quantifier corresponding to the topmost tape,
-- second quantifier corresponding to the second tape, ...
-- outermost quantifier corresponding to the bottommost tape
eval :: Structure -> Predicate -> Bool
eval getRel p = Dfa.nonEmpty $ eval' getRel p
  where
    eval' getRel (Atom c)    = getRel c
    eval' getRel (And p1 p2) = Dfa.intersect  (eval' getRel p1) (eval' getRel p2)
    eval' getRel (Or  p1 p2) = Dfa.union      (eval' getRel p1) (eval' getRel p2)
    eval' getRel (Not p)     = Dfa.complement (eval' getRel p)
    eval' getRel (Exists p)  = Dfa.eliminate  (eval' getRel p)
    eval' getRel (ForAll p)  = Dfa.complement $ Dfa.eliminate $ Dfa.complement (eval' getRel p)
