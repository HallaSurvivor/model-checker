module Dfa where

import Prelude hiding (init)

data DFA s a = DFA { transition :: a -> s -> s
                   , init       :: s
                   , final      :: s -> Bool 
                   }

runDFA :: DFA s a -> [a] -> Bool
runDFA d = final d . transition' 
  where
    transition' = foldr (transition d) (init d) 
