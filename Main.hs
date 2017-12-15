-- A few tests for the model checker

import Structure
import Dfa
import Prelude hiding (succ)

data SuccStates = Sink | Carry | NoCarry deriving (Eq, Show)

succ :: DFA
succ = mkDFA Carry delta (\s -> s /= Sink)
  where
    delta [Z,O] Carry   = NoCarry
    delta [O,Z] Carry   = Carry
    delta [O,O] NoCarry = NoCarry
    delta [Z,Z] NoCarry = NoCarry
    delta _     _       = Sink

-- we use Bool as our state set
equal :: DFA
equal = mkDFA True delta id
  where
    delta [Z,Z] True = True
    delta [O,O] True = True
    delta _       _  = False

testStructure :: Structure
testStructure 'e' ns = extend equal ns
testStructure 's' ns = extend succ  ns

-- forall x . forall y . (x+1 = y+1) implies (x = y)
--
-- but remember we can only work with relations...
-- so this becomes the (uglier)
--
-- forall sx . forall sy . forall x . forall y .
-- succ (x, sx) and succ (y, sy) and equal (sx, sy) implies equal (x,y)
--
-- we set tape 0 = x, tape 1 = y, tape 2 = sx, tape 3 = sy
testProp1 :: Predicate
testProp1 = 
  ForAll -- sy
  (
    ForAll -- sx
    (
      ForAll -- y
      (
        ForAll -- x
        (
          (Atom [0,2] 's' &&& Atom [1,3] 's' &&& Atom [2,3] 'e') ==> Atom [0,1] 'e'
        )
      )
    )
  )

-- forall x . exists y . x = y
testProp2 :: Predicate
testProp2 = ForAll $ Exists $ Atom [0,1] 'e'

-- exists x . forall y . x = y
testProp3 :: Predicate
testProp3 = Exists $ ForAll $ Atom [0,1] 'e'

main :: IO ()
main = 
  putStrLn "solving..." 
  >> print (eval testStructure testProp1) 
  >> print (eval testStructure testProp2) 
  >> print (eval testStructure testProp3) 
