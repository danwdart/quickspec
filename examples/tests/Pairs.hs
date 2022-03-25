-- Just for testing polymorphic generalisation

import           Data.Monoid
import           QuickSpec

main =
  quickSpec [series [sig1, sig2]]
  where
    sig1 = [
      con "," ((,) :: A -> B -> (A, B)),
      con "fst" (fst :: (A, B) -> A),
      con "snd" (snd :: (A, B) -> B) ]
    sig2 = [
      con "pair" (True, 'a')]
