-- The monad laws for lists.
import           Control.Monad
import           QuickSpec

main = quickSpec [
  withMaxTestSize 20,
  con "return" (return :: A -> [A]),
  con ">>=" ((>>=) :: [A] -> (A -> [B]) -> [B]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con ">=>" ((>=>) :: (A -> [B]) -> (B -> [C]) -> A -> [C]) ]
