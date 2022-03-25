-- Pretty-printing combinators, testing against a model implementation.
-- Illustrates running QuickSpec on a progressively larger set of signatures.
-- See the QuickSpec paper for more details.
{-# LANGUAGE DeriveDataTypeable #-}
import           Control.Monad
import           Data.Proxy
import           Prelude         hiding ((<>))
import           QuickSpec
import           Test.QuickCheck

newtype Layout = Layout [(Int, String)]
  deriving (Typeable, Eq, Ord, Show)

instance Arbitrary Layout where
  arbitrary = fmap Layout (liftM2 (:) arbitrary arbitrary)

text :: String -> Layout
text s = Layout [(0, s)]

nest :: Int -> Layout -> Layout
nest k (Layout l) = Layout [(i+k, s) | (i, s) <- l]

($$) :: Layout -> Layout -> Layout
Layout xs $$ Layout ys = Layout (xs ++ ys)

(<>) :: Layout -> Layout -> Layout
Layout xs <> Layout ys =
  combine (init xs) (last xs) (head ys) (tail ys)
  where
    combine xs (i, s) (j, t) ys =
      Layout xs $$
      Layout [(i, s ++ t)] $$
      nest (i + length s - j) (Layout ys)

nesting :: Layout -> Int
nesting (Layout ((i,_):_)) = i

main = quickSpec [
  withMaxTermSize 9,
  monoType (Proxy :: Proxy Layout),
  background [
    con "\"\"" "",
    con "++" ((++) :: String -> String -> String),
    con "0" (0 :: Int),
    con "+" ((+) :: Int -> Int -> Int),
    con "length" (length :: String -> Int) ],
  series [sig1, sig2]]
  where
    sig1 = [
      con "text" text,
      con "nest" nest,
      con "$$" ($$),
      con "<>" (<>) ]
    sig2 = [con "nesting" nesting]
