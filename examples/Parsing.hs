-- Parser combinators.
-- Illustrates observational equality with polymorphic types.
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
import           Control.Monad
import           Data.Constraint
import           Data.List
import           QuickSpec
import           Test.QuickCheck
import           Text.ParserCombinators.ReadP

deriving instance Typeable ReadP

-- Generate random parsers.
instance Arbitrary a => Arbitrary (ReadP a) where
  arbitrary = fmap readS_to_P arbReadS

arbReadS :: Arbitrary a => Gen (String -> [(a, String)])
arbReadS = fmap convert (liftM2 (,) (elements [0..5]) arbitrary)
  where
    convert (n, parse) xs = take n [(x, drop n xs) | (x, n) <- parse xs]

-- Observational equality for parsers.
instance Ord a => Observe String [(a, String)] (ReadP a) where
  observe input parser = sort (readP_to_S parser input)

peek :: ReadP Char
peek = do
  (x:_) <- look
  return x

main = quickSpec [
  inst (Sub Dict :: Arbitrary A :- Arbitrary (ReadP A)),
  inst (Sub Dict :: Ord A :- Observe String [(A, String)] (ReadP A)),

  background [
    con "return" (return :: A -> ReadP A),
    con "()" (),
    con "void" (void :: ReadP A -> ReadP ()),
    con ">>=" ((>>=) :: ReadP A -> (A -> ReadP B) -> ReadP B),
    con ">=>" ((>=>) :: (A -> ReadP B) -> (B -> ReadP C) -> A -> ReadP C) ],

  con "get" get,
  con "peek" peek,
  con "+++" ((+++) :: ReadP A -> ReadP A -> ReadP A),
  con "<++" ((<++) :: ReadP A -> ReadP A -> ReadP A),
  con "pfail" (pfail :: ReadP A),
  con "eof" eof ]

