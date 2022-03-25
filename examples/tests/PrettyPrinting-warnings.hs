-- Test case for warning generation.
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
import           Control.Monad
import           Data.Constraint
import           Data.Proxy
import           Prelude                   hiding ((<>))
import           QuickSpec
import           Test.QuickCheck
import           Text.PrettyPrint.HughesPJ hiding (Str)

deriving instance Typeable Doc

instance Arbitrary Doc where
  arbitrary =
    sized $ \n ->
      let bin = resize (n `div` 2) arbitrary
          un = resize (n-1) arbitrary in
      oneof $
        [ liftM2 ($$) bin bin | n > 0 ] ++
        [ liftM2 (<>) bin bin | n > 0 ] ++
        [ liftM2 nest arbitrary un | n > 0 ] ++
        [ fmap text arbitrary ]

-- Observational equality.
instance Observe Context Str Doc where
  observe (Context ctx) d = Str (render (ctx d))
newtype Str = Str String deriving (Eq, Ord)

newtype Context = Context (Doc -> Doc)

instance Arbitrary Context where
  arbitrary = Context <$> ctx
    where
      ctx =
        sized $ \n ->
        oneof $
          [ return id ] ++
          [ liftM2 (\x y d -> op (x d) y) (resize (n `div` 2) ctx) (resize (n `div` 2) arbitrary) | n > 0, op <- [(<>), ($$)] ] ++
          [ liftM2 (\x y d -> op x (y d)) (resize (n `div` 2) arbitrary) (resize (n `div` 2) ctx) | n > 0, op <- [(<>), ($$)] ] ++
          [ liftM2 (\x y d -> nest x (y d)) arbitrary (resize (n-1) ctx) | n > 0 ]

unindented :: Doc -> Bool
unindented d = render (nest 100 (text "" <> d)) == render (nest 100 d)

nesting :: Doc -> Int
nesting d = head [ i | i <- nums, unindented (nest (-i) d) ]
  where
    nums = 0:concat [ [i, -i] | i <- [1..] ]

main = quickSpec [
  withMaxTermSize 9,

  background [
    con "[]" ([] :: [A]),
    con "++" ((++) :: [A] -> [A] -> [A]),
    con "0" (0 :: Int),
    con "+" ((+) :: Int -> Int -> Int),
    con "length" (length :: [A] -> Int) ],


  con "text" text,
  con "nest" nest,
  --con "nesting" nesting,
  con "<>" (<>),
  con "$$" ($$),

  defaultTo (Proxy :: Proxy Bool)]
