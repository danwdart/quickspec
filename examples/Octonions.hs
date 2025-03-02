-- The octonions, made using the Cayley-Dickson construction.
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Control.Monad
import           Data.Proxy
import           Data.Ratio
import           QuickSpec
import           Test.QuickCheck
import           Twee.Pretty

newtype SmallRational = SmallRational Rational
  deriving (Eq, Ord, Num, Typeable, Fractional, Conj, CoArbitrary, Show)
instance Arbitrary SmallRational where
  arbitrary = SmallRational <$> liftM2 (%) arbitrary (arbitrary `suchThat` (/= 0))

-- A class for types with conjugation, a norm operator and a generator.
class Fractional a => Conj a where
  conj :: a -> a
  norm :: a -> Rational
  it :: Gen a

instance Conj Rational where
  conj x = x
  norm x = x*x
  -- Only generate small rationals for efficiency.
  it = liftM2 (Prelude./) (elements [-10..10]) (elements [1..10])

instance Conj a => Conj (a, a) where
  conj (x, y) = (conj x, negate y)
  norm (x, y) = norm x + norm y
  it = liftM2 (,) it it

instance Conj a => Num (a, a) where
  fromInteger n = (fromInteger n, 0)
  (x, y) + (z, w) = (x + z, y + w)
  (a, b) * (c, d) = (a * c - conj d * b, d * a + b * conj c)
  negate (x, y) = (negate x, negate y)

instance Conj a => Fractional (a, a) where
  fromRational x = (fromRational x, 0)
  recip x = conj x * fromRational (recip (norm x))

newtype Complex = Complex (SmallRational, SmallRational) deriving (Eq, Ord, Num, Typeable, Fractional, Conj, Arbitrary, CoArbitrary, Show)
newtype Quaternion = Quaternion (Complex, Complex) deriving (Eq, Ord, Num, Typeable, Fractional, Conj, Arbitrary, CoArbitrary, Show)
newtype Octonion = Octonion (Quaternion, Quaternion) deriving (Eq, Ord, Num, Typeable, Fractional, Conj, Arbitrary, CoArbitrary, Show)

newtype It = It Octonion deriving (Eq, Ord, Num, Typeable, Fractional, Conj, CoArbitrary, Show)

instance Arbitrary It where
  -- Division is undefined on zero octonions.
  arbitrary = It <$> arbitrary `suchThat` (/= 0)

main = quickSpec [
  -- Make the pruner more powerful, which is helpful when Doing Maths
  withPruningTermSize 9,
  -- One test suffices :)
  withMaxTests 1,
  con "*" ((*) :: It -> It -> It),
  con "inv" (recip :: It -> It),
  con "1" (1 :: It),
  monoType (Proxy :: Proxy It)]
