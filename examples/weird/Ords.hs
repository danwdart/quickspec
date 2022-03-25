{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Data.Ord
import           Prelude         hiding (exp)
import           QuickSpec
import           Test.QuickCheck
import           Twee.Pretty

newtype Nat = Nat Int deriving (Eq, Ord, Num, Enum, CoArbitrary)

instance Arbitrary Nat where
  arbitrary = fmap (Nat . abs) arbitrary

data Ordinal = Zero | Succ Ordinal | Lim (Nat -> Ordinal) deriving Typeable

instance Arbitrary Ordinal where
  arbitrary =
    oneof (fmap Lim arbitrary:map (return . toOrdinal) [0..3])
    where
      toOrdinal 0 = Zero
      toOrdinal n = Succ (toOrdinal (n-1))

instance CoArbitrary Ordinal where
  coarbitrary Zero     = variant 0
  coarbitrary (Succ o) = variant 1 . coarbitrary o
  coarbitrary (Lim h)  = variant 2 . coarbitrary h

instance Eq Ordinal where
  x == y = compare x y == EQ

instance Ord Ordinal where
  compare = comparing toNat
    where
      toNat Zero     = 0
      toNat (Succ x) = succ (toNat x)
      toNat (Lim f)  = maximum (map (toNat' . f) [0..10])
      toNat' Zero     = 0
      toNat' (Succ x) = succ (toNat' x)
      toNat' (Lim f)  = 10000
{-  compare Zero Zero         = EQ
  compare Zero _            = LT
  compare (Succ x) Zero     = GT
  compare (Succ x) (Succ y) = compare x y
  compare (Succ x) _        = LT
  compare (Lim _) Zero      = GT
  compare (Lim _) (Succ _)  = GT
  compare (Lim f) (Lim g)   = compare (f 3) (g 3)-}

plus :: Ordinal -> Ordinal -> Ordinal
plus m Zero     = m
plus m (Succ n) = Succ (plus m n)
plus m (Lim f)  = Lim (plus m . f)

times :: Ordinal -> Ordinal -> Ordinal
times m Zero     = Zero
times m (Succ n) = plus (times m n) m
times m (Lim f)  = Lim (times m . f)

exp :: Ordinal -> Ordinal -> Ordinal
exp m Zero     = Succ Zero
exp m (Succ n) = times (exp m n) m
exp m (Lim f)  = Lim (exp m . f)

sig =
  signature {
    constants = [
       constant "0" Zero,
       (constant "s" Succ) { conStyle = uncurried },
       constant "+" plus,
       constant "*" times ],
    instances = [
      baseType (undefined :: Ordinal) ]}

main = quickSpec sig
