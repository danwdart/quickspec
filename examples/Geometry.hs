-- Henderson's functional geometry. See the QuickSpec paper.
--
-- Illustrates:
--   * Observational equality
--   * Running QuickSpec on a progressively larger set of signatures
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
import           Control.Monad
import           Data.Constraint
import           Data.Monoid
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Word
import           Prelude         hiding (cycle, flip)
import           QuickSpec
import           Test.QuickCheck

-- We use our own number type for efficiency purposes.
-- This can represent numbers of the form x/2^e where x and e are integers.
data Rat = Rat { mantissa :: Integer, exponent :: Int } deriving (Eq, Ord, Show, Typeable)
-- Rat x e = x / 2^e

rat :: Integer -> Int -> Rat
rat x e | e < 0 = error "rat: negative exponent"
rat x 0 = Rat x 0
rat x e | even x = rat (x `div` 2) (e-1)
rat x e = Rat x e

instance Arbitrary Rat where
  arbitrary = liftM2 rat arbitrary (choose (0, 10))
  shrink (Rat x e) = fmap (uncurry rat) (shrink (x, e))

instance CoArbitrary Rat where
  coarbitrary (Rat x e) = coarbitrary x . coarbitrary e

-- A class for types (like Rat) which can be added, subtracted and
-- divided by 2.
class Half a where
  zero :: a
  neg :: a -> a
  plus :: a -> a -> a
  half :: a -> a

instance Half Rat where
  zero = rat 0 0
  neg (Rat x e) = Rat (negate x) e
  plus (Rat x1 e1) (Rat x2 e2) =
    rat (x1 * 2^(e - e1) + x2 * 2^(e - e2)) e
    where
      e = e1 `max` e2
  half (Rat x e) = Rat x (e+1)

instance (Half a, Half b) => Half (a, b) where
  zero = (zero, zero)
  neg (x, y) = (neg x, neg y)
  plus (x, y) (z, w) = (plus x z, plus y w)
  half (x, y) = (half x, half y)

-- A vector is a pair of points.
type Vector = (Rat, Rat)

-- We represent a geometrical object as a triple of vectors.
-- I forget what they mean :)
-- I think two of them represent the direction of the x-axis and y-axis.
-- The word represents an abstract "drawing command".
type Object = (Vector, Vector, Vector, Word)

-- A drawing takes size and rotation information and returns a set of objects.
newtype Drawing = Drawing (Vector -> Vector -> Vector -> Objs) deriving Typeable
newtype Objs = Objs { unObjs :: Set Object } deriving (Eq, Ord, Typeable, Show)
instance Arbitrary Objs where arbitrary = fmap objs arbitrary

objs :: Set Object -> Objs
objs = Objs . Set.filter (\(_,b,c,_) -> b /= zero && c /= zero)

instance Show Drawing where
  show (Drawing x) = show (x one one one)
    where
      one = (Rat 1 0, Rat 1 0)

instance Arbitrary Drawing where
  arbitrary = do
    os <- arbitrary
    return . Drawing $ \x y z -> objs (Set.fromList [(x, y, z, o) | o <- os])
  shrink (Drawing f) =
    [ Drawing $ \x y z -> objs (Set.fromList [(x, y, z, o) | o <- objs'])
    | let os = [ o | (_, _, _, o) <- Set.toList (unObjs (f one one one)) ],
      objs' <- shrink os ]
    where
      one = (Rat 1 0, Rat 1 0)

blank :: Drawing
blank = Drawing (\_ _ _ -> objs Set.empty)

-- The primed versions of the combinators are buggy
over, beside, above, above' :: Drawing -> Drawing -> Drawing
over (Drawing p) (Drawing q) = Drawing (\a b c -> p a b c `union` q a b c)
beside (Drawing p) (Drawing q) = Drawing (\a b c -> p a (half b) c `union` q (a `plus` half b) (half b) c)
above' (Drawing p) (Drawing q) = Drawing (\a b c -> p a b (half c) `union` q (a `plus` half c) b (half c))
above (Drawing p) (Drawing q) = Drawing (\a b c -> p (a `plus` half c) b (half c) `union` q a b (half c))

union :: Objs -> Objs -> Objs
union (Objs x) (Objs y) = objs (x `Set.union` y)

rot, flip, rot45 :: Drawing -> Drawing
rot (Drawing p) = Drawing (\a b c -> p (a `plus` b) c (neg b))
flip (Drawing p) = Drawing (\a b c -> p (a `plus` b) (neg b) c)
rot45 (Drawing p) = Drawing (\a b c -> p (a `plus` half (b `plus` c)) (half (b `plus` c)) (half (c `plus` neg b)))

quartet, quartet' :: Drawing -> Drawing -> Drawing -> Drawing -> Drawing
quartet a b c d = (a `beside` b) `above` (c `beside` d)
quartet' a b c d = (a `beside` b) `above'` (c `beside` d)

cycle, cycle' :: Drawing -> Drawing
cycle x = quartet x (rot (rot (rot x))) (rot x) (rot (rot x))
cycle' x = quartet' x (rot (rot (rot x))) (rot x) (rot (rot x))

-- Observational equality for drawings.
instance Observe (Vector, Vector, Vector) Objs Drawing where
  observe (a, b, c) (Drawing d) = d a b c

main =
  quickSpec [
    inst (Sub Dict :: () :- Arbitrary Drawing),
    inst (Sub Dict :: () :- Observe (Vector, Vector, Vector) Objs Drawing),
    series [sig1, sig2, sig3, sig4, sig5, sig6, sig7] ]
  where
    -- A series of bigger and bigger signatures.
    sig1 = [con "over" over]
    sig2 = [
      con "beside" beside,
      -- con "above" above',
      con "above" above]
    sig3 = [con "rot" rot]
    sig4 = [con "flip" flip]
    sig5 = [
      con "cycle" cycle,
      -- con "cycle" cycle',
      con "quartet" quartet]
    sig6 = [con "rot45" rot45]
    sig7 = [con "blank" blank]
