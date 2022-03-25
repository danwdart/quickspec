-- Sorting and sorted lists.
-- Illustrates testing of conditional laws.
import           Data.List
import           QuickSpec

sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

main = quickSpec [
  lists `without` ["++"],
  con "sort" (sort :: [Int] -> [Int]),
  con "insert" (insert :: Int -> [Int] -> [Int]),
  predicate "sorted" (sorted :: [Int] -> Bool) ]
