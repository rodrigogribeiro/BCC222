module QSort where

import Data.List
import Test.QuickCheck

-- sorted property

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [ _ ] = True
isSorted (x : x' : xs)
    = x <= x' && isSorted (x' : xs)

-- quicksort implementation

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs)
    = qsort lts ++ [x] ++ qsort gts
      where
        lts = [y | y <- xs, y <= x]
        gts = [y | y <- xs, y > x]

-- properties

prop_qsortSorted :: [Int] -> Bool
prop_qsortSorted xs = isSorted (qsort xs)


prop_genSortListSorted :: Property
prop_genSortListSorted
    = forAll genList3 isSorted

prop_qsort_sort2 :: Property
prop_qsort_sort2
    = forAll (genList3 :: Gen [Int])
             (\ xs -> qsort xs == sort xs)

-- sorted list generator

genList2 :: Arbitrary a => Gen [a]
genList2 = sized gen
   where
     gen n = frequency [ (1, return [])
                       , (n, (:) <$> arbitrary <*>
                                     gen (n `div` 2))]

genList3 :: (Arbitrary a, Ord a) => Gen [a]
genList3 = sort <$> genList2
