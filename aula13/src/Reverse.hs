module Reverse where

prop_revInv :: [Int] -> Bool
prop_revInv xs = reverse (reverse xs) == xs

prop_revOk :: [Int] -> [Int] -> Bool
prop_revOk xs ys
   = reverse (xs ++ ys) == reverse ys ++ reverse xs
