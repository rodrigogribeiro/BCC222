module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Aula13


tests :: TestTree
tests = testGroup "Testes para Aula 13"
                 [
                 testGroup "Testes para reverse"
                             [
                               testProperty "Reverse involutivo" $
                                  prop_revInv
                             , testProperty "Reverse ++" $
                                  prop_revOk
                             ]

                 , testGroup "Testes para qsort"
                   [
                     testProperty "Gerador de listas ordenadas" $
                         prop_genSortListSorted
                   , testProperty "qsort == sort" $
                         prop_qsort_sort2
                   , testProperty "qsort orderna" $
                         prop_qsortSorted
                   ]
                 ]


main :: IO ()
main = defaultMain tests
