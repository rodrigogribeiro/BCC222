> module Main where

> import Test.Tasty
> import Test.Tasty.QuickCheck
> import Reverse
> import QSort
> import Tree
> import Exp

Funções para execução dos testes

> tests :: TestTree
> tests = testGroup "Testes para Aula 13"
>                 [
>                   testGroup "Testes para reverse"
>                             [
>                               testProperty "Reverse involutivo" $
>                                  prop_revInv
>                             , testProperty "Reverse ++" $
>                                  prop_revOk
>                             ]
>
>                 , testGroup "Testes para qsort"
>                             [
>                               testProperty "Gerador de listas ordenadas" $
>                               prop_genSortListSorted
>                             , testProperty "qsort == sort" $
>                               prop_qsort_sort2
>                             , testProperty "qsort orderna" $
>                               prop_qsortSorted
>                             ]
>                 , testGroup "Testes para árvores"
>                             [
>                               testProperty "Geração de árvores é correta" $
>                                 genTree2_ok
>                             , testProperty "Inserção preserva invariante" $
>                                 insert_BST
>                             ]
>                 , testProperty "Correção do compilador" $ (flip compileOk [])
>                 ]


> main :: IO ()
> main = defaultMain tests


