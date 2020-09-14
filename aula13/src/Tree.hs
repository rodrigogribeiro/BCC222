module Tree where

import QSort (genList3)
import Test.QuickCheck

data Tree a
   = Leaf
   | Node a (Tree a) (Tree a)
   deriving Show

   -- deriving (Eq, Ord, Show)


instance Foldable Tree where
   foldr _ v Leaf = v
   foldr f v (Node x l r)
     = foldr f (f x (foldr f v r)) l

-- propriedade de bst

bst :: Ord a => Tree a -> Bool
bst Leaf = True
bst (Node x l r) = (all (<= x) l) && (all (>= x) r)

-- inserção

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf
   = Node x Leaf Leaf
insert x (Node y l r)
     | x == y = Node y l r
     | x < y  = Node y (insert x l) r
     | x > y  = Node y l (insert x r)

-- geração de árvores

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

genTree2 :: Gen (Tree Int)
genTree2 = fromList <$> genList3

genTree2_ok :: Property
genTree2_ok
    = forAll genTree2 bst

-- inserção preserva invariante

insert_BST :: Property
insert_BST
   = forAll genTree2
            (\ t -> forAll (arbitrary :: Gen Int)
                    (\ x -> bst t ==> bst (insert x t)))

