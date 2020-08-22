module Tree where


data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show, Ord)


insert :: Ord a => a -> Tree a -> Tree a
insert v Leaf = Node v Leaf Leaf
insert v (Node v' l r)
  | v == v'   = Node v' l r
  | v < v'    = Node v' (insert v l) r
  | otherwise = Node v' l (insert v' r)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node v l r)
  = Node (f v) (mapTree f l)
               (mapTree f r)
