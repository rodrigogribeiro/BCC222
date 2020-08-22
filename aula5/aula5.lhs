Estudo de caso: Árvores binárias de busca
=========================================

* Implementar operações de criar uma árvore
vazia, inserção, busca, remoção e
conversão de/para listas.

* Implementação de funções de ordem superior:
map, foldr e filter.

Definição de árvores binárias.
------------------------------

> data Tree a
>   = Leaf
>   | Node a (Tree a) (Tree a)
>   deriving (Eq, Ord, Show)

Interface de manipulação
------------------------

1. Criar uma árvore vazia

> empty :: Tree a
> empty = Leaf

2. Inserir um valor

> insert :: (a -> a -> Bool) -> -- comparação <
>           (a -> a -> Bool) -> -- comparação ==
>           a                -> -- valor a ser inserido
>           Tree a           -> -- onde inserir
>           Tree a
> insert lt eq v Leaf = Node v empty empty
> insert lt eq v (Node v' l r)
>     | eq v v'   = Node v' l r
>     | lt v v'   = Node v' (insert lt eq v l) r
>     | otherwise = Node v' l (insert lt eq v r)

> testInsert :: Tree Int
> testInsert = insert (<=) (==) 4
>                 (insert (<=) (==) 1
>                     (insert (<=) (==) 2 empty))




3. Busca de elementos.

> search :: (a -> a -> Bool) -> -- comparação <
>           (a -> a -> Bool) -> -- igualdade
>           a                -> -- valor a ser procurado
>           Tree a           ->
>           Bool
> search _ _ v Leaf          = False
> search lt eq v (Node v' l r)
>    | eq v v' = True
>    | lt v v' = search lt eq v l
>    | otherwise = search lt eq v r

4. Remoção

> remove :: (a -> a -> Bool) -> -- comparação <
>           (a -> a -> Bool) -> -- comparação ==
>           a                -> -- valor a ser removido
>           Tree a           ->
>           Tree a
> remove _  _  _ Leaf = Leaf
> remove lt eq v (Node v' l r)
>   | eq v v'   = removeEq l r
>   | lt v v'   = Node v' (remove lt eq v l) r
>   | otherwise = Node v' l (remove lt eq v r)

> removeEq :: Tree a -> Tree a -> Tree a
> removeEq Leaf r = r
> removeEq l Leaf = l
> removeEq l r
>   = case removeMin l of   -- data Maybe a = Nothing | Just a
>       Nothing     -> error "Impossible!"
>       Just (x,r') -> Node x l r'

> removeMin :: Tree a -> Maybe (a, Tree a)
> removeMin Leaf = Nothing
> removeMin (Node v Leaf r) = Just (v, r)
> removeMin (Node v l r)
>   = case removeMin l of
>       Nothing      -> Nothing
>       Just (v',l') -> Just (v' , Node v l' r)



5. Conversão de / para listas

> fromList :: (a -> a -> Bool) ->
>             (a -> a -> Bool) ->
>             [a]              ->
>             Tree a
> fromList lt eq
>    = foldr (insert lt eq) Leaf


> toList :: Tree a -> [a]
> toList Leaf         = []
> toList (Node v l r)
>    = v : (toList l ++ toList r)

> toList' :: Tree a -> [a]
> toList'
>   = flip toList1 []
>     where
>       toList1 Leaf ac         = ac
>       toList1 (Node v l r) ac
>          = v : toList1 l (toList1 r ac)



Funções de ordem superior
-------------------------

1. função foldr

> foldrTree :: (a -> b -> b -> b) ->
>              b                  ->
>              Tree a             ->
>              b
> foldrTree f v Leaf = v
> foldrTree f v (Node v' l r)
>   = f v' (foldrTree f v l)
>          (foldrTree f v r)

2. função map

> mapTree :: (a -> b) -> Tree a -> Tree b
> mapTree f
>    = foldrTree step Leaf
>      where
>        step v acl acr = Node (f v) acl acr


3. função filter

> filterTree :: (a -> a -> Bool) ->
>               (a -> a -> Bool) ->
>               (a -> Bool) -> Tree a -> Tree a
> filterTree lt eq p
>   = fromList lt eq . filter p . toList

Exercícios
----------

1. Reimplementar a função search usando foldrTree.
2. Implementar uma função para calcular a altura
de uma árvore usando foldrTree.
3. Implementar uma função para calcular o número
de elementos de uma árvore binária usando foldrTree.
