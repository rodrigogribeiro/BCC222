Estudo de caso:  Cache de produtos
==================================

> module MinTree where

Introdução
----------

Um problema comum a aplicações é ordenar elementos
de acordo com algum critério. Em lojas web é importante
que a ordenação seja feita de maneira eficiente pois,
muitas vezes o cliente está interessado em listar produtos
em ordem crescente de preços.

Nesse estudo de caso, mostraremos como árvores binárias e
classes de tipo podem fornecer uma solução elegante para
o problema de determinar o produto com o menor preço.

Definindo produtos
------------------

Os produtos de nossa cervejaria serão definidos pelo
seguintes registros:

> newtype Name
>    = Name {unName :: String}
>      deriving (Eq, Ord, Show)

> newtype BeerType
>    = BeerType {unBeer :: String}
>      deriving (Eq, Ord, Show)

> data Product
>   = Product {
>       name  :: Name
>     , kind  :: BeerType
>     , price :: Double
>     } deriving (Eq, Ord, Show) -- cláusula de deriving

O significado de cada um dos tipos anteriores e
de seus campos é imediato.

Problema: Definição automática de comparação
---------------------------------------------

A definição automática de classes é um recurso
conveniente do compilador GHC. Porém, em algumas
situações, o comportamento fornecido por essas
implementações não é o mais adequado. Considere
o código a seguir que corresponde a implementação
gerada pelo GHC para a função <= da classe Ord
para o tipo Product

instance Ord Product where
  (Product n1 k1 p1) <= (Product n2 k2 p2)
    = n1 < n2 || ((n1 == n2) &&
     (k1 < k2 || ((k1 == k2) && p1 < p2)))

que essencialmente representa a ordenação seguindo
a ordem de nomes, depois de tipos e finalmente preços.
Dessa forma, a implementação fornecida pelo GHC não
está de acordo com o critério exigido pela aplicação:
ordenar por preço. Seria tentador adicionar a seguinte
definição de instância:

instance Ord Product where
  (Product n1 k1 p1) <= (Product n2 k2 p2)
    = p1 <= p2

Porém, o GHC retorna a seguinte mensagem de erro:

Duplicate instance declarations

Como permitir que essas duas implementações de
ordenação possam coexistir no código? A solução para
esse problema consiste em introduzir um novo tipo
e implementar a ordenação desejada para este tipo.

> newtype ByPrice
>   = ByPrice Product
>     deriving (Eq, Show)

E a ordenação por preço pode ser realizada como
se segue

> instance Ord ByPrice where
>   (ByPrice p1) <= (ByPrice p2)
>      = price p1 <= price p2

Árvores binárias e menor elemento
---------------------------------

Em vídeos anteriores, apresentamos uma implementação
polimórfica de árvores binárias. O uso de classes de
tipos pode simplificar consideravelmente a
implementação:

> data BTree a
>  = BLeaf
>  | BNode a (BTree a) (BTree a)

> binsert :: Ord a => a -> BTree a -> BTree a
> binsert v BLeaf = BNode v BLeaf BLeaf
> binsert v (BNode v' l r)
>   = case compare v v' of
>       LT -> BNode v' (binsert v l) r
>       EQ -> BNode v' l r
>       GT -> BNode v' l (binsert v r)

> bsearch :: Ord a => a -> BTree a -> Bool
> bsearch _ BLeaf = False
> bsearch v (BNode v' l r)
>   = case compare v v' of
>       LT -> bsearch v l
>       EQ -> True
>       GT -> bsearch v r

Apesar de correta, a implementação acima possui
um inconveniente: para encontrar o menor elemento
em uma árvore devemos percorre-la para encontrar
esse elemento.

Para solucionar esse problema, vamos criar uma
árvore com um "cache" que armazenará o menor preço.

> data CTree v c
>    = CLeaf
>    | CNode v c (CTree v c) (CTree v c)
>    deriving (Eq, Show)

Note que para produzir esse cache de menor preço,
devemos re-implementar a função de inserção.

> cinsert :: ( Ord v
>            , Ord c) => v         ->
>                        c         ->
>                        CTree v c ->
>                        CTree v c
> cinsert v c CLeaf = CNode v c CLeaf CLeaf
> cinsert v c (CNode v' c' l r)
>    = case compare v v' of
>        EQ -> CNode v' c' l r
>        LT -> CNode v' (min c c') (cinsert v c l) r
>        GT -> CNode v' (min c c') l (cinsert v c r)

Observe que é necessária a restrição Ord c para
usarmos a função min sobre os valores do cache c.
A implementação que cada nó armazene também o menor
valor presente na sub-árvore que ele representa.

Agora, suponha que seja necessário calcular
o valor médio de preço de produtos para analisar
qual o percentual de clientes estão comprando
produtos acima / abaixo da média de preços.
Para isso, teremos que implementar outra
função de inserção e outra árvore para esse fim.

Ao invés de implementarmos a mesma funcionalidade,
vamos tentar extrair um padrão.

1. Em ambos os casos (menor e média de preços) o
que desejamos é manter um cache que resume as
informações armazenadas em sub-árvores de um
certo nó. Se a informação é de tipo c, o que
precisamos é uma função que combine os resultados
de subárvores, isto é, uma função de tipo c -> c -> c.

2. Note que, idealmente, a ordem de inserção de
elementos não deve ser importante. Para isso,
precisamos que a função de tipo c -> c -> c
deve ser associativa.

3. Como folhas não possuem valores, deve haver
um valor de tipo c que deve agir como um elemento
neutro da função de tipo c -> c -> c.

Dados os requisitos acima, o que precisamos é
que o tipo c possua a estrutura de um monóide.
Em álgebra, um monóide é uma estrutura formada
por uma função binária e um elemento neutro.

Na biblioteca padrão de Haskell, existe uma classe
de tipos para a estrutura de monóide.

class Semigroup a where
  (<>) :: a -> a -> a

x <> (y <> z) = (x <> y) <> z, <> é associativo.

class Semigroup a => Monoid a where
  mempty  :: a -- elemento neutro.
  mappend :: a -> a -> a
  -- implementação padrão.
  mappend = (<>)

x <> mempty = x
mempty <> x = x


A idéia é que mempty represente um elemento neutro
para a operação mappend (<>) sobre o tipo a.
Usando a classe Monoid, podemos implementar a função
insert de forma a produzir o cache desejado de
forma genérica.

Primeiramente, vamos implementar uma função para
retornar o valor da cache de uma árvore.

> cached :: Monoid c => CTree v c -> c
> cached (CNode _ c _ _) = c
> cached CLeaf           = mempty

Agora, a idéia é que durante a inserção, atualizados
a cache a cada novo nó visitado da árvore.

> insert :: (Ord v
>           , Monoid c) => v         ->
>                          c         ->
>                          CTree v c ->
>                          CTree v c
> insert v c CLeaf = CNode v c CLeaf CLeaf
> insert v c (CNode v' c' l r)
>    = case compare v v' of
>        EQ -> CNode v' c' l r
>        -- aqui atualizamos a cache do nó
>        -- atual por combinar os valores
>        -- das sub-árvores com o valor atual.
>        LT -> let
>                l' = insert v c l
>                c1 = c' <> cached l' <> cached r
>              in CNode v' c1 l' r
>        GT -> let
>                r' = insert v c r
>                c1 = c' <> cached l <> cached r'
>              in CNode v' c1 l r'

Agora, podemos construir a estrutura de menor preço
usando a seguinte estrutura de monóide.

> instance Semigroup ByPrice where
>   (<>) = min

> instance Monoid ByPrice where
>   mempty = ByPrice p
>       where
>         p = Product (Name "")
>                     (BeerType "")
>                     infinite
>         infinite = 1 / 0

Note que na instância de monóide usamos um produto
que possui um preço "infinito" que pode ser obtido
pela divisão de 1 por zero.
