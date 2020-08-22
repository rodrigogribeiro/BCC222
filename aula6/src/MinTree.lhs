Estudo de caso: Ordenando produtos
==================================

Introdução
----------

Um problema comum a aplicações é ordenar elementos
de acordo com algum critério. Em lojas web é importante
que a ordenação seja feita de maneira eficiente pois,
muitas vezes o cliente está interessado em listar produtos
em ordem crescente de preços.

Nesse estudo de caso, mostraremos como árvores binárias e
classes de tipo podem fornecer uma solução elegante para
esse problema.

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
>     } deriving (Eq, Ord, Show)

