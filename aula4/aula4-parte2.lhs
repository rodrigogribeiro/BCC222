Polimorfismo Paramétrico
========================

* Permite definir funções que
operam da mesma forma sobre
qualquer tipo.

Funções polimórficas
--------------------

Exemplo: função head

:t head

head :: forall a . [a] -> a

Isso significa que a chamada

head "Hello!"

é tipada da seguinte forma

head     :: [a   ] -> a
"Hello!" :: [Char]       -- lembre-se String = [Char]
......................
head "Hello!" :: Char


Tipos polimórficos
------------------

data Maybe a = Just a | Nothing

> maybeString :: Maybe a -> String --- *** Maybe
> maybeString (Just _) = "Just"
> maybeString Nothing  = "Nothing"


Modelo de dados
---------------

> type Name = String
> type Surname = String

> data Person
>   = Person {
>       name    :: Name
>     , surname :: Surname
>     } deriving Show --

> data Client i -- i: variável de tipo
>   = Company {
>       companyId   :: i
>     , companyName :: Name
>     , contact     :: Person
>     }
>   | Individual {
>       individualId :: i
>     , person       :: Person
>     } deriving Show

Exemplos
--------

> ex1 :: Client String
> ex1 = Company {
>         companyId = "DistMaria"
>       , companyName = "Distribuidora da Maria"
>       , contact =
>           Person {
>             name = "Maria"
>           , surname = "Silva"
>           }
>       }

> ex2 :: Client Int
> ex2 = Company {
>         companyId = 1
>       , companyName = "Distribuidora da Maria"
>       , contact =
>           Person {
>             name = "Maria"
>           , surname = "Silva"
>           }
>       }
