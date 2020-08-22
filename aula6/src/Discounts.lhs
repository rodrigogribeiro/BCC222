Classes: Functor and Foldable
=============================

> module Discounts where

> import qualified Data.Map as M
> import qualified Data.Set as S
> import StoreModel
> import Tree

Maps, Sets, pacotes e módulos
-----------------------------

Nesta aula, vamos iniciar a desenvolver
programas que usam bibliotecas da linguagem
Haskell. Em particular, vamos utilizar o
pacote containers, que possui implementação
de diversas estruturas de dados.

Em particular, vamos dar um enfoque maior
para Maps --- que consistem da versão
funcional de tabelas de hash --- e
conjuntos (tipo Set).

Adicionando pacotes
-------------------

O Haskell stack faz o gerenciamento de pacotes
usando um arquivo chamado package.yaml presente
na raiz de um projeto. Esse arquivo armazena
diversas configurações relativas ao projeto.
A inclusão de novos pacotes é feita na seção
chamada dependencies. Como desejamos usar o
pacote containers, basta incluí-lo na linha
correspondente desta seção, conforme a seguir:

dependencies:
- base >= 4.7 && < 5
- containers

É possível incluir restrições para versões dos
pacotes. Como usaremos apenas funções básicas
sobre Maps e conjuntos, não há necessidade de
especificar uma versão (ou um intervalo de versões)
para o pacote containers.

Após a modificação do arquivo packages.yaml, basta
executar o comando

stack build

para que o stack baixe e instale a biblioteca e
recompile todo o projeto.


Map
---

O módulo Data.Map provê o tipo Map k v de
mapeamentos (tabelas) que associam chaves
de tipo k a valores de tipo v.
Ao importar o módulo Data.Map usamos

import qualified Data.Map as M

que torna visível todas as definições do
módulo mas adiciona a estas o prefixo "M"
para evitar o conflito de nome com funções
do Prelude.

No exemplo seguinte, utilizamos uma primeira
função sobre o tipo Map: fromList permite
converter uma lista de tipo [(k,v)] em
Map k v.

> mapDB :: M.Map Name Product
> mapDB
>   = M.fromList $ map go database
>     where
>       go p = (name p, p)

A função empty cria um Map vazio.

> emptyMapDB :: Ord k => M.Map k v
> emptyMapDB = M.empty

A função lookup permite recuperar o
valor associado a uma chave em um Map.

> kaiser :: Maybe Product
> kaiser
>   = M.lookup (Name "Kaiser") mapDB

Evidentemente, o tipo Map suporta muitas
outras operações como inserção, remoção,
contar número de elementos, etc.

Maiores informações sobre a biblioteca
Data.Map podem ser consultadas usando o
site Hoogle, de busca por documentação
de bibliotecas Haskell:

https://hoogle.haskell.org

Set
---

O módulo Data.Set consiste da implementação
de um tipo abstrato de dados conjunto. Assim
como Data.Map, esse módulo possui diversas
funções que possuem nomes em conflito com
a biblioteca Prelude e, por isso, normalmente
deve ser importado usando a diretiva qualified.

O tipo Set possui uma interface bem similar à
de Map, sem a necessidade de uma chave. A
seguir, apresentamos alguns exemplos.

A função fromList constrói um Set a partir
de uma lista.

> set1 = S.fromList [1,2,3]
> set2 = S.fromList [2,3,4]

Operações de união e interseção são implementadas
pelas funções union e intersect:

> set1_union_set2 = set1 `S.union` set2
> set1_inter_set2 = set1 `S.intersection` set2

Várias outras operações são fornecidas pela
biblioteca. Maiores detalhes podem ser consultados
na documentação que pode ser acessada usando o Hoogle.

https://hoogle.haskell.org


Classe Functor
--------------

Vamos considerar o problema de aplicar um determinado
desconto em todos os produtos de nossa loja. Para isso,
poderíamos desenvolver a seguinte função:

> discountList :: Double -> [Product] -> [Product]
> discountList perc
>    = map (\ p -> p{price = perc * price p})

Porém, o uso de listas é bem longe do ideal para eficiência.
Dessa forma, podemos usar árvores binárias para melhorar o
tempo para a tarefa de aplicação de descontos.

> discountTree :: Double -> Tree Product -> Tree Product
> discountTree perc
>    = mapTree (\ p -> p{price = perc * price p})

A função mapTree aplica uma função a cada um dos valores
armazenados em uma árvore binária:

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node v l r)
  = Node (f v) (mapTree f l)
               (mapTree f r)

Evidentemente, temos um problema de duplicidade: idealmente
a funcionalidade de aplicar um desconto não deveria ser
dependente da estrutura de dados a armazenar os produtos
de nossa aplicação. Uma forma de permitir que essa operação
de descontos funcione independente da estrutura de dados,
devemos usar sobrecarga para a função de caminhamento de
árvores ou listas.

A biblioteca padrão de Haskell possui uma classe de tipos
para generalizar a função map para listas, a classe Functor.

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$>) = fmap

Por exemplo, as definições de Functor para listas e árvores
é como se segue:

instance Functor [] where
  fmap f = map f

> instance Functor Tree where
>   fmap _ Leaf = Leaf
>   fmap f (Node v l r)
>     = Node (f v) (f <$> l) (f <$> r)

Note que usamos o operador <$> ao invés da função fmap.
Usando fmap, podemos generalizar a função para desconto,
como se segue:

> discount :: Functor f => Double -> f Product -> f Product
> discount perc
>    = fmap (\ p -> p{price = perc * price p})

Dessa forma, a função discount pode ser utilizada a qualquer
estrutura de dados que armazena produtos.


Classe Foldable
---------------

Assim como a classe Functor generaliza a função map, a
classe Foldable é a generalização de foldr. A definição
de Foldable é como se segue:

class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldr   :: (a -> b -> b) -> b -> f a -> b

  fold    :: Monoid m => f m -> m
  foldr'  :: (a -> b -> b) -> b -> f a -> b
  foldl   :: (a -> b -> a) -> a -> f b -> a
  foldl'  :: (a -> b -> a) -> a -> f b -> a
  foldr1  :: (a -> a -> a) -> f a -> a
  foldl1  :: (a -> a -> a) -> f a -> a

Apesar de possuir várias funções, não é necessário
implementar todas elas. Basicamente, basta codificar
foldMap ou foldr que são usadas para definir todas
as outras funções.
