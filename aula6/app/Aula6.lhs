Classes de tipos
================

Nos últimos vídeos, abordamos o problema
de definir árvores binárias de busca.

> data Tree a -- polimórfica: variável de tipo.
>    = Leaf
>    | Node a (Tree a) (Tree a)
>    deriving Show

Note que a definição acima é polimórfica:
permite que árvores armazenem valores de
qualquer tipo.

Porém, ao implementarmos funcionalidades como
inserção e busca, precisamos comparar valores.
Para manter nossas definições polimórficas,
optamos por passar as funções de comparação
como parâmetros, como a seguir:

> insert1 :: (a -> a -> Bool) -> -- teste para <
>            (a -> a -> Bool) -> -- teste para ==
>            a                ->
>            Tree a           ->
>            Tree a
> insert1 lt eq v Leaf = Node v Leaf Leaf
> insert1 lt eq v (Node v' l r)
>    | eq v v'   = Node v' l r
>    | lt v v'   = Node v' (insert1 lt eq v l) r
>    | otherwise = Node v' l (insert1 lt eq v r)

Observe que o uso da definição anterior é poluída
por exigir que funções de comparação sejam passadas
como parâmetro. Uma forma de reduzir esse problema
é uso de um registro para armazenar as funções
de comparação.

> data LtEq a
>   = LtEq {
>       lt :: a -> a -> Bool -- menor, <
>     , eq :: a -> a -> Bool -- igualdade, ==
>     }

Usando o registro acima, temos a seguinte versão de
insert:

> insert2 :: LtEq a ->
>            a      ->
>            Tree a ->
>            Tree a
> insert2 lteq v Leaf = Node v Leaf Leaf
> insert2 lteq v (Node v' l r)
>    | eq lteq v v' = Node v' l r
>    | lt lteq v v' = Node v' (insert2 lteq v l) r
>    | otherwise    = Node v' l (insert2 lteq v r)

O uso do registro resolveu parcialmente o problema
de parâmetros artificiais por diminuindo-os de dois
para um.

Porém, como podemos implementar árvores de busca
usando polimorfismo paramétrico? Note que funções
de comparação são possuem o mesmo comportamento
para todos os tipos. O teste para igualdade em listas
funciona de forma distinta do teste de igualdade sobre
booleanos. A questão é que o tipo de polimorfismo
necessário para lidar com igualdade e comparação é o
que chamamos de **sobrecarga**.

A sobrecarga permite atribuir implementações distintas
a um mesmo símbolo. A linguagem Haskell foi pioneira
por combinar, de forma sistemática, o suporte a
sobrecarga com polimorfismo paramétrico. A solução
adotada por Haskell consiste no uso de classes de tipos.

Uma classe de tipo consiste em um nome, um parâmetro de
tipo e uma sequência de assinaturas de tipos de funções.
A seguir, apresento um exemplo de classe de tipos que
define operações de igualdade:


class Eq a where
  (==) :: a -> a -> Bool -- igualdade
  (/=) :: a -> a -> Bool -- desigualdade


A definição acima declara uma classe de nome Eq que
possui um parâmetro de tipo, a, e duas definições de
tipos para funções. De forma geral, definições de
classe possuem a forma:

class Name var where
     fun1 :: type1
     .
     .
     .
     funn :: typen

Intuitivamente, uma classe de tipos define um conjunto
de operações (uma interface) que são suportadas por
um tipo representado por uma variável.

Implementações das operações definidas por uma classe
de tipos são declaradas em uma instância. Uma
implementação de uma instância da classe Eq para o
tipo Bool é apresentada a seguir.

instance Eq Bool where
   -- (==) :: Bool -> Bool -> Bool
   True  == True  = True
   False == False = True
   _     == _     = False

   x /= y = not (x == y)

Observe que a instância acima apresenta implementações
da classe Eq para o tipo Bool. Os tipos das funções
== e /= é exatamente o tipo definido na classe Eq
quando substituímos a variável "a" pelo tipo Bool.

Instâncias podem prover implementações para tipos
polimórficos. Como exemplo, considere a seguinte
instância de Eq para listas:

instance Eq a => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys

  xs /= ys = not (xs == ys)


Para entender a definição acima, vamos analisá-la
passo a passo. Primeiramente, temos

Eq a => Eq [a]

que define que temos uma implementação da classe Eq
para o tipo [a] *se* houver uma implementação de Eq
para o tipo a, isto é, podemos testar igualdade de
listas se for possível testar a igualdade de seus
elementos. Em Haskell, chamamos ocorrências de nomes
de classes tipos à esquerda do símbolo => de
restrições. Falaremos mais sobre restrições adiante.

Vamos considerar a equação da igualdade para listas
não vazias.

  (x : xs) == (y : ys) = x == y && xs == ys

Veja que a primeira ocorrência de (==) no lado direito
da equação é aplicada sobre *elementos* da lista:

x == y

e, portanto, essa ocorrência possui tipo

Eq a => a -> a -> Bool

Por sua vez, em

xs == ys

temos que a igualdade possui tipo

Eq a => [a] -> [a] -> Bool

Nessa mesma expressão, podemos notar o uso da função
(==) com dois tipos (e implementações) distintos,
o que caracteriza a sobrecarga.

Restrições de classes, como Eq a, são utilizadas para
limitar o conjunto de tipos que uma expressão pode
ser utilizada polimorficamente. De maneira simples,
uma expressão de tipo

Eq a => a -> a -> Bool

pode ser utilizada sobre quaisquer parâmetros de tipo
"a" que possuam instâncias da classe Eq.

Algumas classes da biblioteca padrão
------------------------------------

Haskell possui uma vasta biblioteca de classes de tipos.
Apresentaremos as principais classes, sua interface de
funções.

* Classe Eq

O primeira classe considerada é Eq, que fornece uma
interface para tipos que suportam teste de igualdade.
A definição para Eq já foi apresentada anteriormente
nesse texto e, portanto, a omitiremos agora.

* Classe Ord

A classe Ord provê operações para comparação de valores
de um certo tipo.

class Eq a => Ord a where
   (<)     :: a -> a -> Bool
   (>)     :: a -> a -> Bool
   (>=)    :: a -> a -> Bool
   (<=)    :: a -> a -> Bool
   max     :: a -> a -> a
   min     :: a -> a -> a
   compare :: a -> a -> Ordering

em que o tipo Ordering é definido como

data Ordering = LT | EQ | GT

cujos contrutores denotam o resultado de
testes de comparação: LT (menor), EQ (igual)
e GT (maior).

Observe que a classe Ord possui uma retrição:

Eq a => Ord a

A restrição Eq a impõe que todo tipo que seja
instância de Ord deva também possuir uma
instância para a classe Eq.

* Classe Show

A classe Show possui uma única operação responsável
por converter valores em string.

class Show a where
  show :: a -> String

* Classe Num

A classe Num apresenta o conjunto de funções associadas
a tipos numéricos.

class Num a where
  (+)         :: a -> a -> a
  (-)         :: a -> a -> a
  (*)         :: a -> a -> a
  negate      :: a -> a
  abs         :: a -> a
  signum      :: a -> a
  fromInteger :: Integer -> a

A classe Num define o tipo de operações aritméticas
(soma, subtração e multiplicação), inteiros negativos (negate),
valor absoluto (abs), sinal (signum) e conversão de valores
inteiros para outros tipos núméricos (fromInteger).
Em especial, números inteiros são sobrecarregados em Haskell:

1 :: Num a => a

dessa forma, podemos usar literais inteiros para representar
valores de outros tipos por fornecer uma implementação para
a classe Num. Usaremos essa técnica posteriormente na disciplina.

* Classe Read

A classe Read provê uma operação para conversão de strings em
valores de um certo tipo.

class Read a where
  read :: String -> a


* Mais classes

Existem muitas outras classes de tipos presentes nas bibliotecas
de Haskell. Uma excelente referência que provê uma visão geral
das classes de tipos mais utilizadas por bibliotecas da linguagem
é o seguinte artigo:

https://wiki.haskell.org/Typeclassopedia


Implementações padrão
---------------------

Classes de tipos podem fornecer implementações padrão de suas
funções, evitando assim, que o programador tenha que implementar
toda a interface por completo. Um exemplo de implementações padrão
são as presentes na classe Eq

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x == y = not (x /= y)
  x /= y = not (x == y)

que define a igualdade (desigualdade) como a negação da
desigualdade (igualdade).


Derivação automática de instâncias
----------------------------------

O compilador Haskell GHC é capaz de calcular definições
padrão de algumas classes para novos tipos de dados. Para
acionar esse recurso basta adicionar a cláusula deriving
a definição de um tipo. O GHC pode gerar instâncias das
classes Eq, Ord, Show e Read e de muitas outras.

> search :: Eq a => a -> [a] -> Bool
> search _ [] = False
> search v (x : xs) = v == x || search v xs

> main :: IO ()
> main = return ()
