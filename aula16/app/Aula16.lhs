---
title: Correção de programas.
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

Matemática
==========

- Funções matemática não dependem de
valores "ocultos" ou que podem ser alterados.
     - Ex: 2 + 3 = 5 tanto em 4 * (2 + 3) quanto
       em (2 + 3) * (2 + 3).

- Isso facilita a demonstração de propriedades
sobre essas funções.

Matemática
===========

- Exemplo de propriedades (teoremas):

$$
\begin{array}{l}
\forall x y. x + y = y + x \\
\forall x y. x \times y = y \times x \\
\forall x y z. x + (y + z) = (x + y) + z \\
\forall x . x + 0 = 0 + x = x \\
\forall x y z. x \times (y + z) = (x \times y) + (x \times z)\\
\end{array}
$$

Utilidade
=========

- Teoremas podem ajudar na **performance**
    - Substituir implementações ineficientes por
      equivalentes mais eficientes.
- Teoremas são a forma de mostrar que seu código
atende os requisitos corretamente.

Utilidade
=========

"Correctness is clearly the prime quality.
If a system does not do what it is supposed
to do, then everything else about it
matters little."

- Bertrand Meyer, criador da linguagem Eiffel.

Álgebra
=======

- Em matemática, é comum termos demonstrações
similares a:
$$
\begin{array}{lcl}
(a + b)^2 & = & \textit{def. de }x^2\\
(a + b)\times (a + b) & = & \textit{distr.}\\
((a + b) \times a) + ((a + b) \times b) & = & \textit{comut.}\\
(a \times (a + b)) + (b \times (a + b)) & = & \textit{distr.}\\
(a \times a + a \times b) + (b \times a + b\times b) & = & ...\\
\end{array}
$$

Álgebra
=======

- Continuando...
$$
\begin{array}{lcl}
(a \times a + a \times b) + (b \times a + b\times b) & = & \textit{assoc.} \\
a \times a + (a \times b + b \times a) + b\times b & = & \textit{comut.} \\
a \times a + (a \times b + a \times b) + b\times b & = & \textit{comut.} \\
a^2 + 2 \times a \times b + b^2 & = &\textit{def. de}x^2 \textit{ e de} +\\
\end{array}
$$

Em Haskell
==========

- Como Haskell possui transparência referencial, podemos provar propriedades
sobre programas usando raciocínio baseado em equações, como na matemática.


Exemplo
=======

- Considere a definição da função `reverse`:

~~~~{.haskell}
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]
~~~~~

Exemplo
=======

- Provar que `forall x. reverse [x] = [x]`.

~~~~{.haskell}
reverse [x]       = -- list notation
~~~~~

Exemplo
=======

- Provar que `forall x. reverse [x] = [x]`.

~~~~{.haskell}
reverse [x]       = -- list notation
reverse (x : [])  = -- def. reverse
~~~~~

Exemplo
=======

- Provar que `forall x. reverse [x] = [x]`.

~~~~{.haskell}
reverse [x]       = -- list notation
reverse (x : [])  = -- def. reverse
reverse [] ++ [x] = -- def. reverse
~~~~~

Exemplo
=======

- Provar que `forall x. reverse [x] = [x]`.

~~~~{.haskell}
reverse [x]       = -- list notation
reverse (x : [])  = -- def. reverse
reverse [] ++ [x] = -- def. reverse
[] ++ [x]         = -- def. ++
~~~~~

Exemplo
=======

- Provar que `forall x. reverse [x] = [x]`.

~~~~{.haskell}
reverse [x]       = -- list notation
reverse (x : [])  = -- def. reverse
reverse [] ++ [x] = -- def. reverse
[] ++ [x]         = -- def. ++
[x]
~~~~~


Exercício
=========

- Prove que, para todo `x :: a` e `f :: a -> b`,
`map f [x] = [f x]`, usando a definição de `map`.

~~~~{.haskell}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs
~~~~

Análise de casos
================

- Em algumas situações, é necessário considerar
as diferentes possibilidades de parâmetros de
entrada.

- Exemplo: provar que `not` é involutivo.

~~~~{.haskell}
forall x. not (not x) = x
~~~~~

Análise de casos
================

- Definição de `not`:

~~~~~{.haskell}
not :: Bool -> Bool
not False = True
not True  = False
~~~~~

Análise de casos
================

- Provando que `not (not x) = x`.

- Caso `x = False`:

~~~~{.haskell}
not (not False) = -- def. de not
not True        = -- def. de not
False
~~~~~~

Análise de casos
================

- Provando que `not (not x) = x` (cont.).

- Caso `x = True`:

~~~~{.haskell}
not (not True) = -- def. de not
not False      = -- def. de not
True
~~~~~


Exercício
=========

- Prove que a operação de disjunção, `(||)`,
atende as seguintes propriedades:

~~~~{.haskell}
forall a b c. a || (b || c) = (a || b) || c
forall a. a || False = a
forall b. False || b = b
~~~~~~

Tipos recursivos
================

Números naturais
================

- Representando números naturais na notação de
Peano.

\begin{code}
data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show)
\end{code}

- Exemplos

~~~~{.haskell}
two :: Nat
two = Succ (Succ Zero)
~~~~~

Números naturais
================

- Representando a soma.

\begin{code}
(.+.) :: Nat -> Nat -> Nat
Zero      .+. m = m -- 1
(Succ n') .+. m = Succ (n' .+. m) -- 2
\end{code}


Números naturais
================

- Exemplo:

~~~~~{.haskell}
(Succ (Succ Zero)) .+. (Succ Zero) = -- eq. 2
~~~~~

Números naturais
================

- Exemplo:

~~~~~{.haskell}
(Succ (Succ Zero)) .+. (Succ Zero) = -- eq. 2
Succ ((Succ Zero) .+. (Succ Zero)) = -- eq. 2
~~~~~

Números naturais
================

- Exemplo:

~~~~~{.haskell}
(Succ (Succ Zero)) .+. (Succ Zero) = -- eq. 2
Succ ((Succ Zero) .+. (Succ Zero)) = -- eq. 2
Succ (Succ (Zero .+. (Succ Zero))) = -- eq. 1
~~~~~

Números naturais
================

- Exemplo:

~~~~~{.haskell}
(Succ (Succ Zero)) .+. (Succ Zero) = -- eq. 2
Succ ((Succ Zero) .+. (Succ Zero)) = -- eq. 2
Succ (Succ (Zero .+. (Succ Zero))) = -- eq. 1
Succ (Succ (Succ Zero))
~~~~~


Números naturais
================

- Usando a definição de soma (equação 1), temos que:

~~~~~{.haskell}
forall n. Zero .+. n = n
~~~~~~

- Parece óbvio que a seguinte propriedade também deve
ser verdadeira:

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

Números naturais
================

- Porém, a propriedade não é imediata a partir das
equações 1 e 2 da adição.

- Afinal, não é possível determinar se `n = Zero` ou
se `n = Succ n'`, para algum `n'` em

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

Números naturais
================

- Como a adição é definida recursivamente, não podemos
usar análise de casos para concluir a prova de

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

- Para isso, devemos usar **indução**.

Tipos recursivos
================

- Provas envolvendo funções recursivas são
realizadas por indução.

- Casos base são construtores do tipo que
não envolvem recursão.

- Passo indutivo para construtores envolvendo
recursão.

Indução sobre `Nat`
==================

- Para provar `forall x :: Nat. P (x)`, basta
provar:
     - `P(Zero)`.
     - `forall n :: Nat . P(n) -> P(Succ n)`.

Indução sobre `Nat`
===================

- Para a propriedade

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

`P(n)` é dado por `n .+. Zero = n`.

Indução sobre `Nat`
===================

- Para a propriedade

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

`P(Zero)` é dado por `Zero .+. Zero = Zero`.

Indução sobre `Nat`
===================

- Para a propriedade

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

`forall n. P(n) -> P(Succ n)` é dado por:

~~~~~{.haskell}
forall n. n .+. Zero = n -> (Succ n) .+. Zero = (Succ n)
~~~~~

Indução sobre `Nat`
==================

- Provando a propriedade

~~~~~{.haskell}
forall n. n .+. Zero = n
~~~~~~

- Caso base: `n = Zero`.

~~~~{.haskell}
Zero .+. Zero = -- def. de .+.
Zero
~~~~~

Indução sobre `Nat`
==================

- Caso indutivo: `n = Succ n'`.
    - Hipótese de indução: `n' .+. Zero = n'`.

~~~~~{.haskell}
(Succ n') .+. Zero = -- def. de .+.
~~~~~~

Indução sobre `Nat`
==================

- Caso indutivo: `n = Succ n'`.
    - Hipótese de indução: `n' .+. Zero = n'`.

~~~~~{.haskell}
(Succ n') .+. Zero = -- def. de .+.
Succ (n' .+. Zero) = -- H.I.
~~~~~~

Indução sobre `Nat`
==================

- Caso indutivo: `n = Succ n'`.
    - Hipótese de indução: `n' .+. Zero = n'`.

~~~~~{.haskell}
(Succ n') .+. Zero = -- def. de .+.
Succ (n' .+. Zero) = -- H.I.
Succ n'
~~~~~~


Indução sobre `Nat`
==================

- Mais um exemplo:

~~~~{.haskell}
forall n m. Succ (n .+. m) = n .+. (Succ m)
~~~~~

- Prova por indução sobre `n :: Nat`.

Indução sobre `Nat`
==================

- Caso base: `n = Zero`. Suponha `m :: Nat` arbitrário.

~~~~{.haskell}
Succ (Zero .+. m) = -- def. de .+.
~~~~~~

Indução sobre `Nat`
==================

- Caso base: `n = Zero`. Suponha `m :: Nat` arbitrário.

~~~~{.haskell}
Succ (Zero .+. m) = -- def. de .+.
Succ m            = -- def. de .+.
~~~~~~

Indução sobre `Nat`
==================

- Caso base: `n = Zero`. Suponha `m :: Nat` arbitrário.

~~~~{.haskell}
Succ (Zero .+. m) = -- def. de .+.
Succ m            = -- def. de .+.
Zero .+. Succ m
~~~~~~


Indução sobre `Nat`
===================

- Caso indutivo: `n = Succ n'`. Suponha `m :: Nat` arbitrário
e que `Succ (n' .+. m) = n' .+. (Succ m)`. Temos:

~~~~{.haskell}
Succ (Succ n') .+. m   = -- def. de .+.
~~~~~

Indução sobre `Nat`
===================

- Caso indutivo: `n = Succ n'`. Suponha `m :: Nat` arbitrário
e que `Succ (n' .+. m) = n' .+. (Succ m)`. Temos:

~~~~{.haskell}
Succ (Succ n') .+. m   = -- def. de .+.
Succ (Succ (n' .+. m)) = -- H.I.
~~~~~

Indução sobre `Nat`
===================

- Caso indutivo: `n = Succ n'`. Suponha `m :: Nat` arbitrário
e que `Succ (n' .+. m) = n' .+. (Succ m)`. Temos:

~~~~{.haskell}
Succ (Succ n') .+. m   = -- def. de .+.
Succ (Succ (n' .+. m)) = -- H.I.
Succ (n' .+. (Succ m)) = -- def. de .+.
~~~~~

Indução sobre `Nat`
===================

- Caso indutivo: `n = Succ n'`. Suponha `m :: Nat` arbitrário
e que `Succ (n' .+. m) = n' .+. (Succ m)`. Temos:

~~~~{.haskell}
Succ (Succ n') .+. m   = -- def. de .+.
Succ (Succ (n' .+. m)) = -- H.I.
Succ (n' .+. (Succ m)) = -- def. de .+.
(Succ n') .+. (Succ m)
~~~~~


Exercícios
==========

- Prove que a adição é uma operação associativa, isto é:

~~~~{.haskell}
forall n m p. (n .+. m) .+. p = n .+. (m .+. p)
~~~~~

Exercícios
==========

- Implemente a operação de multiplicação sobre o tipo `Nat`.


Exercícios
==========

- Um monóide é uma estrutura algébrica formada por um conjunto,
um operador binário e um elemento atendendo as seguintes condições:

$$
\begin{array}{l}
\forall x\,y\,z.x \circ (y \circ z) = (x \circ y) \circ z\\
\forall x . x \circ \epsilon = \epsilon \circ x = x\\
\end{array}
$$

Exercícios
==========

- Prove que a operação de multiplicação sobre o tipo `Nat` forma
um monóide quando consideramos $\epsilon = 1$.

Exercícios
==========

- Prove que a operação de adição sobre o tipo `Nat` é comutativa, isto é:

~~~~{.haskell}
forall n m. n .+. m = m .+. n
~~~~~~~

Exercícios
==========

- Prove que a operação de multiplicação sobre o tipo `Nat` é comutativa.

Listas
======


Indução
=======

- Para provar uma propriedade

~~~~{.haskell}
forall xs :: [a] . P (xs)
~~~~

Devemos provar:

- `P([])`
- `forall x xs. P(xs) -> P(x : xs)`

Exemplo
=======

- Provar que:

~~~~{.haskell}
forall xs ys. length (xs ++ ys) = length xs + length ys
~~~~~

Provaremos por indução sobre a lista `xs`.


Exemplo
=======

- Definições de `length` e `(++)`:

~~~~{.haskell}
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs
~~~~~

Exemplo
=======

- Caso base: `xs = []`. Suponha `ys :: [a]` arbitrário.

~~~~{.haskell}
length ([] ++ ys) = -- def. de ++
~~~~~

Exemplo
=======

- Caso base: `xs = []`. Suponha `ys :: [a]` arbitrário.

~~~~{.haskell}
length ([] ++ ys) = -- def. de ++
length ys         = -- aritmética
~~~~~

Exemplo
=======

- Caso base: `xs = []`. Suponha `ys :: [a]` arbitrário.

~~~~{.haskell}
length ([] ++ ys) = -- def. de ++
length ys         = -- aritmética
0 + length ys     = -- def. de length
~~~~~

Exemplo
=======

- Caso base: `xs = []`. Suponha `ys :: [a]` arbitrário.

~~~~{.haskell}
length ([] ++ ys) = -- def. de ++
length ys         = -- aritmética
0 + length ys     = -- def. de length
length [] + length ys
~~~~~

Exemplo
=======

- Caso base: `xs = z : zs`. Suponha `z :: a`, `zs ys :: [a]` arbitrários
e que `length (zs ++ ys) = length zs  + length ys`.

~~~~{.haskell}
length ((z : zs) ++ ys)     = -- def. de ++
~~~~

Exemplo
=======

- Caso base: `xs = z : zs`. Suponha `z :: a`, `zs ys :: [a]` arbitrários
e que `length (zs ++ ys) = length zs  + length ys`.

~~~~{.haskell}
length ((z : zs) ++ ys)     = -- def. de ++
length (z : (zs ++ ys))     = -- def. de length
~~~~

Exemplo
=======

- Caso base: `xs = z : zs`. Suponha `z :: a`, `zs ys :: [a]` arbitrários
e que `length (zs ++ ys) = length zs  + length ys`.

~~~~{.haskell}
length ((z : zs) ++ ys)     = -- def. de ++
length (z : (zs ++ ys))     = -- def. de length
1 + length (zs ++ ys)       = -- H.I.
~~~~

Exemplo
=======

- Caso base: `xs = z : zs`. Suponha `z :: a`, `zs ys :: [a]` arbitrários
e que `length (zs ++ ys) = length zs  + length ys`.

~~~~{.haskell}
length ((z : zs) ++ ys)     = -- def. de ++
length (z : (zs ++ ys))     = -- def. de length
1 + length (zs ++ ys)       = -- H.I.
1 + (length zs + length ys) = -- aritmética
~~~~

Exemplo
=======

- Caso base: `xs = z : zs`. Suponha `z :: a`, `zs ys :: [a]` arbitrários
e que `length (zs ++ ys) = length zs  + length ys`.

~~~~{.haskell}
length ((z : zs) ++ ys)     = -- def. de ++
length (z : (zs ++ ys))     = -- def. de length
1 + length (zs ++ ys)       = -- H.I.
1 + (length zs + length ys) = -- aritmética
(1 + length zs) + length ys = -- def. de length
~~~~

Exemplo
=======

- Caso base: `xs = z : zs`. Suponha `z :: a`, `zs ys :: [a]` arbitrários
e que `length (zs ++ ys) = length zs  + length ys`.

~~~~{.haskell}
length ((z : zs) ++ ys)     = -- def. de ++
length (z : (zs ++ ys))     = -- def. de length
1 + length (zs ++ ys)       = -- H.I.
1 + (length zs + length ys) = -- aritmética
(1 + length zs) + length ys = -- def. de length
length (z : zs) + length ys
~~~~

Exercício
=========

- Prove que a concatenação de listas é uma operação associativa, isto é:

~~~~{.haskell}
forall xs ys zs :: [a]. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
~~~~~

Exemplo
=======

- Provar a seguinte propriedade de `map`:

~~~~~{.haskell}
forall xs :: [a]. map id xs = xs
~~~~~~

Exemplo
=======

- Caso `xs = []`

~~~~{.haskell}
map id [] = -- def. de map
[]
~~~~~

Exemplo
=======

- Caso `xs = y : ys`. Suponha `y :: a` e `ys :: [a]` arbitrários
e que `map id ys = ys`.

~~~~{.haskell}
map id (y : ys)  = -- def. de map
~~~~~

Exemplo
=======

- Caso `xs = y : ys`. Suponha `y :: a` e `ys :: [a]` arbitrários
e que `map id ys = ys`.

~~~~{.haskell}
map id (y : ys)  = -- def. de map
id y : map id ys = -- H.I.
~~~~~

Exemplo
=======

- Caso `xs = y : ys`. Suponha `y :: a` e `ys :: [a]` arbitrários
e que `map id ys = ys`.

~~~~{.haskell}
map id (y : ys)  = -- def. de map
id y : map id ys = -- H.I.
id y : ys        = -- def. de id
~~~~~

Exemplo
=======

- Caso `xs = y : ys`. Suponha `y :: a` e `ys :: [a]` arbitrários
e que `map id ys = ys`.

~~~~{.haskell}
map id (y : ys)  = -- def. de map
id y : map id ys = -- H.I.
id y : ys        = -- def. de id
y : ys
~~~~~

Map fusion
==========

- Teorema que permite compor dois caminhamentos sobre uma lista como
um único.

- Formalmente:

~~~~~{.haskell}
forall xs :: [a], f :: a -> b, g :: b -> c. (map g . map f) xs = map (g . f) xs
~~~~~~

Map fusion
==========

- Caso base: `xs = []`.

~~~~{.haskell}
(map g . map f) [] = -- def. de (.)
~~~~~

Map fusion
==========

- Caso base: `xs = []`.

~~~~{.haskell}
(map g . map f) [] = -- def. de (.)
map g (map f [])   = -- def. de map
~~~~~

Map fusion
==========

- Caso base: `xs = []`.

~~~~{.haskell}
(map g . map f) [] = -- def. de (.)
map g (map f [])   = -- def. de map
map g []           = -- def. de map
~~~~~

Map fusion
==========

- Caso base: `xs = []`.

~~~~{.haskell}
(map g . map f) [] = -- def. de (.)
map g (map f [])   = -- def. de map
map g []           = -- def. de map
[]                 = -- def. de map
~~~~~

Map fusion
==========

- Caso base: `xs = []`.

~~~~{.haskell}
(map g . map f) [] = -- def. de (.)
map g (map f [])   = -- def. de map
map g []           = -- def. de map
[]                 = -- def. de map
map (g . f) []
~~~~~


Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
map g (f y : map f ys)             = -- def. de map
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
map g (f y : map f ys)             = -- def. de map
g (f y) : (map g (map f ys))       = -- def. de (.)
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
map g (f y : map f ys)             = -- def. de map
g (f y) : (map g (map f ys))       = -- def. de (.)
((g . f) y) : (map g (map f ys))   = -- def. de (.)
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
map g (f y : map f ys)             = -- def. de map
g (f y) : (map g (map f ys))       = -- def. de (.)
((g . f) y) : (map g (map f ys))   = -- def. de (.)
((g . f) y) : ((map g . map f) ys) = -- H.I.
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
map g (f y : map f ys)             = -- def. de map
g (f y) : (map g (map f ys))       = -- def. de (.)
((g . f) y) : (map g (map f ys))   = -- def. de (.)
((g . f) y) : ((map g . map f) ys) = -- H.I.
((g . f) y) : map (g . f) ys       = -- def. de map
~~~~~

Map fusion
==========

- Caso indutivo: `xs = y : ys`. Suponha `y` e `ys` arbitrários e que
`(map g . map f) ys = map (g . f) ys`.

~~~~{.haskell}
(map g . map f) (y : ys)           = -- def. de (.)
map g (map f (y : ys))             = -- def. de map
map g (f y : map f ys)             = -- def. de map
g (f y) : (map g (map f ys))       = -- def. de (.)
((g . f) y) : (map g (map f ys))   = -- def. de (.)
((g . f) y) : ((map g . map f) ys) = -- H.I.
((g . f) y) : map (g . f) ys       = -- def. de map
map (g . f) (y : ys)
~~~~~

Exercício
==========

- Prove a seguinte propriedade sobre `map`:

~~~~{.haskell}
forall xs ys f. map f (xs ++ ys) = map f xs ++ map f ys
~~~~~

Exercício
=========

- Prove a seguinte propriedade sobre `map`:

~~~~{.haskell}
forall xs f. length (map f xs) = length xs
~~~~~

Reverse
=======

- Provar a seguinte propriedade:

~~~~{.haskell}
forall xs ys. reverse (xs ++ ys) = reverse ys ++ reverse xs
~~~~~

Reverse
=======

- Caso `xs = []`. Suponha `ys` arbitrário.

~~~~~{.haskell}
reverse ([] ++ ys) = -- def. de ++
~~~~~~

Reverse
=======

- Caso `xs = []`. Suponha `ys` arbitrário.

~~~~~{.haskell}
reverse ([] ++ ys) = -- def. de ++
reverse ys         = -- Prop. forall ys. ys ++ [] = ys
~~~~~~

Reverse
=======

- Caso `xs = []`. Suponha `ys` arbitrário.

~~~~~{.haskell}
reverse ([] ++ ys) = -- def. de ++
reverse ys         = -- Prop. forall ys. ys ++ [] = ys
reverse ys ++ []   =
~~~~~~

Reverse
=======

- Caso `xs = []`. Suponha `ys` arbitrário.

~~~~~{.haskell}
reverse ([] ++ ys) = -- def. de ++
reverse ys         = -- Prop. forall ys. ys ++ [] = ys
reverse ys ++ []   =
reverse ys ++ reverse []
~~~~~~

Reverse
=======

- Caso `xs = z : zs`. Suponha `z`, `zs` e `ys` arbitrários
e que `reverse (zs ++ ys) = reverse ys ++ reverse zs`.

~~~~~{.haskell}
reverse ((z : zs) ++ ys)          = -- def. de ++
~~~~~~~

Reverse
=======

- Caso `xs = z : zs`. Suponha `z`, `zs` e `ys` arbitrários
e que `reverse (zs ++ ys) = reverse ys ++ reverse zs`.

~~~~~{.haskell}
reverse ((z : zs) ++ ys)          = -- def. de ++
reverse (z : (zs ++ ys))          = -- def. de reverse
~~~~~~~

Reverse
=======

- Caso `xs = z : zs`. Suponha `z`, `zs` e `ys` arbitrários
e que `reverse (zs ++ ys) = reverse ys ++ reverse zs`.

~~~~~{.haskell}
reverse ((z : zs) ++ ys)          = -- def. de ++
reverse (z : (zs ++ ys))          = -- def. de reverse
reverse (zs ++ ys) ++ [z]         = -- H.I.
~~~~~~~

Reverse
=======

- Caso `xs = z : zs`. Suponha `z`, `zs` e `ys` arbitrários
e que `reverse (zs ++ ys) = reverse ys ++ reverse zs`.

~~~~~{.haskell}
reverse ((z : zs) ++ ys)          = -- def. de ++
reverse (z : (zs ++ ys))          = -- def. de reverse
reverse (zs ++ ys) ++ [z]         = -- H.I.
(reverse ys ++ reverse zs) ++ [z] = -- Prop. ++ associativo.
~~~~~~~

Reverse
=======

- Caso `xs = z : zs`. Suponha `z`, `zs` e `ys` arbitrários
e que `reverse (zs ++ ys) = reverse ys ++ reverse zs`.

~~~~~{.haskell}
reverse ((z : zs) ++ ys)          = -- def. de ++
reverse (z : (zs ++ ys))          = -- def. de reverse
reverse (zs ++ ys) ++ [z]         = -- H.I.
(reverse ys ++ reverse zs) ++ [z] = -- Prop. ++ associativo.
reverse ys ++ (reverse zs ++ [z]) = -- def. de reverse
~~~~~~~

Reverse
=======

- Caso `xs = z : zs`. Suponha `z`, `zs` e `ys` arbitrários
e que `reverse (zs ++ ys) = reverse ys ++ reverse zs`.

~~~~~{.haskell}
reverse ((z : zs) ++ ys)          = -- def. de ++
reverse (z : (zs ++ ys))          = -- def. de reverse
reverse (zs ++ ys) ++ [z]         = -- H.I.
(reverse ys ++ reverse zs) ++ [z] = -- Prop. ++ associativo.
reverse ys ++ (reverse zs ++ [z]) = -- def. de reverse
reverse ys ++ (reverse (z : zs))
~~~~~~~


Exercício
=========

- Prove a seguinte propriedade:

~~~~~{.haskell}
forall xs f. reverse (map f xs) = map f (reverse xs)
~~~~~~

Fold/Map fusion
===============

- Permite combinar duas operações sobre listas em uma única.
     - Idéia subjacente ao framework map/reduce.

~~~~{.haskell}
forall xs f g v. (foldr g v . map f) xs = foldr (g . f) v xs
~~~~~

Fold/Map fusion
===============

- Caso base: `xs = []`. Suponha `f`, `g` e `v` arbitrários.

~~~~{.haskell}
(foldr g v . map f) [] = -- def. de (.)
~~~~~~

Fold/Map fusion
===============

- Caso base: `xs = []`. Suponha `f`, `g` e `v` arbitrários.

~~~~{.haskell}
(foldr g v . map f) [] = -- def. de (.)
foldr g v (map f [])   = -- def. de map
~~~~~~

Fold/Map fusion
===============

- Caso base: `xs = []`. Suponha `f`, `g` e `v` arbitrários.

~~~~{.haskell}
(foldr g v . map f) [] = -- def. de (.)
foldr g v (map f [])   = -- def. de map
foldr g v []           = -- def. de foldr
~~~~~~

Fold/Map fusion
===============

- Caso base: `xs = []`. Suponha `f`, `g` e `v` arbitrários.

~~~~{.haskell}
(foldr g v . map f) [] = -- def. de (.)
foldr g v (map f [])   = -- def. de map
foldr g v []           = -- def. de foldr
v                      = -- def. de foldr
~~~~~~

Fold/Map fusion
===============

- Caso base: `xs = []`. Suponha `f`, `g` e `v` arbitrários.

~~~~{.haskell}
(foldr g v . map f) [] = -- def. de (.)
foldr g v (map f [])   = -- def. de map
foldr g v []           = -- def. de foldr
v                      = -- def. de foldr
foldr (g . f) v []
~~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
foldr g v (map f (y : ys))         = -- def. de map
~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
foldr g v (map f (y : ys))         = -- def. de map
foldr g v (f y : map f ys)         = -- def. de foldr
~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
foldr g v (map f (y : ys))         = -- def. de map
foldr g v (f y : map f ys)         = -- def. de foldr
g (f y) (foldr g v (map f ys))     = -- def. de (.)
~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
foldr g v (map f (y : ys))         = -- def. de map
foldr g v (f y : map f ys)         = -- def. de foldr
g (f y) (foldr g v (map f ys))     = -- def. de (.)
(g . f) y ((foldr g v . map f) ys) = -- H.I.
~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
foldr g v (map f (y : ys))         = -- def. de map
foldr g v (f y : map f ys)         = -- def. de foldr
g (f y) (foldr g v (map f ys))     = -- def. de (.)
(g . f) y ((foldr g v . map f) ys) = -- H.I.
(g . f) y (foldr (g . f) v ys)     = -- def. de foldr
~~~~~

Fold/Map fusion
===============

- Caso indutivo: `xs = y : ys`. Suponha `f`, `g`, `v`, `y` e `ys` arbitrários e
que `(foldr g v . map f) ys = foldr (g . f) v ys`.

~~~~{.haskell}
(foldr g v . map f) (y : ys)       = -- def. de (.)
foldr g v (map f (y : ys))         = -- def. de map
foldr g v (f y : map f ys)         = -- def. de foldr
g (f y) (foldr g v (map f ys))     = -- def. de (.)
(g . f) y ((foldr g v . map f) ys) = -- H.I.
(g . f) y (foldr (g . f) v ys)     = -- def. de foldr
foldr (g . f) v (y : ys)
~~~~~

Exercício
=========

- Considere a função `elem`:

~~~~~{.haskell}
elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y : ys) = x == y || elem x ys
~~~~~

Prove a propriedade:

~~~~{.haskell}
forall xs ys x. elem x (xs ++ ys) = elem x xs || elem x ys
~~~~~~

Árvores
=======

Árvores
=======

- Para provar propriedades sobre árvores binárias, basta provar:
     - P(Leaf)
     - `forall l r x. P(l) -> P(r) -> P(Node x l r)`

Exemplo
=======

\begin{code}
data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)
      deriving (Eq, Ord, Show)

size :: Tree a -> Int
size Leaf = 0
size (Node _ l r) = 1 + size l + size r
\end{code}

Exemplo
=======

\begin{code}
height :: Tree a -> Int
height Leaf = 0
height (Node _ l r) = 1 + max (height l) (height r)
\end{code}

Exemplo
=======

- Provar que:

~~~~{.haskell}
forall t. height t <= size t
~~~~~

Exemplo
=======

- Caso base (t = `Leaf`):

~~~~{.haskell}
height Leaf  = -- def. height
~~~~~

Exemplo
=======

- Caso base (t = `Leaf`):

~~~~{.haskell}
height Leaf  = -- def. height
0           <= -- aritmética
~~~~~

Exemplo
=======

- Caso base (t = `Leaf`):

~~~~{.haskell}
height Leaf  = -- def. height
0           <= -- aritmética
0           =
~~~~~

Exemplo
=======

- Caso base (t = `Leaf`):

~~~~{.haskell}
height Leaf  = -- def. height
0           <= -- aritmética
0           =
size Leaf
~~~~~

Exemplo
=======

- Caso recursivo: (t = `Node x l r`). Suponha que
`height l <= size l` e `height r <= size r`.

~~~~~{.haskell}
height (Node x l r)           = -- def. de height
~~~~~~

Exemplo
=======

- Caso recursivo: (t = `Node x l r`). Suponha que
`height l <= size l` e `height r <= size r`.

~~~~~{.haskell}
height (Node x l r)           = -- def. de height
1 + max (height l) (height r) <= -- H.I.
~~~~~~

Exemplo
=======

- Caso recursivo: (t = `Node x l r`). Suponha que
`height l <= size l` e `height r <= size r`.

~~~~~{.haskell}
height (Node x l r)           = -- def. de height
1 + max (height l) (height r) <= -- H.I.
1 + max (size l) (size r)     <= -- aritmética
~~~~~~

Exemplo
=======

- Caso recursivo: (t = `Node x l r`). Suponha que
`height l <= size l` e `height r <= size r`.

~~~~~{.haskell}
height (Node x l r)           = -- def. de height
1 + max (height l) (height r) <= -- H.I.
1 + max (size l) (size r)     <= -- aritmética
1 + size l + size r           =
~~~~~~

Exemplo
=======

- Caso recursivo: (t = `Node x l r`). Suponha que
`height l <= size l` e `height r <= size r`.

~~~~~{.haskell}
height (Node x l r)           = -- def. de height
1 + max (height l) (height r) <= -- H.I.
1 + max (size l) (size r)     <= -- aritmética
1 + size l + size r           =
size (Node x l r)
~~~~~~

Exercício
=========

- Considere a função `toList`:

\begin{code}
toList :: Tree a -> [a]
toList Leaf = []
toList (Node x l r) = toList l ++ [x] ++ toList r
\end{code}


Exercício
=========

- Considere a função `member`:

\begin{code}
member :: Eq a => a -> Tree a -> Bool
member _ Leaf = False
member v (Node x l r) = v == x || member v l || member v r
\end{code}

Prove que:

~~~{.haskell}
forall t x. member x t = elem x (toList t)
~~~~

Expressões
==========

Estudo de caso
==============

- Uso de tipos de dados algébricos
para construção de um compilador
de expressões aritméticas para
uma máquina de pilha.


Estudo de caso
==============

- Máquina de pilha: modelo de linguagem
de baixo nível que usa uma pilha como
memória.

- Exemplo de máquina de pilha: JVM.

Sintaxe de expressões
=====================

\begin{code}
data Expr
   = Const Int      -- constantes
   | Expr :+: Expr  -- adição
   deriving (Eq, Ord, Show)
\end{code}

Exercício
=========

- Estenda a sintaxe de expressões para incluir a
operação de multiplicação.

Exemplo
=======

- Representando $2 + (1 + 3)$:

\begin{code}
sample :: Expr
sample
   = (Const 2) :+: (Const 1 :+: Const 3)
\end{code}

Semântica de expressões
=======================

- Interpretador de expressões.

\begin{code}
eval :: Expr -> Int
eval (Const n) = n
eval (e :+: e') = eval e + eval e'
\end{code}

Exercício
=========

- Estenda a interpretador de expressões para incluir a
operação de multiplicação.


Máquina de pilha
================

- Instruções

\begin{code}
data Instr
   = Push Int -- empilha uma constante
   | Add      -- soma dois números no topo da pilha
   deriving (Eq, Ord, Show)
\end{code}

Exercício
=========

- Estenda a sintaxe de instruções para incluir a operação
de multiplicação.

Máquina de pilha
================

- Interpretador

\begin{code}
type Stack = [Int]

instr :: Instr -> Stack -> Stack
instr (Push n) s = n : s
instr Add (n : n' : s) = (n + n') : s
\end{code}

Exercício
=========

- Estenda o interpretador de instruções para incluir a operação
de multiplicação.


Máquina de pilha
================

- Interpretando um programa

\begin{code}
type Program = [Instr]

exec :: Program -> Stack -> Stack
exec [] s = s
exec (Push n : ps) s = n : s
exec (Add : ps) (n : m : s) = (n + m) : s
\end{code}

Exercício
=========

- Estenda o interpretador de programas para incluir a operação
de multiplicação.

Exercício
=========

- Implemente a função `exec` usando `foldl`.

Compilador
==========

\begin{code}
compile :: Expr -> Program
compile (Const n) = [Push n]
compile (e :+: e') =
  compile e ++ compile e' ++ [Add]
\end{code}

Exercício
=========

- Estenda o compilador de expressões para incluir a operação
de multiplicação.

Exercício
=========

- Implemente a função de compilação usando um acumulador
de forma a evitar o uso de concatenação.

~~~~{.haskell}
compile :: Expr -> Program
compile = flip compile' []

compile' :: Expr -> Program -> Program
compile = -- sua implementação
~~~~~

Correção
========

- Problema: Como determinar se seu compilador está correto?

- Correção de compiladores é um problema sério.
    - Como saber se o erro está em seu código ou
      o compilador gerou código incorreto?

Correção
========

- Como dizer que um compilador é correto?

- Basicamente se o código resultante da compilação
produz o mesmo resultado do código fornecido como entrada.


Correção
========

- Como relacionar o resultados entre programas expressos
em diferentes linguagens?

- Comparando o resultado de execução de cada um.
    - Usaremos os resultados dos interpretadores.

Correção
========

- Propriedade de correção: provada por indução sobre
a estrutura da expressão.

~~~~~{.haskell}
forall e s. eval e : s == exec (compile e) s
~~~~~

Correção
========

- Para provarmos essa propriedade de correção,
precisaremos de um resultado auxiliar.

- Prova por indução sobre a estrutura da lista p.

~~~~{.haskell}
forall p p' s . exec (p ++ p') s = exec p' (exec p s)
~~~~~

Correção
========

- Caso base: `p = []`. Suponha `p', s` arbitrários.

~~~~{.haskell}
exec ([] ++ p') s = -- def. de ++
~~~~~

Correção
========

- Caso base: `p = []`. Suponha `p', s` arbitrários.

~~~~{.haskell}
exec ([] ++ p') s = -- def. de ++
exec p' s         = -- def. de exec.
~~~~~

Correção
========

- Caso base: `p = []`. Suponha `p', s` arbitrários.

~~~~{.haskell}
exec ([] ++ p') s = -- def. de ++
exec p' s         = -- def. de exec.
exec p' (exec [] s)
~~~~~


Correção
========

- Caso indutivo: `p = i : p`. Suponha que para todo `p',s` temos
`exec (p ++ p') s = exec p' (exec p s)`. Agora, consideremos os
seguintes casos sobre `i.`

Correção
========

- Caso `i = Push n`

~~~~{.haskell}
exec (((Push n) : p) ++ p') s = -- def. de ++
~~~~~~

Correção
========

- Caso `i = Push n`

~~~~{.haskell}
exec (((Push n) : p) ++ p') s = -- def. de ++
exec (Push n : (p ++ p')) s   = -- def. de exec
~~~~~~

Correção
========

- Caso `i = Push n`

~~~~{.haskell}
exec (((Push n) : p) ++ p') s = -- def. de ++
exec (Push n : (p ++ p')) s   = -- def. de exec
exec (p ++ p') (n : s)        = -- H.I.
~~~~~~

Correção
========

- Caso `i = Push n`

~~~~{.haskell}
exec (((Push n) : p) ++ p') s = -- def. de ++
exec (Push n : (p ++ p')) s   = -- def. de exec
exec (p ++ p') (n : s)        = -- H.I.
exec p' (exec p (n : s))      = -- def. de exec
~~~~~~

Correção
========

- Caso `i = Push n`

~~~~{.haskell}
exec (((Push n) : p) ++ p') s = -- def. de ++
exec (Push n : (p ++ p')) s   = -- def. de exec
exec (p ++ p') (n : s)        = -- H.I.
exec p' (exec p (n : s))      = -- def. de exec
exec p' (exec ((Push n) : p) s)
~~~~~~


Correção
========

- Caso `i = Add`

~~~~{.haskell}
exec ((Add : p) ++ p') s = -- def. de ++
~~~~~

Correção
========

- Caso `i = Add`

~~~~{.haskell}
exec ((Add : p) ++ p') s = -- def. de ++
exec (Add : (p ++ p')) s = -- s == (n : m : s')
~~~~~

Correção
========

- Caso `i = Add`

~~~~{.haskell}
exec ((Add : p) ++ p') s = -- def. de ++
exec (Add : (p ++ p')) s = -- s == (n : m : s')
exec (Add : (p ++ p')) (n : m : s') = -- def. de exec
~~~~~

Correção
========

- Caso `i = Add`

~~~~{.haskell}
exec ((Add : p) ++ p') s = -- def. de ++
exec (Add : (p ++ p')) s = -- s == (n : m : s')
exec (Add : (p ++ p')) (n : m : s') = -- def. de exec
exec (p ++ p') ((n + m) : s') = -- H.I.
~~~~~

Correção
========

- Caso `i = Add`

~~~~{.haskell}
exec ((Add : p) ++ p') s = -- def. de ++
exec (Add : (p ++ p')) s = -- s == (n : m : s')
exec (Add : (p ++ p')) (n : m : s') = -- def. de exec
exec (p ++ p') ((n + m) : s') = -- H.I.
exec p' (exec p ((n + m) : s')) = -- def. de exec
~~~~~

Correção
========

- Caso `i = Add`

~~~~{.haskell}
exec ((Add : p) ++ p') s = -- def. de ++
exec (Add : (p ++ p')) s = -- s == (n : m : s')
exec (Add : (p ++ p')) (n : m : s') = -- def. de exec
exec (p ++ p') ((n + m) : s') = -- H.I.
exec p' (exec p ((n + m) : s')) = -- def. de exec
exec p' (exec (Add : p) (n : m : s'))
~~~~~


Exercício
=========

- Modifique a prova anterior de maneira a considerar a operação
de multiplicação.

Correção
========

- De posse do resultado anterior, podemos provar a correção
do compilador de expressões.

~~~~~{.haskell}
forall e s. eval e : s == exec (compile e) s
~~~~~


Correção
========

- Caso `e = Const n`.

~~~~{.haskell}
eval (Const n) : s = -- def. de eval
~~~~~

Correção
========

- Caso `e = Const n`.

~~~~{.haskell}
eval (Const n) : s = -- def. de eval
n : s              = -- def. de exec
~~~~~

Correção
========

- Caso `e = Const n`.

~~~~{.haskell}
eval (Const n) : s = -- def. de eval
n : s              = -- def. de exec
exec [] (n : s)    = -- def. de exec
~~~~~

Correção
========

- Caso `e = Const n`.

~~~~{.haskell}
eval (Const n) : s = -- def. de eval
n : s              = -- def. de exec
exec [] (n : s)    = -- def. de exec
exec [ Push n ] s  = -- def. de compile
~~~~~

Correção
========

- Caso `e = Const n`.

~~~~{.haskell}
eval (Const n) : s = -- def. de eval
n : s              = -- def. de exec
exec [] (n : s)    = -- def. de exec
exec [ Push n ] s  = -- def. de compile
exec (compile (Const n)) s
~~~~~


Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
exec [Add] (eval e2 : eval e1 : s) = -- H.I. para e2
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
exec [Add] (eval e2 : eval e1 : s) = -- H.I. para e2
exec [Add] (exec (compile e2) (eval e1 : s)) = -- exec/exec
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
exec [Add] (eval e2 : eval e1 : s) = -- H.I. para e2
exec [Add] (exec (compile e2) (eval e1 : s)) = -- exec/exec
exec (compile e2 ++ [Add]) (eval e1 : s) = -- H.I. para e1
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
exec [Add] (eval e2 : eval e1 : s) = -- H.I. para e2
exec [Add] (exec (compile e2) (eval e1 : s)) = -- exec/exec
exec (compile e2 ++ [Add]) (eval e1 : s) = -- H.I. para e1
exec (compile e1 ++ (compile e2 ++ [Add])) s = -- ++ assossiativo
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
exec [Add] (eval e2 : eval e1 : s) = -- H.I. para e2
exec [Add] (exec (compile e2) (eval e1 : s)) = -- exec/exec
exec (compile e2 ++ [Add]) (eval e1 : s) = -- H.I. para e1
exec (compile e1 ++ (compile e2 ++ [Add])) s = -- ++ assossiativo
exec (compile e1 ++ compile e2 ++ [Add]) s = -- def. de compile
~~~~~

Correção
========

- Caso `e = e1 :+: e2`.

~~~~{.haskell}
eval (e1 :+: e2) s = -- def. de eval
(eval e1 + eval e2) : s = -- def. de exec
exec [Add] (eval e2 : eval e1 : s) = -- H.I. para e2
exec [Add] (exec (compile e2) (eval e1 : s)) = -- exec/exec
exec (compile e2 ++ [Add]) (eval e1 : s) = -- H.I. para e1
exec (compile e1 ++ (compile e2 ++ [Add])) s = -- ++ assossiativo
exec (compile e1 ++ compile e2 ++ [Add]) s = -- def. de compile
exec (compile (Add e1 e2)) s
~~~~~


Exercício
=========

- Estenda a prova anterior para lidar com a operação de multiplicação.

Conclusão
=========

- Correção de compiladores é um problema real e com
implicações importantes no desenvolvimento de software.

- Compiladores reais: compcert, compilador de C, totalmente verificado.
