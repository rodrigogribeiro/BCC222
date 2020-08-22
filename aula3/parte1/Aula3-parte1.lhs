---
author: Programação Funcional
title: Tipos de dados
date: Prof. Rodrigo Ribeiro
---

Problema
========

- Representar informação sobre um cliente.
- Informação composta por:
    - Nome
    - Sobrenome
    - Aceita receber informações sobre promoções?

Modelagem
=========

- Informações representadas pelos tipos:
    - Nome :: String
    - Sobrenome :: String
    - Ofertas :: Bool
- Como definir o tipo cliente?

Modelagem
=========

- Podemos utilizar uma tupla!

> ex1 :: (String, String, Bool)
> ex1 = ("José", "Silva", False)

Problemas
=========

- Uso do tipo String é pouco informativo!
    - Como diferenciar de nome e sobrenome?
    - Usar a posição na tripla é algo
      propenso a erros.

Sinônimos
=========

- Podemos melhorar a legibilidade do código
usando sinônimos de tipos.

> type Name = String
> type Surname = String
> type SendOffer = Bool

Modelagem
=========

- Representando o exemplo anterior.

```haskell
type Client = (Name, Surname, SendOffer)

ex2 :: Client
ex2 = ("José", "Silva", False)
````

Modelagem
=========

- Agora, o código é mais informativo!

- Porém, ainda dependente da posição
de componentes no par.

Modelagem
=========

- O uso de sinônimos não define um novo tipo
de dados!

- Logo, os tipos Client e (String, String, Bool) são
idênticos!

- Como melhorar isso?


Tipos Algébricos
================

- Podemos definir um novo tipo de dados em Haskell.

> data Client
>   = Customer Name Surname SendOffer -- pessoa física
>   | Company  Name                   -- empresa

> ex3 :: Client
> ex3 = Customer "José" "Silva" False

> ex4 :: Client
> ex4 = Company "Distribuidora da Maria"

Tipos Algébricos
================

- Tipos algébricos definem:
    - Nome de um tipo: Client
    - Um conjunto de construtores: Costumer
- Construtores são funções para criar
  valores do tipo.

