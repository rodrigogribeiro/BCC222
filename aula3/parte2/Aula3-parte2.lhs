---
author: Programação Funcional
title: Casamento de padrão
date: Prof. Rodrigo Ribeiro
---

Motivação
========

- No vídeo anterior, vimos como
definir um tipo de dados para
clientes.

Motivação
=========

- Alguns sinônimos

> type Name = String
> type Surname = String
> type SendOffer = Bool

Motivação
=========

> data Client
>   = Customer Name Surname SendOffer -- pessoa física
>   | Company  Name                   -- empresa

> ex1 :: Client
> ex1 = Customer "Jose" "Silva" False

> ex2 :: Client
> ex2 = Company "Distribuidora da Maria"

Problema
========

- Como criar uma mensagem de boas vindas
para o cliente assim que ele acessa a
aplicação?


Exemplo
=======

- Para o cliente

```haskell
ex1 = Customer "Jose" "Silva" False
```

desejamos apresentar a mensagem:

```haskell
Bem vindo, José!
```

Como fazer?
===========

- Para isso, vamos utilizar
casamento de padrão!

- Casamento de padrão permite definir
funções para analisar a estrutura
de seus parâmetros.

Exemplo
=======

> greet :: Client -> String
> greet (Customer nm sm so) = "Bem vindo, " ++ nm ++ "!"
> -- nm será o nome
> -- sm será o sobrenome
> -- so será o flag de ofertas
> greet (Company nm)        = "Bem vindo, " ++ nm ++ "!"
> -- nm será o nome
