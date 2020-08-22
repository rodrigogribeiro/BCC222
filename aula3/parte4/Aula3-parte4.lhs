---
author: Programação Funcional
title: Registros em Haskell
date: Prof. Rodrigo Ribeiro
---

Motivação
=========

- Considere o seguinte possível modelo
de nossa aplicação.

> type Name    = String
> type Surname = String

> data RPerson  = RPerson Name Surname

Problema
========

- Como definir funções para obter
o nome e sobrenome de uma pessoa?

- Solução: casamento de padrão

Solução
=======

> rname :: RPerson -> Name
> rname (RPerson nm _) = nm

> rsurname :: RPerson -> Surname
> rsurname (RPerson _ sm) = sm

Registros
=========

> data Person
>   = Person {
>       name    :: Name
>     , surname :: Surname
>     }

Funções de Projeção
===================

````haskell
name    :: Person -> Name
surname :: Person -> Surname
````

Exemplo
=======

> ex1 :: Person
> ex1 = Person { name    = "Rodrigo"
>              , surname = "Ribeiro"}

-- dependentes da posição

> ex2 :: Person
> ex2 = Person "Rodrigo" "Ribeiro"

Saudação
========

> greet :: Person -> String
> greet p = "Bem vindo, " ++ name p ++ "!"
