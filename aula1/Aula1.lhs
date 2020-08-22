---
author: Programação Funcional
title: Primeiros passos com Haskell
date: Prof. Rodrigo Ribeiro
---

Setup 
======

- Escolha um editor de texto: Atom, 
Sublime, VSCode, Emacs, Vi(m), etc...

- Instale o Haskell-stack: [https://www.haskellstack.org](https://www.haskellstack.org).

Setup
=====

- Os seguintes itens são opcionais:
     - HLint: ferramenta para sugestões para código Haskell.
     - Pandoc: ferramenta para produção dos slides usados na
     disciplina.

Haskell Stack
=============

- Ferramenta para gerenciar projetos e bibliotecas.

- Instala todas as dependendências automaticamente (incluindo o compilador GHC).

Intepretador
============

- No terminal, digite `stack ghci` e você irá obter o prompt do interpretador:

~~~~~{.haskell}
Prelude*>
~~~~~~

Interpretador
=============

- O GHCi é um REPL.
     - Read, Eval, Print, Loop.
- Prelude é a biblioteca importada por todo módulo Haskell.

- Exemplos do interpretador.
