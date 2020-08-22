---
author: Programação Funcional
title: Apresentação do curso
date: Prof. Rodrigo Ribeiro
---

Motivação
==========

- Características de linguagens funcionais incorporadas
em linguagens _mainstream_.

- Frameworks em linguagens atuais usam conceitos de 
linguagens funcionais.

Motivação
==========

- Diversas empresas usam linguagens funcionais para 
desenvolvimento de produtos.

- Nubank usa a linguagem Closure no desenvolvimento de 
todos seu produtos.

- Nubank comprou a empresa que desenvolve a linguagem.


Porquê Haskell?
==============

- Linguagem _funcional pura_.
- Linguagem fortemente tipada.
- Avaliação _lazy_.
- Polimorfismo paramétrico e de sobrecarga.

Programação funcional
=====================

- Funções são _first-class citizens_.
- Funções podem ser argumentos ou resultados de 
outras funções.

Exemplo
=======

- Realizar ação sobre uma coleção de elementos.

```java
Iterator it = list.iterator();
while (it.hasNext()) {
   Element e = it.next();
   action(e) ; // does something
}
```````

Exemplo
=======

- Realizar ação sobre uma coleção de elementos.

```haskell
map action list
```

Linguagem pura
==============

- Expressões em Haskell não possuem efeitos colaterais, por padrão.
     - Efeitos colaterais: atribuição, I/O, etc...

- Expressões produzem o mesmo resultado em todas as execuções.


Linguagem pura
==============

- Vantagens:
     - Facilita o teste e execução concorrente.

- O mesmo não é necessariamente verdade em linguagens 
como Java, C, Python...


Avaliação _lazy_
================

- Expressões somente são executadas quando necessárias
para o resultado final.

- Quando calculadas, seu resultado pode ser reutilizado.


Avaliação _lazy_
================

- Ou descartado, se não utilizado posteriormente.

- Recurso poderoso, mas pode comprometer a eficiência 
se não usado corretamente.

Tipagem forte
=============

- Sistemas de tipos classificam valores em _tipos_.

- Essa classificação é utilizada para detectar comportamentos 
indesejados em programas.


Tipagem forte
=============

- Linguagens podem ser classificadas de acordo com 
o momento de verificação de tipos.

- Linguagens com tipagem dinâmica fazem a verificação
durante a execução do código.


Tipagem forte
=============

- Linguagens com tipagem estática realizam a verificação
durante a compilação do código.

- Haskell é uma linguagem com tipagem estática e com 
um sistema de tipos muito _expressivo_.


Polimorfismo
============

- Haskell prove suporte a dois tipos de polimorfismo: 
paramétrico (aka generics) e de sobrecarga.

- Haskell possui um mecanismo poderoso de sobrecarga.


Foco do curso
=============

- Estudaremos os conceitos de programação funcional
aplicando-os em um problema: desenvolvimento de uma loja 
de cerveja.

