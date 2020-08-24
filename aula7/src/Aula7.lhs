Avaliação lazy
==============

Introdução
-----------

A avaliação de Haskell é bastante diferente
das estratégias utilizadas pela maioria das
linguagens de programação.

Haskell utiliza avaliação _lazy_ que significa
que apenas as subexpressões necessárias para
alcançar o resultado final são executadas e
isso e feito no último momento possível.

Por exemplo, na expressão

head [2 + 3 , 4  * 5]

a multiplicação não é executada, por não
relevante para o resultado final.
