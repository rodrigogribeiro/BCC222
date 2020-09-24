---
title: Elm
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

Front-end
===========

- Para desenvolver aplicações front-end:
     - Html: descreve a estrutura de páginas.
     - CSS: descreve estilos de páginas.
     - JavaScript: modifica páginas dinamicamente.
- Elm é uma linguagem funcional para realizar
essas tarefas

Elm
=====

![](elm.png "Elm language")

Elm
====

- Linguagem criada por Evan Czaplicki em 2012.
- Ganhando popularidade:
     - Encontros e conferências.
     - Comunidade muito ativa.
     - Livros recentes publicados.
- Uso na indústria: Prezi, TruQu, No Red Ink ...

Elm
====

- JavaScript e Elm são usadas para webapps
     - JavaScript é executada pelo browser, Elm
       compila para JavaScript.
     - JavaScript manipula o DOM diretamente, Elm não.
     - JavaScript é não tipada e insegura; Elm é
       estaticamente tipada e robusta.

Elm
====

- Apps Elm seguem o padrão MVC.
     - Model: descreve o estado atual da aplicação.
     - View: descreve como gerar o HTML a partir do estado.
     - Controller: descreve como a interação do usuário atualiza o modelo.


Hello world!
==============

```elm
type alias Model = String

initialModel : Model
initialModel = "Hello world!"

view : Model -> Html
view m = div [ id "content"]
             [ h1 [] [ text "Hello world!" ]
             , p [] [ text m ]
             ]

main :: Html
main = view initialModel
```

Html?
======

- Elm possui uma biblioteca expressiva para
construção dinâmica de páginas.

- Uma função para cada tag Html: div, h1, img, etc.

- Tags customizáveis por atributos.
     - Cada tag recebe uma lista de tais atributos.

- Tags podem ter uma lista de filhos.

Tipos em Elm
===============

- Elm possui diversos tipos básicos: Bool, String, Int, Float, ...

- Listas, tuplas.

- Row types.

Elm
===

- Instalar?

- Veja como obter a linguagem para sua plataforma na página
http://elm-lang.org


Compilar?
============

```
> elm make hello.elm
Success! Compiled 1 module.

> firefox index.html
...
```

Nada demais
==============

- Hello world não convence ninguém...

- Vamos considerar agora duas pequenas
aplicações interativas.

Invertendo strings
==================

- Modelo

```elm
type alias Model = String

initialModel : Model
initialModel = "Hello World!"
```

Invertendo strings
==================

- Visão

```elm
view : Model -> Html msg
view m = div [ id "content"]
             [ h1 myStyle
                  [ text "My first Elm app" ]
                  , p myStyle
                      [ text (String.reverse m) ]
             ]
```

Invertendo strings
==================

- CSS

```elm
style1 : (String, String) -> Attribute msg
style1 p = style (first p) (second p)

myStyle = List.map style1
            [ ("width", "100%")
            , ("height", "40px")
            , ("padding", "10px 0")
            , ("font-size", "2em")
            , ("text-align", "center") ]
```

Boring...
==========

- Até agora, nenhuma interação com o usuário.

- Vamos adicionar interação e outro elemento do
MVC: o controlador.

Invertendo texto
=================

- Vamos adicionar um campo de texto.

- Digitar texto irá gerar um evento que
atualizará o modelo, usando o controlador.

- Modelo atualizado leva a atualização
na visão.

Invertendo texto
================

- Incluindo o text field

```elm
view : Model -> Html msg
view m = div [ id "content"]
             [ h1 myStyle
                  [ text "My first Elm app" ]
             , input ([ placeholder "Digite aqui"
                      , onInput identity ] ++ myStyle)
             , p myStyle
                 [ text m ]
             ]
```

Invertendo o texto
==================

- Tratamento de evento.

```elm
onInput identity
```

Executa a função identidade sobre a string digitada e a
repassa ao controlador.

Invertendo texto
================

- Controlador

```elm
type alias Msg = String

update : Msg -> Model -> Model
update msg m = String.reverse m
```

Invertendo texto
=================

- Agrupando os componentes do MVC na função main.

```elm
main =
  Browser.sandbox { init = "", update = update, view = view }
```

Contador
=========

- Outra aplicação simples: Página contendo botões para incrementar e
decrementar um inteiro.

- Modelo: valor inteiro

```elm
type alias Model = Int
```

Contador
========

- Usaremos um tipo algébrico para representar mensagens de
incremento e decremento.

```elm
type Msg = Increment | Decrement
```

Contador
========

- Controlador modifica o modelo de acordo com a mensagem
recebida da interface gráfica

```elm
update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1
    Decrement ->
      model - 1
````

Contador
========

- Visão: associa a cada botão a sua respectiva mensagem ao executar
o evento on click.

```elm
view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
````

Formulário
===========

- Exemplo: Formulário para validar senha.
    - Senha e sua confirmação devem ser iguais.
- Modelo.

```elm
type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }
```

Formulário
===========

- Estado inicial do modelo.

```elm
init : Model
init = Model "" "" ""
````

Formulário
==========

- Mensagens armazenam os valores recebidos da
interface.

```elm
type Msg
  = Name String
  | Password String
  | PasswordAgain String
```

Formulário
==========

- Visão: Função auxiliar para formatar os campos de texto.

```elm
viewInput : String ->
            String ->
            String ->
            (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
````

Formulário
==========

- Validação: Verifica se as senhas são iguais.

```elm
viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ]
        [ text "Passwords do not match!" ]
```

Formulário
==========

- Visão

```elm
view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password"
                "Re-enter Password"
                 model.passwordAgain PasswordAgain
    , viewValidation model
    ]
```

Porquê Elm?
============

- Elm é uma linguagem robusta. De acordo com a No Red Ink, esta
possui uma aplicação com mais 80.000 linhas de código em produção
desde 2015.

- Nenhum falha em tempo de execução.

Porquê Elm?
===========

- O runtime da linguagem é capaz de calcular alterações mínimas ao DOM.
- Isso torna o seu uso muito eficiente quando comparado a código JS feito
por programadores experientes.

Porquê Elm?
===========

- Manter software expresso em Elm é fácil.

- Deseja modificar o HTML? Mude a função da visão.

Porquê Elm?
===========

- Deseja modificar o estilo da página? Modifique o código relativo ao CSS.

- Deseja adicionar um botão? Adicione uma nova mensagem, extenda a visão
com o novo botão e adicione o código para a mensagem no controlador.

Porquê Elm?
===========

- Elm possui uma ampla gama de biblioteca para os mais variados fins.

- Elm possui uma biblioteca que integra a linguagem ao ReactNative.
    - Permite desenvover apps Android e iOS.

Concluindo
===========

- Elm é uma alternativa segura e robusta ao JavaScript.

- Linguagem fortemente tipada e pura, sem efeitos colaterais.
    - Sem usar mônadas como Haskell.
