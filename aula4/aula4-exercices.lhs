Exercícios
==========

O objetivo destes exercícios é a definição
de algumas funções usando casamento de padrão e
funções de ordem superior sobre listas.

Descrição
---------

Vamos considerar os seguintes tipo de dados
para representar clientes da aplicação de
venda de cervejas:

> newtype Name = Name { unName :: String }
>                deriving (Show, Eq, Ord)

                 derivação automática de
                 conversão para string, testes
                 de igualdade e comparação.

> newtype Surname = Surname { unSurname :: String }
>                   deriving (Show, Eq, Ord)

Neste modelo, definimos os tipos Name e Surname usando a
construção newtype. Quando utilizamos newtype, assim como
data, estamos criando um novo tipo de dados. Isso implica
que o valor

Name "Maria"

é diferente da string "Maria". Usando newtypes, podemos
diferenciar entre nome e sobrenome de clientes no modelo.

Outra alteração que faremos no modelo é a de incluir
informações sobre endereço. Para isso, vamos criar
diversos novos tipos para modelar os componentes de um
endereço.

Por questão de simplicidade, vamos representar os diferentes
componentes de um endereço como registros que armazenam
strings usando a construção newtype.

> newtype Street = Street { unStreet :: String }
>                  deriving (Eq, Ord, Show)

> newtype Number = Number { unNumber :: String }
>                  deriving (Eq, Ord, Show)

> newtype District = District {unDistrict :: String }
>                    deriving (Eq, Ord, Show)

> newtype City = City { unCity :: String }
>                     deriving (Eq, Ord, Show)

> newtype Province = Province { unProvince :: String }
>                    deriving (Eq, Ord, Show)

> data Address
>    = Address {
>        street   :: Street   -- rua
>      , number   :: Number   -- número
>      , district :: District -- bairro
>      , city     :: City     -- cidade
>      , province :: Province -- estado
>      } deriving (Eq, Ord, Show)

Outra informação relevante em nossa aplicação é o e-mail
de contato. Representaremos a informação de e-mail usando
o seguinte tipo de dados.

> newtype Email = Email { unEmail :: String }
>                 deriving (Eq, Ord, Show)

A seguir, usando os tipos para nome, sobrenome e e-mail,
podemos definir o tipo Person:

> data Person
>    = Person {
>        name    :: Name
>      , surname :: Surname
>      , email   :: Email
>      } deriving (Show, Eq, Ord)

Finalmente, usando esses tipos, podemos definir o
tipo de dados para clientes como apresentado a seguir.

> data Client
>    = Company {
>        companyId      :: Int
>      , companyName    :: Name
>      , contact        :: Person
>      , companyAddress :: Address
>      }
>    | Individual {
>        individualId      :: Int
>      , individual        :: Person
>      , individualAddress :: Address
>      } deriving (Eq, Ord, Show)

Ao final deste arquivo, existe uma constante chamada
database que define uma lista de clientes que deverá
ser utilizada por você como caso de testes para os
exercícios a serem desenvolvidos.

Exercícios
----------

O objetivo desses exercícios é a criação de algumas
funções para manipulação de dados de clientes na
aplicação de venda de cervejas.

1. Desenvolva a função

> clientEmail :: Client -> Email
> clientEmail = _

que recupera o e-mail de um cliente fornecido como parâmetro.
Sua função deve ser desenvolvida usando casamento de padrão.

Para a criação de mensagens de e-mails, precisamos de
templates que são especificados por um tipo de documentos:

> data Doc
>    = Empty           -- string vazia
>    | Char Char       -- string formada por um caractere
>    | Text String     -- string
>    | Line            -- quebra de linha
>    | Concat Doc Doc  -- concatenação de documentos
>    | Nest Int Doc    -- Nest n d identa o documento d em n espaços
>    deriving (Eq, Ord, Show)

O uso deste tipo permite a construção modular de mensagens de
texto padronizadas.

2. Usando o tipo de dados Doc e o tipo Client, construa uma função
que constrói a seguinte mensagem de feliz aniversário para o cliente
José Pereira:

Prezado Sr. José Pereira,

Nós da distribuidora de cervejas BCC222
desejamos um feliz aniversário! Aproveite
para fazer um pedido de uma cerveja nessa
data especial!

Atenciosamente,

Distribuidora de Cervejas BCC222

> birthdayMessage :: Client -> Doc
> birthdayMessage = _

3. Um componente importante para geração
de mensagens é a conversão de valores de
tipo Doc em Strings. Para isso, implemente
a função

> render :: Doc -> String
> render = _

que converte um valor de tipo Doc em sua
respectiva String.

4. Outra tarefa importante do sistema é verificar se
um determinado cliente possui cadastro no sistema.
Para isso, implemente a seguinte função:

> findBy :: (a -> Bool) -> [a] -> Maybe a
> findBy = foldr step base
>          where
>            step = _
>            base = _

que a partir de um predicado --- função de tipo
a -> Bool --- e uma lista de valores de tipo
a, retorna Nothing caso não exista nenhum valor
que atenda o predicado ou o primeiro elemento a
satisfazê-lo.

Note que, obrigatoriamente, sua
implementação deverá utilizar o "template" para
funções sobre listas usando foldr.

Finalmente, de posse da função findBy, implemente
a função:

> findByName :: Name -> Maybe Client
> findByName = _

5. Outro componente da aplicação proposta envolve
a geração de uma lista contendo todos os nomes,
sobrenomes e e-mail de contato de clientes.
Para isso, implemente a seguinte função:

> contactList :: [Client] -> [(Name, Surname, Email)]
> contactList = foldr step base
>               where
>                 step = _
>                 base = _

Novamente, sua definição deve utilizar o template
para "foldr" fornecido.

6. Outra funcionalidade da aplicação é tentar
otimizar as entregas da distribuidora de bebidas.
Para isso, vamos criar funções que permitem agrupar
clientes por bairro ou cidade.

Os próximos exercícios visam implementar essa
funcionalidade.

a) O primeiro passo para implementarmos a
funcionalidade de agrupar clientes por bairro ou
cidade é identificar o bairro ou cidade de um
certo cliente. Para isso, implemente as seguintes
funções utilizando apenas composição:

> districtClient :: Client -> District
> districtClient = _

> cityClient :: Client -> City
> cityClient = _

b) Implemente a função

> groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
> groupBy = _

que agrupa, em uma mesma lista, elementos considerados
iguais por um critério de comparação implementado como
uma função de tipo a -> a -> Bool.

c) Utilizando as funções anteriores, implemente as
funções:

> groupByCity :: [Client] -> [[Client]]
> groupByCity = _

> groupByDistrict :: [Client] -> [[Client]]
> groupByDistrict = _

que agrupam clientes que residem em uma mesma cidade
ou bairro, respectivamente.


O "banco de dados" da aplicação
------------------------------

A seguinte lista funciona como caso de teste para
as funções destes exercícios.

> database :: [Client]
> database = [ Individual 1 (Person (Name "Jose")
>                                   (Surname "Pereira"))
>                           (Address (Street "R. de cima")
>                                    (Number "111")
>                                    (District "Alvorada")
>                                    (City "São Manoel do Lado")
>                                    (Province "Minas Gerais"))
>            , Individual 2 (Person (Name "Carlos")
>                                   (Surname "Silva"))
>                           (Address (Street "R. de baixo")
>                                    (Number "222")
>                                    (District "Cordilheira")
>                                    (City "Lagoa Cheia")
>                                    (Province "Minas Gerais"))
>            , Company    3 (Name "Bar da Manoel")
>                           (Person (Name "Manoel")
>                                   (Surname "do Bar"))
>                           (Address (Street "R. 12")
>                                    (Number "333")
>                                    (District "Lagoa")
>                                    (City "São Manoel do Lado")
>                                    (Province "Minas Gerais"))
>            , Company    4 (Name "Restaurante do Joaquim")
>                           (Person (Name "Joaquim")
>                                   (Surname "Souza"))
>                           (Address (Street "R. Pedro Silva")
>                                    (Number "444")
>                                    (District "Alumínio")
>                                    (City "Batista")
>                                    (Province "Minas Gerais"))
>            , Individual 5 (Person (Name "Bill")
>                                   (Surname "Gates"))
>                           (Address (Street "Microsoft")
>                                    (Number "555")
>                                    (District "Microsoft District")
>                                    (City "City of all bugs")
>                                    (Province "Nothing works"))
>            , Individual 6 (Person (Name "João")
>                                   (Surname "Dinheiro"))
>                           (Address (Street "R. das Nuvens")
>                                    (Number "666")
>                                    (District "Alvorada")
>                                    (City "São Manoel do Lado")
>                                    (Province "Minas Gerais"))
>            , Company    7 (Name "Pizzaria da Maria")
>                           (Person (Name "Maria")
>                                   (Surname "Terra"))
>                           (Address (Street "R. de cima")
>                                    (Number "777")
>                                    (District "Alvorada")
>                                    (City "São Manoel do Lado")
>                                    (Province "Minas Gerais"))]
