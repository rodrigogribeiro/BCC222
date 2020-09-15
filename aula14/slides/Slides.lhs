---
title: Manipulando JSON
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

JSON
====

- JavaScript Object Notation.

- Formato simples para representação
estrutura de informação.

- Utilizado em aplicações REST.

JSON
====

- Quatro tipos básicos de valores: números,
strings, booleanos e `null`.

- Arranjos: sequência ordenada de valores.

- Object: sequência não ordenada de pares
chave/valor.
    - Chaves: Strings quaisquer
    - Valor: qualquer JSON válido.


Objetivo
========

- Implementar uma biblioteca
para manipulação de JSON em Haskell.

- Combinaremos diversos tópicos para isso:
Parsing, tipos de dados algébricos, lenses e
classes de tipos e QuickCheck.

Representação
=============

- Valores JSON:

```haskell
data JSON
  = JString String
  | JNumber Int
  | JBool Bool
  | JNull
  | JObject [(String, JSON)]
  | JArray [JSON]
```

Lenses
======

- Cada construtor possui um lens associado.

```haskell
getBool :: Lens JSON (Maybe Bool)
getBool
  = lens get_bool set_bool
  where
    get_bool (JBool b)
      = Just b
    get_bool _
      = Nothing

    set_bool (Just d) (JBool _)
      = JBool d
    set_bool _ d
      = d
```

Lenses
======

- Acesso e alteração em array

```haskell
at_ :: Int -> [a] -> Maybe a
at_ n xs
  | n < 0  = Nothing
  | otherwise = f n xs
  where
    f 0 (x : _) = Just x
    f i (_ : xs') = f (i - 1) xs'
    f _ []       = Nothing
```

Lenses
======

- Acesso e alteração em array

```haskell
sat_ :: Int -> Maybe a -> [a] -> [a]
sat_ _ Nothing xs = xs
sat_ n (Just v) xs
  | n < 0 = xs
  | otherwise = f n v xs
  where
    f 0 y (_ : ys) = y : ys
    f i y (z : zs) = z : f (i - 1) y zs
    f _ _ _ = []
```

Lenses
======

- Acesso e alteração em array

```haskell
listAt :: Int -> Lens [a] (Maybe a)
listAt idx
  = lens (at_ idx) (sat_ idx)
```

Printing
========

- Como gerar uma string correspondente a
um valor JSON?

- Simples! Basta definir uma função:

```haskell
printJSON :: JSON -> String
```

Printing
========

- Valores básicos são imediatos.

```haskell
printJSON :: JSON -> String
printJSON (JString s) = show s
printJSON (JNumber n) = show n
printJSON (JBool True) = "true"
printJSON (JBool False) = "false"
```

Printing
========

- Intercalar um separador em uma lista

```haskell
intercalate :: a -> [a] -> [a]
intercalate _ [] = []
intercalate _ [x] = [x]
intercalate y (x : xs)
  = x : y : (intercalate y xs)
```

Printing
========

- Arrays

```haskell
printJSON (JArray arr)
  = "[" ++ values arr ++ "]"
  where
    values [] = ""
    values vs = concat $ intercalate "," arr'
    arr' = map printJSON arr
```

Printing
========

- Objetos
    - Idealmente, deveríamos ser capazes
      de imprimir objetos aninhados.
    - Facilita a depuração em caso de
      problemas.

Printing
========

- Abordagem atual é limitante...
    - Como imprimir de forma legível?
    - Como imprimir de forma "compacta"?
- Vamos generalizar o problema de
impressão.

Documentos
==========

- Representando documentos.

```haskell
data Doc
  = Empty            -- documento vazio
  | Char Char        -- um caractere
  | Text String      -- string
  | Line             -- quebra de linha
  | Concat Doc Doc   -- concatenação
  | Union Doc Doc    -- união
```

Documentos
==========

- Representar documentos como um
tipo de dados, permite diferentes
estratégias para renderizá-los.

- Estratégias diferentes são
apenas funções.

Documentos
==========

- Criando documentos: `Doc` é um TAD.
   - Construtores não são expostos.

```haskell
empty :: Doc
empty = Empty

text :: String -> Doc
text "" = Empty
text s  = Text s

number :: Int -> Doc
number = Text . show
```

Documentos
==========

- `Doc` é um monóide.

```haskell
instance Semigroup Doc where
  Empty <> y = y
  x <> Empty = x
  x <> y     = Concat x y

instance Monoid Doc where
  mempty = Empty
```

Documentos
==========

- Rederização compacta

```haskell
compact :: Doc -> String
compact
  = transform . wrap
    where
      wrap d = [d]
      transform [] = ""
      transform (d : ds)
        = case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line   -> '\n' : transform ds
            d1 `Concat` d2 -> transform (d1 : d2 : ds)
            _ `Union` d ->  transform (d : ds)
```

Documentos
==========

- Implementação no módulo Utils.Pretty.

- Diversas funcionalidades.

- Algoritmo para formatação legível
  implementado como parte da biblioteca.

Parsing
=======

- Conversão de strings em JSON correspondente.

- Utilizamos a biblioteca de parsing
implementada em aulas anteriores.

Parsing
=======

- Função principal

```haskell
jsonParser :: Parser Char JSON
jsonParser
  = nullParser   <|>
    boolParser   <|>
    stringParser <|>
    numberParser <|>
    arrayParser  <|>
    objectParser  
```

Parsing
=======

- Removendo espaços em branco

```haskell
spaces :: Parser Char String
spaces = greedy (sat isSpace)

lexer :: String -> Parser Char String
lexer s
  = f <$> spaces <*> token s
   where
     f _ x = x
```

Parsing
=======

- Parsers básicos

```haskell
boolParser :: Parser Char JSON
boolParser
  = trueParser <|> falseParser
    where
      trueParser
        = JBool True <$ lexer "true"
      falseParser
        = JBool False <$ lexer "false"
```

Parsing
=======

- Parsing para arrays

```haskell
arrayParser :: Parser Char JSON
arrayParser
  = JArray <$> (ob *> spaces
                   *> elements
                   <* spaces
                   <* cb)
    where
      ob = symbol '['
      cb = symbol ']'
      elements = listOf jsonParser comma
      comma = spaces *> symbol ',' <* spaces
```

Testes
=======

- Como garantir a correção?
   - Parsing e impressão devem ser funções inversas.
- Vamos usar QuickCheck para garantir essa propriedade.


Geradores
=========

- Configuração do gerador

```haskell
data Config
  = Config {
      elements :: Int -- maximum element number in array
    , depth    :: Int -- maximum nesting in objects
    , fields   :: Int -- maximum of fields
    }
```

Geradores
=========

- Geração de configurações

```haskell
instance Arbitrary Config where
  arbitrary
    = do
        n <- choose (1,5)
        d <- choose (1,2)
        f <- choose (1,5)
        return (Config n d f)
```

Geradores
=========

- Gerando JSON

```haskell
instance Arbitrary JSON where
  arbitrary
    = do
        cfg <- arbitrary :: Gen Config
        generateJSON cfg
```

Geradores
=========

- Gerando JSON

```haskell
generateJSON :: Config -> Gen JSON
generateJSON cfg
  | depth cfg <= 1
    = frequency
      [
        (1, return JNull)
      , (9, JBool <$> arbitrary)
      , (10, JNumber <$> generateNumber)
      , (10, JString <$> generateStringLit cfg)
      ]
-- ... continua
```

Geradores
=========

- Gerando JSON

```haskell
  | otherwise
    = frequency
      [
        (1, return JNull)
      , (9, JBool <$> arbitrary)
      , (10, JNumber <$> generateNumber)
      , (10, JString <$> generateStringLit cfg')
      , (35, JArray <$> generateArray cfg')
      , (35, JObject <$> generateObject cfg')
      ]
    where
      cfg' = decrease cfg
```

Geradores
=========

- Gerando JSON

```haskell
generateStringLit :: Config -> Gen String
generateStringLit cfg
  = vectorOf (elements cfg)
             (choose ('a', 'z'))

generateArray :: Config -> Gen [JSON]
generateArray cfg
  = vectorOf (elements cfg)
             (generateJSON cfg)
```

Propriedade
===========

- `parse . pretty = id`

```haskell
roundTrip :: Property
roundTrip
  = forAll (arbitrary :: Gen JSON)
       (\ json ->
          let
            str = prettyJSON json
          in case parseJSON str of
               Left _ -> False
               Right json' -> json == json')
```

Resultado
=========

```
Library tests
  Round trip: OK
    +++ OK, passed 100 tests.

All 1 tests passed (0.00s)
```

The Truth
==========

- Durante a implementação, essa propriedade
permitiu:
   - Descobrir um bug na lib de parsing.
   - Descobrir bugs na serialização de JSON
