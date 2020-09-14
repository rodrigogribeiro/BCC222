module Exp where

import Test.QuickCheck

-- expressões

data Exp
   = Const Int
   | Exp :+: Exp
     deriving Show


eval :: Exp -> Int
eval (Const n) = n
eval (e :+: e')
   = eval e + eval e'

-- máquina virtual

data Instr
    = Push Int
    | Add
    deriving Show

type Code = [Instr]
type Stack = [Int]

exec :: Code -> Stack -> Stack
exec [] s = s
exec ((Push n) : c) s = exec c (n : s)
exec (Add : c) (n : m : s) = exec c (n + m : s)

-- compilador

compile :: Exp -> Code
compile (Const n)
  = [Push n]
compile (e :+: e')
  = compile e' ++ compile e ++ [Add]

-- gerando expressões

instance Arbitrary Exp where
    arbitrary = sized genExp
      where
        genExp n
          | n <= 1 = Const <$> arbitrary
          | otherwise
             = frequency
                  [
                    (30, Const <$> arbitrary)
                  , (70, (:+:) <$> genExp n2 <*> genExp n2)]
               where n2 = n `div` 2

-- propriedade: correção do compilador

compileOk :: Exp -> Stack -> Bool
compileOk e s
  = eval e : s == exec (compile e) s
