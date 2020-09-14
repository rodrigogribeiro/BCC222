module Utils.Pretty ( Doc
                    , empty
                    , char
                    , text
                    , number
                    , string
                    , line
                    , fold
                    , hcat
                    , fsep
                    , punctuate
                    , enclose
                    , series
                    , fill
                    , (</>)
                    , softline
                    , flatten
                    , compact
                    , pretty
                    , nest) where


-- simple pretty-printing library

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Eq, Show)


-- interface to Doc type

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s  = Text s

number :: Int -> Doc
number = Text . show

line :: Doc
line = Line

-- Doc is a monoid

instance Semigroup Doc where
  Empty <> y = y
  x <> Empty = x
  x <> y     = Concat x y

instance Monoid Doc where
  mempty = Empty

-- derived operators

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []
  = []
punctuate _ [d]
  = [d]
punctuate p (d : ds)
  = (d <> p) : punctuate p ds

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item
  = enclose open close . fsep . punctuate (char ',') . map item

enclose :: Char -> Char -> Doc -> Doc
enclose open close d
  = char open <> d <> char close

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group d = flatten d `Union` d

flatten :: Doc -> Doc
flatten (x `Concat` y)
  = flatten x `Concat` flatten y
flatten Line
  = Char ' '
flatten (x `Union` _)
  = flatten x
flatten d
  = d

string :: String -> Doc
string = enclose '"' '"' . text

-- compact rendering

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

-- pretty printing

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col



fits :: Int -> String -> Bool
fits w _ | w < 0  = False
fits _ ""         = True
fits _ ('\n' : _) = True
fits w (_ : cs)   = fits (w - 1) cs


fill :: Int -> Doc -> Doc
fill n d
  = walk 0 [d]
  where
    complete col
      = Text (replicate (n - col) ' ')
    walk col []
      = complete col
    walk col (d1 : ds)
      = case d1 of
          Empty  ->
            Empty <> walk col ds
          Char c ->
            Char c <> walk (col + 1) ds
          Text s ->
            Text s <> walk (col + length s) ds
          Line   ->
            complete col <> Line <> walk 0 ds
          Concat d1' d2 ->
            walk col (d1' : d2 : ds)
          Union d1' d2 ->
            walk col (d1' : ds) `Union` walk col (d2 : ds)


nest :: Int -> Doc -> Doc
nest n
  = snd . walk 0
  where
    isOpen c = c `elem` "({["
    isClose c = c `elem` ")}]"
    walk lvl d@(Char c)
      | isOpen c = (lvl + 1, d)
      | isClose c = (lvl - 1, d)
      | otherwise = (lvl, d)
    walk lvl Line
      | lvl <= 0 = (lvl, Line)
      | otherwise = (lvl, Line `Concat` Text (replicate (n * lvl) ' '))
    walk lvl (Concat d1 d2)
      = case walk lvl d1 of
          (lvl1, d1') ->
            case walk lvl1 d2 of
              (lvl2, d2') -> (lvl2, Concat d1' d2')
    walk lvl (Union d1 d2)
      = (lvl, snd (walk lvl d1) `Union` snd (walk lvl d2))
    walk lvl d = (lvl,d)
