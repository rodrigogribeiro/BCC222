{-# LANGUAGE RankNTypes #-}

module Data.JSON ( JSON (..)
                 , getString
                 , getNumber
                 , getBool
                 , isNull
                 , at
                 , lookupJSON) where

import Control.Applicative (liftA2)
import Data.Char
import Utils.Lens
import Test.QuickCheck hiding (elements)

-- representing JSON

data JSON
  = JString String
  | JNumber Int
  | JBool Bool
  | JNull
  | JObject [(String, JSON)]
  | JArray [JSON]
  deriving (Eq, Ord, Show)

-- simple lenses for JSON manipulation

getString :: Lens JSON (Maybe String)
getString
  = lens get_str set_str
    where
      get_str (JString s) = Just s
      get_str _ = Nothing

      set_str (Just s) (JString _)
        = JString s
      set_str _ d = d


getNumber :: Lens JSON (Maybe Int)
getNumber
  = lens get_int set_int
    where
      get_int (JNumber d)
        = Just d
      get_int _
        = Nothing

      set_int (Just d) (JNumber _)
        = JNumber d
      set_int _ d
        = d

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

isNull :: JSON -> Bool
isNull v = v == JNull

-- array lens

at_ :: Int -> [a] -> Maybe a
at_ n xs
  | n < 0  = Nothing
  | otherwise = f n xs
  where
    f 0 (x : _) = Just x
    f i (_ : xs') = f (i - 1) xs'
    f _ []       = Nothing

sat_ :: Int -> Maybe a -> [a] -> [a]
sat_ _ Nothing xs = xs
sat_ n (Just v) xs
  | n < 0 = xs
  | otherwise = f n v xs
  where
    f 0 y (_ : ys) = y : ys
    f i y (z : zs) = z : f (i - 1) y zs
    f _ _ _ = []

listAt :: Int -> Lens [a] (Maybe a)
listAt idx
  = lens (at_ idx) (sat_ idx)


at :: Int -> Lens JSON (Maybe JSON)
at idx
  = lens nth seth
    where
      nth (JArray arr) = at_ idx arr
      nth _ = Nothing

      seth json (JArray arr)
        = JArray ((listAt idx) ~. json $ arr)
      seth _ d  = d

-- object lens

lookupL :: Eq a => a -> Lens [(a,b)] (Maybe b)
lookupL k
  = lens (lookup k) setL
    where
      setL (Just v) ((k',v') : ks)
        | k == k'   = (k',v) : ks
        | otherwise = (k',v') : setL (Just v) ks
      setL _ ks = ks

lookupJSON :: String -> Lens JSON (Maybe JSON)
lookupJSON s
  = lens get set_
  where
    get (JObject fs)
      = lookup s fs
    get _ = Nothing

    set_ json (JObject fs)
      = JObject ((lookupL s) ~. json $ fs)
    set_ _ json = json


-- generator for JSON

data Config
  = Config {
      elements :: Int -- maximum element number in array
    , depth    :: Int -- maximum nesting in objects
    , fields   :: Int -- maximum of fields
    }

instance Arbitrary Config where
  arbitrary
    = do
        n <- choose (1,5)
        d <- choose (1,5)
        f <- choose (1,5)
        return (Config n d f)

instance Arbitrary JSON where
  arbitrary
    = do
        cfg <- arbitrary :: Gen Config
        generateJSON cfg

decrease :: Config -> Config
decrease cfg
  = cfg {depth = (depth cfg) - 1}

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

generateNumber :: Gen Int
generateNumber
  = suchThat (arbitrary :: Gen Int)
             (>= 0)

generateStringLit :: Config -> Gen String
generateStringLit cfg
  = vectorOf (elements cfg)
             (choose ('a', 'z'))

generateArray :: Config -> Gen [JSON]
generateArray cfg
  = vectorOf (elements cfg)
             (generateJSON cfg)

generateObject :: Config -> Gen [(String, JSON)]
generateObject cfg
  = do
      let d = depth cfg
      m <- choose (1, fields cfg)
      vectorOf m (fieldGen cfg)

fieldGen :: Config -> Gen (String, JSON)
fieldGen cfg
  = liftA2 (,) (generateStringLit cfg)
               (generateJSON cfg)
