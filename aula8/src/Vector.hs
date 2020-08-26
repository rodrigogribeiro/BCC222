{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vector where


type Point = (Double, Double)

-- type class for vectors

class Ord v => Vector v where
  -- calculating distance between two points
  distance :: v -> v -> Double
  centroid :: [v] -> v

-- instance for Euclidean distance

instance Vector Point where
  distance (x1,y1) (x2,y2)
    = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

  centroid xs
    = let
        (a,b) .+. (c,d) = (a + c, b + d)
        (u,v) = foldr (.+.) (0,0) xs
        n = fromIntegral $ length xs
      in (u / n, v / n)


-- type class to convert things to vectors

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable Point Point where
  toVector = id

