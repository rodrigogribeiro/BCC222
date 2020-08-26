module KMeans where

import Data.List
import qualified Data.Map as M
import Vector


-- cluster assignment phase

clusterPhase :: Vectorizable e v => [v] -> [e] -> M.Map v [e]
clusterPhase centroids points
  = let
      initialMap = M.fromList $ zip centroids (repeat [])
    in foldr step initialMap points
  where
    step p m = let chosen = minimumBy (compareDistance p)
                                      centroids
                in M.adjust (p :) chosen m
    compareDistance p x y = compare (distance x $ toVector p)
                                    (distance y $ toVector p)

-- calculating new centroids

newCentroidPhase :: ( Vector v
                    , Vectorizable e v) =>
                    M.Map v [e]         ->
                    [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

-- stop condition

shouldStop :: Vector v => [(v,v)] -> Double -> Bool
shouldStop centroids limit
  = foldr (\ (x,y) s -> s + distance x y) 0.0 centroids < limit

-- K Means function

kMeans :: Vectorizable e v    =>
          (Int -> [e] -> [v]) -> -- initialization function
          Int                 -> -- number of centroids
          [e]                 -> -- data
          Double              -> -- limit
          [v]                    -- final centroids
kMeans i k points
  = kMeans' (i k points) points


kMeans' :: Vectorizable e v =>
           [v]              ->
           [e]              ->
           Double           ->
           [v]
kMeans' centroids points limit
  = let
      clusters = clusterPhase centroids points
      oldCentroids = newCentroidPhase clusters
      newCentroids = snd <$> oldCentroids
    in if shouldStop oldCentroids limit
       then newCentroids
       else kMeans' newCentroids points limit


-- simple utilities to test the algorithm

initialize :: Int -> [e] -> [(Double,Double)]
initialize 0 _  = []
initialize n vs = (m,m) : initialize (n - 1) vs
  where
    m = fromIntegral n

info :: [(Double, Double)]
info = [(1,1),(1,2),(4,4),(4,5)]


test :: [(Double, Double)]
test = kMeans initialize 2 info 0.001
