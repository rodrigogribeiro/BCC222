module KMeans1 where

import qualified Data.Map as M
import Data.List
import KMeansState
import Vector

-- cluster phase

clusterPhase :: Vectorizable e v =>
                KMeansState e v ->
                M.Map v [e]
clusterPhase state
  = let
      initial = M.fromList $ zip (_centroids state)
                                 (repeat [])
      step p m = let
                  chosen = minimumBy (compareDistance p)
                                     (_centroids state)
                 in M.adjust (p :) chosen m
      compareDistance p x y
        = compare (distance x $ toVector p)
                  (distance y $ toVector p)
    in foldr step initial (_points state)

-- kmeans definition

kMeans :: Vectorizable e v    =>
          (Int -> [e] -> [v]) ->
          Int                 ->
          [e]                 ->
          Double              ->
          [v]
kMeans i n pts t
  = _centroids (kMeans' (initialState i n pts t))


kMeans' :: Vectorizable e v =>
           KMeansState e v  ->
           KMeansState e v
kMeans' state
  = let
      genCentroids = centroid . map toVector
      clusters = clusterPhase state
      new = snd <$> (M.toList $ genCentroids <$> clusters)
      -- state update 1
      state1 = state { _centroids = new }
      n_err = sum $ zipWith distance (_centroids state)
                                     (_centroids state1)
      -- state update 2
      state2 = state1 { _err = n_err }
      -- state update 3
      state3 = state2 { _steps = (_steps state2) + 1 }
    in if (_err state3) < (_limit state3)
       then state3
       else kMeans' state3


initialize :: Int -> [e] -> [(Double,Double)]
initialize 0 _  = []
initialize n vs = (m,m) : initialize (n - 1) vs
  where
    m = fromIntegral n

info :: [(Double, Double)]
info = [(1,1),(1,2),(4,4),(4,5)]


test :: [(Double, Double)]
test = kMeans initialize 2 info 0.001
