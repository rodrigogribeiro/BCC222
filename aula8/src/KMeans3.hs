module KMeans3 where

import Data.List
import qualified Data.Map as M
import Vector
import CombinatorsState

-- new kmeans state

data KMeansState v
  = KMeansState {
      centroids :: [v]
    , limit     :: Double
    , steps     :: Int
    }

-- centroids function

newCentroids :: Vectorizable e v =>
                M.Map v [e]      ->
                [v]
newCentroids
  = M.elems . fmap (centroid . fmap toVector)

-- cluster phase

clusterPhase :: Vectorizable e v =>
                [v]              ->
                [e]              ->
                M.Map v [e]
clusterPhase centrs points
  = let
      initial = M.fromList $ zip centrs (repeat [])
      compareDistance p x y
        = compare (distance x $ toVector p)
                  (distance y $ toVector p)
      step p m
        = let
            chosen = minimumBy (compareDistance p)
                               centrs
          in M.adjust (p :) chosen m
    in foldr step initial points

-- definition of the kmeans using combinators

kMeans' :: Vectorizable e v =>
           [e]              ->
           State (KMeansState v) [v]
kMeans' points
  = access centroids                       `thenDo` (\ prevs ->
    remain (clusterPhase prevs points)     `thenDo` (\ asgn ->
    remain (newCentroids asgn)             `thenDo` (\ news ->
    modify (\s -> s{centroids = news})     `thenDo` (\ _ ->
    modify (\s -> s{steps = steps s + 1})  `thenDo` (\ _ ->
    access limit                           `thenDo` (\ lim ->
    remain (sum $ zipWith distance prevs news) `thenDo` (\ err ->
    if err < lim then remain news else kMeans' points)))))))
