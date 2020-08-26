module KMeans4 where

import Data.List
import qualified Data.Map as M
import Vector
import Control.Monad.State

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
  = gets centroids                           >>= (\ prevs ->
    pure (clusterPhase prevs points)         >>= (\ asgn ->
    pure (newCentroids asgn)                 >>= (\ news ->
    modify (\s -> s{centroids = news})       >>= (\ _ ->
    modify (\s -> s{steps = steps s + 1})    >>= (\ _ ->
    gets limit                               >>= (\ lim ->
    pure (sum $ zipWith distance prevs news) >>= (\ err ->
    if err < lim then pure news else kMeans' points)))))))
