{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Math.KMeans
Copyright   :  (c) Alp Mestanogullari, 2011
License     :  MIT
Maintainer  :  alpmestan@gmail.com
Stability   :  experimental

An implementation of the k-means clustering algorithm based on the efficient vector package.

-}

module Math.KMeans where

import qualified Data.Vector.Unboxed as V
import qualified Data.List as L
import Data.Function (on)

--- * K-Means clustering algorithm

type Vec = V.Vector Double
type Cluster = Vec

distance :: Vec -> Vec -> Double
distance u v = V.sum $ V.zipWith (\a b -> (a - b)^2) u v

partitionPoints :: Int -> [Vec] -> [[Vec]]
partitionPoints k vs = go vs
  where go vs = case L.splitAt n vs of
          (vs', []) -> [vs']
          (vs', vss) -> vs' : go vss
        n = (length vs + k - 1) `div` k
        
computeClusters :: [[Vec]] -> [Cluster]
computeClusters = map f
  where f (x:xs) = let (n, v) = L.foldl' (\(k, s) v' -> (k+1, V.zipWith (+) s v')) (1, x) xs
                   in V.map (\x -> x / (fromIntegral n)) v

regroupPoints :: [Cluster] -> [Vec] -> [[Vec]]
regroupPoints clusters points = go points
  where go = map (map snd) . L.groupBy (\(a,b) (c,d) -> a == c) . map (closest >>= (,))
        closest p = L.minimumBy (compare `on` (distance p)) clusters
        
kmeansStep :: [Vec] -> [[Vec]] -> [[Vec]]
kmeansStep points pgroups = regroupPoints (computeClusters pgroups) points

kmeansAux :: [Vec] -> [[Vec]] -> [[Vec]]
kmeansAux points pgroups = let pss = kmeansStep points pgroups in
  case pss == pgroups of
  True -> pgroups
  False -> kmeansStep points pss   

-- | Performs the k-means clustering algorithm
--   using trying to use 'k' clusters on the given list of points
kmeans :: Int -> [Vec] -> [[Vec]]
kmeans k points = kmeansAux points pgroups
  where pgroups = partitionPoints k points


