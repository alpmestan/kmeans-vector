{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

{- |
Module      :  Math.KMeans
Copyright   :  (c) Alp Mestanogullari, Ville Tirronen, 2011-2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alpmestan@gmail.com>
Stability   :  experimental

An implementation of the k-means clustering algorithm based on the efficient vector package.

-}

module OldKMeans (kmeans, Point, Cluster(..), computeClusters) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import qualified Data.List as L
import Data.Function (on)

--- * K-Means clustering algorithm

-- | Type holding an object of any type and its associated feature vector
type Point a = (V.Vector Double, a)

-- | Type representing a cluster (group) of vectors by its center and an id
data Cluster = Cluster {
  cid    :: {-# UNPACK #-} !Int, -- ^ an identifier for the cluster
  center :: !(V.Vector Double)   -- ^ the 'position' of the center of the cluster
  } -- deriving (Show,Eq)

-- genVec = V.fromList `fmap` vectorOf 3 arbitrary
-- genPts = (flip zip) [0..] `fmap` replicateM 10 genVec
-- genClusters = do
--    cs <- replicateM 5 genVec
--    return (zipWith Cluster [0.. ] cs)
--
-- prop_regroup = forAll genClusters $ \c ->
--                forAll genPts $ \v ->
--                  s (regroupPoints c v) == s (regroupPoints' c v)
--    where
--     same xs = length (L.nub xs) == length xs
--     s = map L.sort


{-# INLINE distance #-}
distance :: Point a -> V.Vector Double -> Double
distance (u,_) v = V.sum $ V.zipWith (\a b -> (a - b)^2) u v

partition :: Int -> [a] -> [[a]]
partition k vs = go vs
  where go vs = case L.splitAt n vs of
          (vs', []) -> [vs']
          (vs', vss) -> vs' : go vss
        n = (length vs + k - 1) `div` k

{-#INLINE computeClusters#-}
computeClusters :: [[V.Vector Double]] -> [Cluster]
computeClusters = zipWith Cluster [0..] . map f
  where f (x:xs) = let (n, v) = L.foldl' (\(k, s) v' -> (k+1, V.zipWith (+) s v')) (1, x) xs
                   in V.map (\x -> x / (fromIntegral n)) v

{-#INLINE regroupPoints#-}
regroupPoints :: forall a. [Cluster] -> [Point a] -> [[Point a]]
regroupPoints clusters points = L.filter (not.null) . G.toList . G.accum (flip (:)) (G.replicate (length clusters) []) . map closest $ points
 where
   closest p = (cid (L.minimumBy (compare `on` (distance p . center)) clusters),p)

regroupPoints' :: [Cluster] -> [Point a] -> [[Point a]]
regroupPoints' clusters points = go points
  where go points = map (map snd) . L.groupBy ((==) `on` fst) . L.sortBy (compare `on` fst) $ map (\p -> (closest p, p)) points
        closest p = cid $ L.minimumBy (compare `on` (distance p . center)) clusters

kmeansStep :: [Point a] -> [[Point a]] -> [[Point a]]
kmeansStep points pgroups = 
  regroupPoints (computeClusters . map (map fst) $ pgroups) points

kmeansAux :: [Point a] -> [[Point a]] -> [[Point a]]
kmeansAux points pgroups = let pss = kmeansStep points pgroups in
  -- has anything changed since the last step?
  -- even a point jumping from one cluster to another is enough to
  -- enter the 'False' case
  case map (map fst) pss == map (map fst) pgroups of
  True -> pgroups -- nothing's changed, we're done
  False -> kmeansAux points pss -- something has changed, so let's try again

-- | Performs the k-means clustering algorithm
--   trying to use 'k' clusters on the given list of points
kmeans :: Int -> [Point a] -> [[Point a]]
kmeans k points = kmeansAux points pgroups
  where pgroups = partition k points
{-# INLINE kmeans #-}

