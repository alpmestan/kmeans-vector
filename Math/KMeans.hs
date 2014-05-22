{-# LANGUAGE BangPatterns #-}

module Math.KMeans where

{- |
Module      :  Math.KMeans
Copyright   :  (c) Alp Mestanogullari, Ville Tirronen, 2011-2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alpmestan@gmail.com>
Stability   :  experimental

An implementation of the k-means clustering algorithm based on the vector package.

-}

import Data.Foldable
import Data.Monoid
import Data.Traversable

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import qualified Data.List as L
import Data.Function (on)

type Distance = V.Vector Double -> V.Vector Double -> Double

type Clusters a = G.Vector (Cluster a)
type Centroids  = G.Vector (V.Vector Double)

newtype Cluster a = 
	Cluster { elements :: [a] -- ^ elements that belong to that cluster
			} deriving (Eq, Show)

clusterAdd :: Cluster a -> a -> Cluster a
clusterAdd (Cluster c) x = Cluster (x:c)

emptyCluster :: Cluster a
emptyCluster = Cluster []

addCentroids :: V.Vector Double -> V.Vector Double -> V.Vector Double
addCentroids v1 v2 = V.zipWith (+) v1 v2

partition :: Int -> [a] -> Clusters a
partition k vs = G.fromList $ go vs
  where go vs = case L.splitAt n vs of
          (vs', []) -> [Cluster vs']
          (vs', vss) -> Cluster vs' : go vss
        n = (length vs + k - 1) `div` k

{- 

Workflow:

[a] -> Clusters a -> Centroids -> Clusters a -> Centroids -> ... -> [Cluster a]
 |  partition   centroids    		  |							 or Clusters a
 !--------------------->--------------î------ ...

-}

-- | Run the kmeans clustering algorithm.
-- 
--  > kmeans f distance k points
-- 
-- will run the algorithm using 'f' to extract features from your type,
-- using 'distance' to measure the distance between vectors,
-- trying to separate 'points' in 'k' clusters.
kmeans :: (a -> V.Vector Double) -- ^ feature extraction
	   -> Distance               -- ^ distance function
	   -> Int                    -- ^ the 'k' to run 'k'-means with
	   -> [a]                    -- ^ input list of 'points'
	   -> Clusters a             -- ^ result, hopefully 'k' clusters of points
kmeans extract dist k points = go pointGroups
	
	where 
		pointGroups = partition k points

		-- go :: Clusters a -> Clusters a
		go pgroups =
			case kmeansStep pgroups of
				pgroups' | pgroupsEqualUnder pgroups pgroups'  -> pgroups
						 | otherwise -> go pgroups' 

		-- kmeansStep :: Clusters a -> Clusters a
		kmeansStep clusters = 
			case centroidsOf clusters of
				centroids -> 
				    G.filter (not . null . elements)
				  . G.unsafeAccum clusterAdd (G.replicate nbclusters emptyCluster)
				  . map (pairToClosestCentroid centroids)
				  $ points

			where !nbclusters = G.length clusters

		-- centroidsOf :: Clusters a -> Centroids
		centroidsOf cs = G.map centroidOf cs
			where 
			  n = fromIntegral $ G.length cs

			  centroidOf (Cluster elts) = 
			  		V.map (/n) 
				  . L.foldl1' addCentroids
				  $ map extract elts

		-- pairToClosestCentroid :: Centroids -> a -> (Int, a)
		pairToClosestCentroid cs a = (minDistIndex, a)
			where !minDistIndex = G.minIndexBy (compare `on` dist (extract a)) cs

		-- pgroupsEqualUnder :: Clusters a -> Clusters a -> Bool
		pgroupsEqualUnder g1 g2 = 
			G.map (map extract . elements) g1 == G.map (map extract . elements) g2
