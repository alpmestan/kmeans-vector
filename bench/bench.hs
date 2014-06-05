module Main where

import Control.Applicative
import Criterion.Main
import Test.QuickCheck

import qualified Data.Vector  as G
import qualified Data.Vector.Unboxed as V

import qualified OldKMeans   as K
import qualified Math.KMeans as K2

main :: IO ()
main = do 
	persons1 <- generate persons
	persons2 <- generate persons

	defaultMain 
		[ 
		  bgroup "ints" [ bench "v0.2" $ whnf kmeans1 ints1
	                    , bench "v0.3" $ whnf kmeans2 ints2 
	                    ]
	    , bgroup "persons" [ bench "v0.2" $ whnf kmeansP1 persons1 
	    				   , bench "v0.3" $ whnf kmeansP2 persons2]
	    ]

ints1, ints2 :: [Int]
ints1 = [1..10000]
ints2 = [1..10000]

data Person = Person 
	{ age    :: Int
	, weight :: Double
	, name   :: String
	, salary :: Int
	} deriving (Eq, Show)

instance Arbitrary Person where
	arbitrary = do
		Person <$> choose (2, 100)
			   <*> choose (5, 150)
			   <*> pure "francis"
			   <*> choose (500, 100000)

persons :: Gen [Person]
persons = vector 10000

-- kmeans of 'Int's in 3 clusters
kmeans1 = G.fromList . K.kmeans  3 . map (\i -> (extract i, i))
kmeans2 = K2.kmeans extract dist 3

-- kmeans of 'Person's in 4 clusters
kmeansP1 = G.fromList . K.kmeans 4 . map p2v
	where p2v p = (personToVec p, p)
kmeansP2 = K2.kmeans personToVec eucl 4

personToVec :: Person -> V.Vector Double
personToVec p = V.fromList 
	[ fromIntegral $ age p 
	, weight p 
	, fromIntegral $ salary p
	]

extract :: Int -> V.Vector Double
extract = V.singleton . fromIntegral

dist :: K2.Distance
dist v1 v2 = V.sum $ V.zipWith (\x1 x2 -> abs (x1 - x2)) v1 v2

eucl :: K2.Distance
eucl v1 v2 = V.sum $ V.zipWith (\x1 x2 -> (x1 - x2)^2) v1 v2