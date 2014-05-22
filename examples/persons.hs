import Control.Applicative
import Math.KMeans
import Test.QuickCheck

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector         as G

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
			   <*> pure ""
			   <*> choose (500, 100000)

persons :: Gen [Person]
persons = vector 10000

eucl :: Distance
eucl v1 v2 = V.sum $ V.zipWith (\x1 x2 -> (x1 - x2)^2) v1 v2

personToVec :: Person -> V.Vector Double
personToVec p = V.fromList 
	[ fromIntegral $ age p 
	, weight p 
	, fromIntegral $ salary p
	]

runKMeans :: [Person] -> Clusters Person
runKMeans = kmeans personToVec eucl 4

main :: IO ()
main = do
	ps <- generate persons
	print $ G.length (runKMeans ps)
