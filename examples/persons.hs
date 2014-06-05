import Control.Applicative
import Control.Monad
import Math.KMeans
import Test.QuickCheck

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector         as G

data Person = Person 
    { age    :: Int
    , weight :: Double
    , name   :: String
    , salary :: Int
    } deriving (Eq)

instance Show Person where
    show p = "<" ++ name p ++ ", " 
          ++ show (weight p) ++ "kg, " 
          ++ show (salary p) ++ "€/month, "
          ++ show (age p) ++ "y.o>"

instance Arbitrary Person where
    arbitrary = do
        Person <$> choose (2, 100)
               <*> choose (5, 150)
               <*> pure "francis"
               <*> choose (500, 100000)

persons :: Gen [Person]
persons = vector 5

d :: Distance
d v1 v2 = V.sum $ V.zipWith (\x1 x2 -> abs (x1 - x2)) v1 v2

personToVec :: Person -> V.Vector Double
personToVec p = V.fromList 
    [ fromIntegral $ age p 
    , weight p 
    , fromIntegral $ salary p
    ]

runKMeans :: [Person] -> Clusters Person
runKMeans = kmeans personToVec d 2

main :: IO ()
main = do
    ps <- generate persons
    print ps

    let clusters = runKMeans ps
    putStrLn $ show (G.length clusters)
            ++ " cluster(s) found."
    G.mapM_ print clusters