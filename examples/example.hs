import Control.Applicative
import Control.Monad
import Math.KMeans
import Math.Probable

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector         as G

runKMeans :: [V.Vector Double] -> Clusters (V.Vector Double)
runKMeans = kmeans id euclidSq 2

oneVecOf :: RandT IO Double -> RandT IO (V.Vector Double)
oneVecOf doubleGen = vectorOf 10 doubleGen

doubleGen1 :: RandT IO Double
doubleGen1 = normal (-1500) 0.1

doubleGen2 :: RandT IO Double
doubleGen2 = normal 1500 0.1

main :: IO ()
main = do
    v1s <- mwc $ listOf 5 (oneVecOf doubleGen1)
    v2s <- mwc $ listOf 5 (oneVecOf doubleGen2)
    let input = v1s ++ v2s

    let clusters = runKMeans input
    putStrLn $ show (G.length clusters)
            ++ " cluster(s) found."
    G.mapM_ print clusters