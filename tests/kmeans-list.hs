-- Benchmarking the existing 'kmeans' package
import Control.Monad
import Data.KMeans
import System.Environment
import System.Random

main = do
  [dim, nb, k] <- getArgs
  go dim nb k 10
  where go dim nb k 0 = return ()
        go dim nb k i = do
          vectors <- generateVecs (read nb :: Int) (read dim :: Int)
          let cls = kmeans (read k :: Int) vectors
          putStrLn . show $ cls    
          go dim nb k (i-1)
  
generateVecs :: Int -> Int -> IO [[Double]]
generateVecs nb dim = do
  g <- getStdGen
  replicateM nb $ replicateM dim (randomRIO (-10, 10))