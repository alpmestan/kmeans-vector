-- Benchmarking my 'kmeans-vector' package
import Control.Monad
import Math.KMeans
import qualified Data.Vector.Unboxed as V
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

generateVecs :: Int -> Int -> IO [V.Vector Double]
generateVecs nb dim = do
  g <- getStdGen
  replicateM nb (V.fromList `fmap` replicateM dim (randomRIO (-10, 10)))