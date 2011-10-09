import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Double.Conversion.ByteString as BC
import qualified Data.Vector.Unboxed as V
import Math.KMeans
import System.Environment
import System.IO
import System.Random
import Text.Printf

dim = 2
k = 5
nb = 10000

main = do
  filename <- (!!0) `fmap` getArgs
  vectors <- generateVecs nb dim
  let cls = kmeans k vectors
  saveVecs filename cls
  
generateVecs :: Int -> Int -> IO [V.Vector Double]
generateVecs nb dim = do
  g <- getStdGen
  replicateM nb (V.fromList `fmap` replicateM dim (randomRIO (-10, 10)))

saveVecs :: String -> [[V.Vector Double]] -> IO ()
saveVecs filename vss = do
  let vecs = concat $ zipWith (\k l -> map (\v -> (k,v)) l) [1..] vss
  let serialized = BS.unlines $ map serialize vecs
  BS.writeFile filename serialized
  
  where serialize (n, v) = BC.toExponential 4 (v V.! 0) `BS.append` BS.pack "   " `BS.append` BC.toExponential 4 (v V.! 1) `BS.append` BS.pack "   " `BS.append`BC.toExponential 4 ((n / (fromIntegral k)) :: Double) 