import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Double.Conversion.ByteString as BC
import qualified Data.Vector.Unboxed as V
import Math.KMeans
import System.IO
import System.Random
import Text.Printf

dim = 2
k = 4
nb = 100

main = do
  vectors <- generateVecs nb dim
  {- let vecs2 = [ V.fromList [1.0, 1.0],
                V.fromList [2.0, 2.0],
                V.fromList [-10, -10],
                V.fromList [-12, -8] ] -}
  -- putStrLn . show $ vectors
  let cls = kmeans k vectors
  -- let cls = kmeans k vecs2
  mapM_ (putStrLn . show . length) cls
  -- putStrLn . show $ cls
  saveVecs cls
  
generateVecs :: Int -> Int -> IO [V.Vector Double]
generateVecs nb dim = do
  g <- getStdGen
  replicateM nb (V.fromList `fmap` replicateM dim (randomRIO (-10, 10)))

saveVecs :: [[V.Vector Double]] -> IO ()
saveVecs vss = do
  let vecs = concat $ zipWith (\k l -> map (\v -> (k,v)) l) [1..] vss
  let serialized = BS.unlines $ map serialize vecs
  BS.writeFile "kmeans.dat" serialized
  
  where serialize (n, v) = BC.toExponential 4 (v V.! 0) `BS.append` BS.pack "   " `BS.append` BC.toExponential 4 (v V.! 1) `BS.append` BS.pack "   " `BS.append`BC.toExponential 4 ((n / (fromIntegral k)) :: Double) 