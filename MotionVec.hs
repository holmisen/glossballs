module MotionVec where

import Data.Complex
import System.Random


randomVecs
  :: (RealFloat b, RandomGen g, Random b) =>
     g -> b -> b -> [Complex b]
randomVecs g min max = zipWith mkPolar ls ds where
    ls = randomRs (min,max) g1
    ds = randomRs (0  ,2*pi) g2
    (g1,g2) = split g
