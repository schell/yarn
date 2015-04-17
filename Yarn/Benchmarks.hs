module Main where

import Yarn
import Control.Arrow
import Control.Monad.Identity
import Criterion.Main
import Debug.Trace

main :: IO ()
main = do
    let wide = wideGraph 127
        deep = deepGraph 127
        expo2 = expoGraph 2 127
        expo3 = expoGraph 3 121
    defaultMain $ [ bgroup "wide graph (127)" [ bench "stepped (1000)" $ nfIO $ stepGraph wide 1000
                                              , bench "stepped (100)" $ nfIO $ stepGraph wide 100
                                              , bench "stepped (50)" $ nfIO $ stepGraph wide 50
                                              , bench "stepped (10)" $ nfIO $ stepGraph wide 10
                                              ]
                  , bgroup "deep graph (127)" [ bench "stepped (1000)" $ nfIO $ stepGraph deep 1000
                                              , bench "stepped (100)" $ nfIO $ stepGraph deep 100
                                              , bench "stepped (50)" $ nfIO $ stepGraph deep 50
                                              , bench "stepped (10)" $ nfIO $ stepGraph deep 10
                                              ]
                  , bgroup "expo 2 graph (127)" [ bench "stepped (1000)" $ nfIO $ stepGraph expo2 1000
                                                , bench "stepped (100)" $ nfIO $ stepGraph expo2 100
                                                , bench "stepped (50)" $ nfIO $ stepGraph expo2 50
                                                , bench "stepped (10)" $ nfIO $ stepGraph expo2 10
                                                ]
                  , bgroup "expo 3 graph (121)" [ bench "stepped (1000)" $ nfIO $ stepGraph expo3 1000
                                                , bench "stepped (100)" $ nfIO $ stepGraph expo3 100
                                                , bench "stepped (50)" $ nfIO $ stepGraph expo3 50
                                                , bench "stepped (10)" $ nfIO $ stepGraph expo3 10
                                                ]
                  ]
    return ()

stepGraph :: Yarn Int IO () Int
          -> Int -- ^ The number of steps
          -> IO Int
stepGraph gph n = do
    g' <- foldM (\g _ -> execYarn g 1 ()) gph [0..n-1]
    evalYarn g' 1 ()

wideGraph :: Int -> Yarn Int IO a Int
wideGraph n = arr sum <~ (sequenceA $ take n $ cycle [time])

deepGraph :: Int -> Yarn Int IO a Int
deepGraph 0 = time
deepGraph n = time + deepGraph (n-1)

-- | An exponential graph of `n` nodes where each parent node pulls from `w` child
-- nodes that runs some number of steps deep (until the required number of
-- nodes is reached).
expoGraph :: Int -> Int -> Yarn Int IO a Int
expoGraph w n = expo $ trace (unwords ["For expo", show w, show n, "we take", show steps, "steps"]) steps
    where steps = numStepsForNodes (fromIntegral w) (fromIntegral n)
          expo 0 = time
          expo s = arr sum <~ (sequenceA $ take w $ cycle [expo (s-1)])

numStepsForNodes :: Float -> Float -> Int
numStepsForNodes w nodes = stepsTillGreater
    where allSteps = map (numNodes w) [0..]
          stepsTillGreater = length $ takeWhile (< nodes) allSteps

numNodes :: Float -> Float -> Float
numNodes w s = sum $ [ w ** x | x <- [0..s] ]
