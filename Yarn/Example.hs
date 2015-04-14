{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yarn
import Control.Arrow
import Control.Concurrent
import System.Remote.Monitoring
import Data.IORef

lineGenerator :: IORef String -> Yarn Double IO () String
lineGenerator ref = Yarntaom $ \_ _ -> do
    str <- readIORef ref
    return $ Output str $ lineGenerator ref

network :: IORef String -> Yarn Double IO () String
network ref = lineGenerator ref ~> arr (show . map fromEnum)

main :: IO ()
main = do
    _ <- forkServer "localhost" 8000

    putStrLn "Yarn example."

    ref <- newIORef ""
    let read' = getLine >>= writeIORef ref >> read'
    _ <- forkIO read'

    let go t y = do (Output val y') <- stepYarn y t ()
                    putStrLn val
                    go (t+1) y'

    go 0 $ network ref
