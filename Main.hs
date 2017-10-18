module Main where

import Network
import Matrix

main :: IO ()
main = do
    net <- readNet "net.txt" :: IO (Net Double)
    let input = Matrix [[1],[1]]
    let target = Matrix [[0.12344456777]]
    let trained = last $ take 1000 (train net input target)
    print target
    print $ last (activations net input)
    print $ last (activations trained input)