module Main where

import Network
import Matrix

main :: IO ()
main = do
    net <- readNet "net.txt" :: IO (Net Double)
    tdata <- readTrainingData "training.txt" :: IO (Training Double)
    let trained = last $ take 1000 (train net tdata)

    putStr "Target values: "
    print $ targets tdata

    putStr "Old activation: "
    print $ last (activations net (inputs tdata))

    putStr "Trained activations: "
    print $ last (activations trained (inputs tdata))

    putStr "Writing new network to net.txt..."
    writeNet "net.txt" trained
    putStrLn "Done."