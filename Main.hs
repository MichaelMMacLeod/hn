module Main where

import Network

main :: IO ()
main = do
    net <- readNet state
    print $ propagate net

state :: String
state = "state.txt"