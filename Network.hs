module Network 
    (   Net (..)
    ,   propagate
    ,   readNet
    )   where

import Matrix

data Net = Net 
    { inputs :: Matrix Double
    , weights :: [Matrix Double]
    , biases :: [Matrix Double]
    } 
    deriving (Read, Show, Eq)

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

output :: Floating a => Matrix a -> Matrix a -> Matrix a -> Matrix a
output i w b = w * i + b

activate :: Floating a => Matrix a -> Matrix a
activate = fmap sigmoid

propStep :: Floating a => Matrix a -> Matrix a -> Matrix a -> Matrix a
propStep i w b = activate (output i w b)

propagate :: Net -> [Matrix Double]
propagate (Net _ [] _) = []
propagate (Net _ _ []) = []
propagate (Net i (w : ws) (b : bs)) =
    layer : propagate (Net layer ws bs)
    where
        layer = propStep i w b

readNet :: FilePath -> IO Net
readNet f = readFile f >>= \n -> return . read $ n