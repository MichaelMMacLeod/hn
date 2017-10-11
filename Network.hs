module Network 
    (   Net (..)
    ,   propagate
    ,   readNet
    )   where

import Matrix

data Net = Net 
    { activations :: Matrix Double
    , weights :: [Matrix Double]
    , biases :: [Matrix Double]
    } 
    deriving (Read, Show, Eq)

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

output :: Floating a => Matrix a -> Matrix a -> Matrix a -> Matrix a
output a w b = w * a + b

activate :: Floating a => Matrix a -> Matrix a
activate = fmap sigmoid

propStep :: Floating a => Matrix a -> Matrix a -> Matrix a -> Matrix a
propStep a w b = activate (output a w b)

propagate :: Net -> [Matrix Double]
propagate (Net _ [] _) = []
propagate (Net _ _ []) = []
propagate (Net a (w : ws) (b : bs)) =
    layer : propagate (Net layer ws bs)
    where
        layer = propStep a w b

readNet :: FilePath -> IO Net
readNet f = fmap read (readFile f)