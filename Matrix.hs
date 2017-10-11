module Matrix 
    (   Matrix (..)
    )   where

import Data.List (transpose)

newtype Matrix a = Matrix [[a]] deriving (Read, Show, Eq)

instance Num a => Num (Matrix a) where
    Matrix a + Matrix b = Matrix (zipWith (zipWith (+)) a b)
    Matrix a - Matrix b = Matrix (zipWith (zipWith (-)) a b)
    Matrix a * Matrix b = 
        Matrix [[sum $ zipWith (*) a' b' | b' <- transpose b] 
                                         | a' <- a ]
    negate (Matrix a) = Matrix (map (map negate) a)
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs x = x
    signum _ = 1

instance Functor Matrix where
    fmap f (Matrix a) = Matrix (map (map f) a)