module Matrix
    (   Matrix (..)
    ,   (|*|)
    ,   (*|)
    ,   (|^)
    ,   (|^^)
    ,   (|**)
    ,   transpose
    )   where

    import qualified Data.List (transpose)

    newtype Matrix a = Matrix [[a]] deriving (Read, Eq, Show)

    instance Functor Matrix where
        fmap f (Matrix a) = Matrix (map (map f) a)

    instance (Num a) => Num (Matrix a) where
        Matrix a + Matrix b = Matrix (zipWith (zipWith (+)) a b)
        Matrix a - Matrix b = Matrix (zipWith (zipWith (-)) a b)

        Matrix a * Matrix b = 
            if length (head a) /= length b then
                error "Bad matrix multiplication"
            else
                Matrix [[sum $ zipWith (*) a' b' | b' <- Data.List.transpose b] 
                                                 | a' <- a ]

        negate (Matrix a) = Matrix (map (map negate) a)
        fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
        abs x = x
        signum _ = 1

    -- Hadamard product
    (|*|) :: (Num a) => Matrix a -> Matrix a -> Matrix a
    Matrix a |*| Matrix b = Matrix (zipWith (zipWith (*)) a b)

    -- Multiply by a scalar value
    (*|) :: (Num a) => a -> Matrix a -> Matrix a
    a *| m = fmap (*a) m

    -- Power functions
    (|^) :: (Num a, Integral b) => Matrix a -> b -> Matrix a
    m |^ a = fmap (^a) m

    (|^^) :: (Fractional a, Integral b) => Matrix a -> b -> Matrix a
    m |^^ a = fmap (^^a) m

    (|**) :: (Floating a) => Matrix a -> a -> Matrix a
    m |** a = fmap (**a) m

    -- transpose
    transpose :: Matrix a -> Matrix a
    transpose (Matrix a) = Matrix (Data.List.transpose a)