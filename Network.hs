module Network
    (   Net (..)
    ,   Training (..)
    ,   transfer
    ,   outputs
    ,   activations
    ,   deltas
    ,   deltaWeights
    ,   train
    ,   classify
    ,   readNet
    ,   readTrainingData
    ,   writeNet
    ,   writeTrainingData
    ,   randMatrix
    ,   randWeights
    )   where


    import Matrix
    import qualified Data.List
    import System.Random
    import Control.Monad (replicateM)

    -- A simple wrapper for a neural network.

    data Net a = Net
        {   weights :: [Matrix a]
        ,   biases :: [Matrix a]
        }   deriving (Read, Show, Eq)

    data Training a = Training
        {   inputs :: Matrix a
        ,   targets :: Matrix a
        }   deriving (Read, Show, Eq)

    -- Transfer functions.

    sigmoid :: (Floating a) => a -> a
    sigmoid x = 1 / (1 + exp (-x))

    sigmoid' :: (Floating a) => a -> a
    sigmoid' x = sigmoid x * (1 - sigmoid x)

    transfer :: (Floating a) => Matrix a -> Matrix a
    transfer = fmap sigmoid

    transfer' :: (Floating a) => Matrix a -> Matrix a
    transfer' = fmap sigmoid'


    -- Forward propagation functions.

    -- Calculates the value of each output layer.
    outputs :: (Floating a)
        => Net a
        -> Matrix a
        -> [Matrix a]
    outputs net input =
        let
            out (w:ws) (b:bs) i =
                layer : out ws bs (transfer layer)
                where
                    layer = transpose w * i + b
            out [] [] _ = []
            out _ _ _ = error "Bad neural network configuration."
        in
            out (weights net) (biases net) input

    -- Calculates the value of each activation layer.
    activations :: (Floating a)
        => Net a
        -> Matrix a
        -> [Matrix a]
    activations net input =
        let
            act (w:ws) (b:bs) i = 
                layer : act ws bs layer 
                where
                    layer = transfer (transpose w * i + b)
            act [] [] _ = []
            act _ _ _ = error "Bad neural network configuration."
        in
            input : act (weights net) (biases net) input


    -- Back propagation functions.

    -- Calculates the delta value for each output layer.
    deltas :: (Floating a)
        => Net a
        -> [Matrix a]
        -> Matrix a
        -> [Matrix a]
    deltas net output target =
        let
            del (w:ws) (o:os) t =
                layer : others
                where
                    layer = (w * head others) |*| transfer' o
                    others = del ws os t
            del _ [o] t = [(transfer o - t) |*| transfer' t]
            del _ _ _ = error "Bad neural network configuration."
        in
            del (weights net) output target

    -- Calculates the delta value for each weight.
    deltaWeights :: (Floating a)
        => [Matrix a]
        -> [Matrix a]
        -> [Matrix a]
    deltaWeights (a:activation) (d:delta) =
        layer : others
        where
            layer = a * transpose d
            others = deltaWeights activation delta
    deltaWeights [] [] = []
    deltaWeights _ _ = error "Bad neural network configuration."

    -- Adjusts a network's weights and biases to better fit the training data.
    train :: (Floating a) 
        => Net a
        -> Training a
        -> [Net a]
    train net tdata =
        let
            output = outputs net (inputs tdata)
            activation = activations net (inputs tdata)
            delta = deltas (Net (drop 1 (weights net)) (biases net)) output (targets tdata)
            deltaWeight = deltaWeights (init activation) delta
            weights' = zipWith (-) (weights net) deltaWeight
            biases' = zipWith (-) (biases net) delta
            net' = Net weights' biases'
        in
            net' : train net' tdata

    classify :: (Floating a, Ord a)
        => Matrix a
        -> [String]
        -> (a, String)
    classify (Matrix [activation]) classifiers =
        let
            p (a,_) (b,_) = if a > b then GT else LT
            sorted = Data.List.sortBy p (zip activation classifiers)
        in
            head sorted
    classify _ _ = error "Bad neural network configuration."

    -- IO Utilities.

    -- Reads a neural network from a file.
    readNet :: (Read a, Floating a) => FilePath -> IO (Net a)
    readNet = fmap read . readFile

    -- Reads training data from a file.
    readTrainingData :: (Read a, Floating a) => FilePath -> IO (Training a)
    readTrainingData = fmap read . readFile

    -- Writes a neural network to a file.
    writeNet :: (Show a, Floating a) => FilePath -> Net a -> IO ()
    writeNet = (. show) . writeFile

    -- Writes training data to a file.
    writeTrainingData :: (Show a, Floating a) => FilePath -> Training a -> IO ()
    writeTrainingData = (. show) . writeFile

    -- Generates a randomized matrix of size (rows, columns).
    randMatrix :: (Floating a, Random a) => (Int, Int) -> IO (Matrix a)
    randMatrix (r,c) = fmap Matrix (replicateM r (replicateM c randomIO))

    randWeights :: (Floating a, Random a) => [Int] -> IO [Matrix a]
    randWeights (x:y:xs) = do
        matrix <- randMatrix (x,y)
        others <- randWeights (y:xs)
        return (matrix : others)
    randWeights _ = return []