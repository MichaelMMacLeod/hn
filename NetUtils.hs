module NetUtils
    (   Net (..)
    ,   transfer
    ,   outputs
    ,   activations
    ,   deltas
    ,   deltaWeights
    ,   train
    )   where


    import Matrix


    -- A simple wrapper for a neural network.

    data Net a = Net
        {   weights :: [Matrix a]
        ,   biases :: [Matrix a]
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

    -- Adjusts a network's weights and biases to better fit the given input to target matrix.
    train :: (Floating a) 
        => Net a 
        -> Matrix a 
        -> Matrix a 
        -> [Net a]
    train net input target =
        let
            output = outputs net input
            activation = activations net input
            delta = deltas (Net (drop 1 (weights net)) (biases net)) output target
            deltaWeight = deltaWeights (init activation) delta
            weights' = zipWith (-) (weights net) deltaWeight
            biases' = zipWith (-) (biases net) delta
            net' = Net weights' biases'
        in
            net' : train net' input target