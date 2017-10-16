module Backprop 
    (   sigmoid
    ,   transfer
    ,   outputs
    ,   activations
    -- ,   deltas
    ,   deltaWeights
    )   where

    import Matrix

    sigmoid :: Floating a => a -> a
    sigmoid x = 1 / (1 + exp (-x))

    sigmoid' :: Floating a => a -> a
    sigmoid' x = sigmoid x * (1 - sigmoid x)

    transfer :: Floating a => Matrix a -> Matrix a
    transfer = fmap sigmoid

    transfer' :: Floating a => Matrix a -> Matrix a
    transfer' = fmap sigmoid'

    type Input a = Matrix a
    type Weights a = [Matrix a]
    type Biases a = [Matrix a]
    type Outputs a = [Matrix a]
    type Activations a = [Matrix a]

    -- Returns the output layers of a neural network.
    outputs :: Floating a
        => Weights a
        -> Biases a
        -> Input a
        -> Outputs a
    outputs (w:ws) (b:bs) i = 
        layer : outputs ws bs (transfer layer)
        where
            layer = transpose w * i + b
    outputs [] [] _ = []
    outputs _ _ _ = error "Invalid matrix configuration"
    
    activations :: Floating a
        => Weights a
        -> Biases a
        -> Input a
        -> Activations a
    activations weights biases input =
        input : _activations weights biases input
        where
            _activations (w:weights) (b:biases) input =
                layer : others
                where
                    layer = transfer (transpose w * input + b)
                    others = _activations weights biases layer
            _activations [] [] _ = []
            _activations _ _ _ = error "Invalid matrix configuration"

    deltas :: Floating a
        => [Matrix a]
        -> [Matrix a]
        -> Matrix a
        -> [Matrix a]
    deltas (w:ws) (o:os) target =
        layer : others
        where
            layer = (w * head others) |*| transfer' o
            others = deltas ws os target
    deltas _ [o] target =
        [(transfer o - target) |*| transfer' o]
    deltas _ _ _ = error "Invalid network configuration"

    deltaWeights :: Floating a => [Matrix a] -> [Matrix a] -> [Matrix a]
    deltaWeights (a:as) (d:ds) =
        layer : others
        where
            layer = a * transpose d
            others = deltaWeights as ds
    deltaWeights [] [] = []
    deltaWeights _ _ = error "Invalid network configuration"

    main :: IO ()
    main = do
        print $ (last . last) (train weights biases input target 0)

    train _ _ _ _ 10000 = []
    train weights biases input target n =
        let 
            os = outputs weights biases input
            as = activations weights biases input
            ds = deltas (drop 1 weights) os target
            dw = deltaWeights (init as) ds
            weights' = zipWith (-) weights dw
            biases' = zipWith (-) biases ds
        in
            as : train weights' biases' input target (n + 1)

    input :: Matrix Double
    input = 
        Matrix  [   [1]
                ,   [1]
                ]

    target :: Matrix Double
    target =
        Matrix  [   [0.123456789]
                ]

    weights :: [Matrix Double]
    weights = 
        [   Matrix  [   [0.5, 0.5]
                    ,   [0.5, 0.5]
                    ]
        ,   Matrix  [   [0.5]
                    ,   [0.5]
                    ]
        ]

    biases :: [Matrix Double]
    biases =
        [   Matrix  [   [0]
                    ,   [0]
                    ]
        ,   Matrix  [   [0]
                    ]
        ]
