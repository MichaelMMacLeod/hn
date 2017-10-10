module Network (sigmoid, output, activate) where



import Matrix



-- Forwards propagation


sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))


output :: Floating a => Matrix a -> Matrix a -> Matrix a -> Matrix a
output inputs weights biases = weights * inputs + biases


activate :: Floating a => Matrix a -> Matrix a
activate = fmap sigmoid


propagate :: Floating a => Matrix a -> Matrix a -> Matrix a -> Matrix a
propagate inputs weights biases = activate (output inputs weights biases)

propagation :: Floating a => Matrix a -> [Matrix a] -> [Matrix a] -> Matrix a
-- iterates propagate until the output layer is reached