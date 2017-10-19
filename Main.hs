module Main (main) where
    

    import Network
    import Matrix

    import System.Environment (getArgs)

    import Data.IDX
    import Data.List

    import qualified Data.Vector.Unboxed as Vector


    -- generate :: FilePath -> ([Int], Int) -> IO ()
    -- generate file (layers,n) = do
    --     net <- randNet (layers, n) :: IO (Net Double)
    --     writeNet file net

    toTrainingData :: [(Int, Vector.Vector Double)] -> ([String], Matrix Double)
    toTrainingData c =
        let 
            (labels, vectors) = unzip c
            matrix = Matrix (map Vector.toList vectors)
        in
            (map show labels, matrix)

    makeInputMatrix :: [Vector.Vector Double] -> Matrix Double
    makeInputMatrix xs = Matrix (Data.List.transpose (map Vector.toList xs))

    makeTargetMatrix :: [Int] -> Matrix Double
    makeTargetMatrix xs = Matrix (map (map fromIntegral) (Data.List.transpose (_make xs)))
        where
            _make :: [Int] -> [[Int]]
            _make (x:xs) = ((take x (repeat 0)) ++ [1] ++ (take (9 - x) (repeat 0))) : _make xs
            _make [] = []

    getSize (Matrix m) = (length m, length (head m))

    main = do
        args <- getArgs
        Just idxlabels <- decodeIDXLabelsFile (head args)
        Just idxdata <- decodeIDXFile (last args)
        let (Just labeledData) = labeledDoubleData idxlabels idxdata
            (t,i) = unzip labeledData
            input = makeInputMatrix i
            target = makeTargetMatrix (map fromIntegral t)
            tdata = Training input target
        net <- randNet ([784,16,16,10],60000) :: IO (Net Double)
        let net' = last (take 1 (train net tdata))
        print (last (activations net input))
        -- (784x16)T 784x6000 
        -- print $ getSize input
        -- print $ getSize target
        -- print (map getSize (biases net))
        -- print (map getSize (weights net))
        -- print (getSize (inputs tdata))
        -- print (getSize (targets tdata))