module Main (main) where

    import Network
    import System.Environment (getArgs)

    generate :: FilePath -> [Int] -> IO ()
    generate file layers = do
        net <- randNet layers :: IO (Net Double)
        writeNet file net

    main = do
        args <- getArgs
        generate (head args) (read (last args)) 
