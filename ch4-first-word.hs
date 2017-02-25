-- file: ch4-first-word.hs

import System.Environment (getArgs)


interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ function input


main = mainWith myFunction
    where
        mainWith func = do
            args <- getArgs
            case args of
                [input, output] -> interactWith func input output
                _ -> putStrLn "error: Expected 2 arguments"

        myFunction = firstWords


firstWords :: String -> String
firstWords input = unlines $ map (head_or_empty . words) $ lines input
    where head_or_empty (x:xs) = x
          head_or_empty _      = ""
