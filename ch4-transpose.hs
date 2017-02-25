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

        myFunction = transposeChars


transposeChars :: String -> String
transposeChars input = unlines $ transpose ' ' $ lines input


transpose :: a -> [[a]] -> [[a]]
transpose zero rows = if all null rows
                 then []
                 else (map (safeHead zero) rows) : (transpose zero $ map safeTail rows)


safeHead _    (x:xs) = x
safeHead zero _      = zero

safeTail (x:xs) = xs
safeTail _      = []
