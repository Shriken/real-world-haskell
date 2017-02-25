-- file: ch4-excercises.hs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _    []     = []
splitWith pred (x:xs) | pred x    = splitWith pred xs
                      | otherwise = first_hunk : splitWith pred rest
    where (first_hunk, rest) = break pred $ x:xs


asInt_fold :: String -> Int
asInt_fold s :: foldl s
