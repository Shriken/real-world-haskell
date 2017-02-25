-- file: ch4-excercises.hs
import Data.Char
import Data.List


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _    []     = []
splitWith pred (x:xs) | pred x    = splitWith pred xs
                      | otherwise = first_hunk : splitWith pred rest
    where (first_hunk, rest) = break pred $ x:xs


type ErrorMessage = String
asInt_fold :: String -> Either ErrorMessage Int
asInt_fold []       = Left "expected an int, found no digits"
asInt_fold ('-':xs) = case asInt_fold xs of
    Left  msg -> Left msg
    Right x   -> Right $ negate x
asInt_fold s        = foldl' addDigit (Right 0) s
    where addDigit (Left msg)  x                         = Left msg
          addDigit (Right acc) x | not $ isDigit x       = Left "non-digit character found"
                                 | div maxBound 10 < acc = Left "number too large"
                                 | otherwise             = Right $ acc * 10 + (digitToInt x)


myConcat :: [[a]] -> [a]
myConcat l = foldr (++) [] l


takeWhileRecursive :: (a -> Bool) -> [a] -> [a]
takeWhileRecursive p (x:xs) = if p x then x : takeWhileRecursive p xs else []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold p l = foldr step [] l
    where step x acc = if not $ p x then [] else x:acc


groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy c l  = reverse $ foldl step [[head l]] $ tail l
    where step ((a:as):ass) x | a `c` x   = (a:as ++ [x]):ass
                              | otherwise = [x]:(a:as):ass
