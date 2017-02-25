-- file: ch3-excercises.hs
import Data.List


myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength []   = 0


mean :: [Int] -> Float
mean l = fromIntegral (sum l) / fromIntegral (length l)


makePalindrome l = l ++ reverse l


isPalindrome l = and $ zipWith (==) l $ reverse l


sortByLength :: [[a]] -> [[a]]
sortByLength l = sortOn length l


joinBy :: a -> [[a]] -> [a]
joinBy _   []       = []
joinBy _   [x]      = x
joinBy sep (x:y:xs) = x ++ [sep] ++ joinBy sep (y:xs)


data Tree a = Node a (Tree a) (Tree a)
            | Empty

height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)


data Vector = Vector Float Float
    deriving Show

angle (Vector x y) = atan2 y x
neg (Vector x y) = Vector (-x) (-y)


data Point = Point Float Float
    deriving (Show, Eq)

sub (Point x1 y1) (Point x2 y2) = Vector (x2 - x1) (y2 - y1)


data Direction = LFT | RGT | STT
    deriving (Show, Eq)

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3 = if dtheta > 0 then LFT
                     else if dtheta < 0 then RGT
                     else STT
    where dtheta = constrain (-pi) pi $ (angle $ sub p3 p2) - (angle $ sub p2 p1)


-- a < b
clamp a b x = if x < a then a
              else if x > b then b
              else x

-- a < b
constrain a b x = if x < a then constrain a b (x + diff)
                  else if x >= b then constrain a b (x - diff)
                  else x
    where diff = b - a


directions :: [Point] -> [Direction]
directions (p1:p2:p3:ps) = (direction p1 p2 p3) : (directions $ p2:p3:ps)
directions _ = []


convex_hull :: [Point] -> [Point]
convex_hull ps = graham_scan [start] $ sortOn anglesToStart rest
    where anglesToStart = angle . neg . sub start
          start         = minimumBy yxLexicoOrd ps
          rest          = filter ((/=) start) ps
          yxLexicoOrd (Point x1 y1) (Point x2 y2) = if y1 < y2 then LT
                                                    else if y1 > y2 then GT
                                                    else if x1 < x2 then LT
                                                    else if x1 > x2 then GT
                                                    else EQ

graham_scan :: [Point] -> [Point] -> [Point]
graham_scan path []          = path
graham_scan path (next:rest) = graham_scan (compress (next:path)) rest
    where compress (p1:p2:p3:p4:ps) = if (direction p1 p2 p3) == (direction p2 p3 p4)
                                      then p1:p2:p3:p4:ps
                                      else compress (p1:p3:p4:ps)
          compress ps = ps
