-- 1. (a)
countNeg :: [Int] -> Int
countNeg lt = sum [1 | elem <- lt, elem<0]
-- 1. (b)
raise :: Int -> Int -> Int
raise x n = product [x | _ <- [1..n]]
-- 1. (c)
addZero :: [Int] -> [Int]
addZero xs = [0] ++ xs ++ [0]

pairs :: [Int] -> [(Int, Int)]
pairs x = zip (init x) (tail x)

pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = [x+y | (x,y) <- pairs $ addZero (pascal (n-1))]
-- 2. (a)
q1f1a :: [Int] -> [Int]
q1f1a [] = []
q1f1a xs = map (*3) $ filter (>=3) $ filter (<=10) xs
-- 2. (b)
q1f1b :: [Int] -> [Int]
q1f1b [] = []
q1f1b lt = [(*3) elem | elem <- lt, elem>=3, elem<=10]
-- 2. (c)
compre :: [a]->(a->b)->(a->Bool)->[b]
compre xs f p = map f $ filter p xs
-- 3.
subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
-- 4. (a)
maxl :: (Ord a) => [a] -> a  
maxl = foldr1 (\x acc -> if x > acc then x else acc)
-- 4. (b)
member :: (Eq a) => [a] -> a -> Bool  
member xs x = foldl (\acc y -> if x == y then True else acc) False xs
-- 5.
remdup :: [Char]->[Char]
remdup [] = []
remdup (x:xs) = x : remdup (filter (/=x) (x:xs))

elemOcc :: Char->[Char]->Int
elemOcc k = length . filter (==k)

occurrences :: [Char] -> [(Char, Int)]
occurrences xs = zip (remdup xs) (map (elemOcc') (remdup xs))
                where elemOcc' a = elemOcc a xs
-- 6. (a)
myMap :: (a -> b) -> [a] -> [b]
myMap f xs= foldr (\y ys -> f y: ys) [] xs
-- 6. (b)
{-
  id is base case function, and build a new function by folding a list of functions together with (.) .
  After folding them, go to map to do map's order.
-}

main =  do
  print(countNeg [-10,3,-5,7,8,-9,-11])
  print $ raise 2 8
  print(pascal 4)
  print(q1f1a [2,4,5,12])
  print(q1f1b [2,4,5,12])
  print(compre [2,4,5,8,12] (*3) (\x -> x>=3 && x<=10))
  print(maxl [1,3,6,10,2,8])
  print(member [1,3,6,10,2,8] 11)
  print(member [1,3,6,10,2,8] 10)
  print(occurrences ['a', 'b', 'c', 'a', 'c'])