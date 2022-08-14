-- example
power :: Int -> Int -> Int
power n 0 = 1
power n k = n * power n (k-1)

-- 1. (a)
power1 :: Int -> Int -> Int
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product $ replicate k n

-- 1. (b)
power2 :: Int -> Int -> Int
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k =
  if even k
  then power2 (n^2) (div k 2)
  else n * power2 n (k-1)


-- 2. (a)
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

-- 2. (b)
rev2 :: [a] -> [a]
rev2 [x,y] = [y,x]
rev2 (x:xs) = [x] ++ xs


-- 3. (a)
tailUpto :: Int -> Int -> [Int] -> [Int]
tailUpto m n xs | m > n = xs
tailUpto m n xs = m : (tailUpto (m+1) n xs)

-- 3. (b)
tailFib :: Int -> Int -> Int -> Int
tailFib n b a | n == 1 = a
tailFib n b a = tailFib (n - 1) a (a + b)


-- 4. (a)
palindrome :: [Int] -> Bool
palindrome w = w == reverse w

-- 4. (b)
isPermutation :: [Int] -> [Int] -> Bool
isPermutation a b | myLength a /= myLength b = False
isPermutation [] [] = True
isPermutation (x:xs) tar = isPermutation xs (removeOnce x tar)

-- helper function for 4. (b)
removeOnce :: Int -> [Int] -> [Int]
removeOnce x [] = []
removeOnce x (y:ys) | x == y    = ys
                    | otherwise = y : removeOnce x ys

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


main = do
    pretest "power 7 5" $ power 7 5
    pretest "power 3 7" $ power 3 7
    pretest "power1 7 5" $ power1 7 5
    pretest "power2 3 7" $ power2 3 7
    pretest "myButLast [1, 2, 3, 4]" $ myButLast [1, 2, 3, 4]
    pretest "myButLast ['a'..'z']" $ myButLast ['a'..'z']
    pretest "rev2 [1, 2]" $ rev2 [1, 2]
    pretest "rev2 [1, 2, 3]" $ rev2 [1, 2, 3]
    pretest "tailUpto 3 8 [1,2]" $ tailUpto 3 8 [1, 2]
    pretest "tailUpto 8 3 [1]" $ tailUpto 8 3 [1]
    pretest "tailFib 5 0 1" $ tailFib 5 0 1
    pretest "palindrome [1, 2, 2, 3, 3]" $ palindrome [1, 2, 2, 3, 3]
    pretest "palindrome [1, 2, 3, 2, 1]" $ palindrome [1, 2, 3, 2, 1]
    pretest "palindrome [3]" $ palindrome [3]
    pretest "palindrome []" $ palindrome []
    pretest "isPermutation [] []" $ isPermutation [] []
    pretest "isPermutation [1,2,1] [2,1,1]" $ isPermutation [1,2,1] [2,1,1]
    pretest "isPermutation [1,2,1] [2,1,2]" $ isPermutation [1,2,1] [2,1,2]
    pretest "isPermutation [1,2,1] [2,1,1,2]" $ isPermutation [1,2,1] [2,1,1,2]
    pretest "removeOnce 3 [1,3,5,3,4]" $ removeOnce 3 [1,3,5,3,4]
    pretest "removeOnce 5 [1,2,3,3,4]" $ removeOnce 5 [1,2,3,3,4]
    pretest "removeOnce 3 []" $  removeOnce 3 []
    where
    pretest p a = putStrLn $ p ++ " = " ++ show a