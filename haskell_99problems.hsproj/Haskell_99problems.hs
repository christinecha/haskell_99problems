------ 1 ------
myLast :: [a] -> a
myLast []     = error "list too small"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

------ 2 ------
secondToLast :: [a] -> a
secondToLast []       = error "list too small"
secondToLast (x:[])   = error "list too small"
secondToLast (x:_:[]) = x
secondToLast (x:xs)   = secondToLast xs

------ 3 ------
elementAt :: [a] -> Int -> a
elementAt []     _ = error "list too small"
elementAt (x:xs) 0 = x
elementAt (x:xs) n = elementAt xs (n-1)

------ 4 ------
myLength :: [a] -> Int
myLength list = foldr fn 0 list
  where
    fn _ acc = acc + 1

------ 5 ------
myReverse :: [a] -> [a]
myReverse list = foldl fn [] list
  where
    fn x acc = acc:x

------ 6 ------
isPalindrome :: (Eq a) => (Num a) => [a] -> Bool
isPalindrome list = myReverse list == list

------ 7 ------
data TreeOf a = Elem a | Branch [TreeOf a]
  
myFlatten :: TreeOf a -> [a]
myFlatten (Elem x) = [x]
myFlatten (Branch (x:[])) = myFlatten x
myFlatten (Branch (x:xs)) = myFlatten x ++ myFlatten (Branch xs)

------ 8 ------
myCompress :: (Eq a) => [a] -> [a]
myCompress list = helper [] list
  where
    helper :: (Eq a) => [a] -> [a] -> [a]
    helper new (x:[]) = new ++ [x]
    helper new (x:y:xs)
      | x == y    = helper new          (y:xs)
      | otherwise = helper (new ++ [x]) (y:xs)

------ 9 ------
mySublists :: (Eq a) => [a] -> [[a]]
mySublists list = helper [] [] list
  where
    helper :: (Eq a) => [[a]] -> [a] -> [a] -> [[a]]
    helper new sublist (x:[]) = new ++ [x:sublist]
    helper new []      (x:y:xs)
      | x == y    = helper new [x] (y:xs)
      | otherwise = helper new [x] (y:xs)
    helper new sublist (x:y:xs)
      | x == y    = helper new                  (x:sublist) (y:xs)
      | otherwise = helper (new ++ [x:sublist]) []          (y:xs)
 
------ 10 ------
myNumberedSublists :: (Eq a) => [a] -> [(Int, a)]
myNumberedSublists (x:xs) = helper [] (0, x) (x:xs)
  where
    helper :: (Eq a) => [(Int, a)] -> (Int, a) -> [a] -> [(Int, a)]
    helper new (count, letter) (x:[])   = new ++ [(count+1, letter)]
    helper new (0, letter)     (x:y:xs)
      | x == letter = helper new (1, x) (y:xs)
      | otherwise   = helper new (1, x) (y:xs)
    helper new (count, letter) (x:y:xs)
      | x == y    = helper new                     (count+1, x) (y:xs)
      | otherwise = helper (new ++ [((count+1), x)]) (0, x)     (y:xs)
 


