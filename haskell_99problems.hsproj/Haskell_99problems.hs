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
    helper new sublist (x:[])   = new ++ [x:sublist]
    helper new []      (x:y:xs) = helper new [x] (y:xs)
    helper new sublist (x:y:xs)
      | x == y    = helper new                  (x:sublist) (y:xs)
      | otherwise = helper (new ++ [x:sublist]) []          (y:xs)
 
------ 10 ------
myNumberedSublists :: (Eq a) => [a] -> [(Int, a)]
myNumberedSublists []     = []
myNumberedSublists (x:xs) = helper [] (1, x) (x:xs)
  where
    helper :: (Eq a) => [(Int, a)] -> (Int, a) -> [a] -> [(Int, a)]
    helper new (count, letter) (x:[])   = new ++ [(count, letter)]
    helper new (0, letter)     (x:y:xs) = helper new (1, x) (y:xs)
    helper new (count, letter) (x:y:xs)
      | x == y    = helper new                        (count+1, x) (y:xs)
      | otherwise = helper (new ++ [(count, letter)]) (1, y) (y:xs)
 
------ 11 ------
data TupleTree a = Multiple (Int, a) | Single a
                   deriving (Show)
                  
myOptimizedSublists :: [(Int, a)] -> [TupleTree a] 
myOptimizedSublists list = map fn list
  where 
    fn :: (Int, a) -> TupleTree a
    fn (1, letter) = Single letter
    fn (count, letter) = Multiple (count, letter)

------ 12 ------
myUncompress :: [TupleTree a] -> [a]
myUncompress tree = foldr fn [] tree
  where
    fn :: TupleTree a -> [a] -> [a]
    fn (Multiple (count, a)) acc = helper (count, a) [] ++ acc
    fn (Single a)            acc = [a] ++ acc
    
    helper :: (Int, a) -> [a] -> [a]
    helper (0, a)     result = result
    helper (count, a) result = helper (count-1, a) (result ++ [a])

------ 13 ------
firstInTriple :: (a, b, c) -> a
firstInTriple (a,_,_) = a

directlyEncoded :: (Eq a) => [a] -> [TupleTree a]
directlyEncoded (x:xs) = helper [] (1, x) (x:xs)
  where
    helper :: (Eq a) => [TupleTree a] -> (Int, a) -> [a] -> [TupleTree a]
    helper new (count, letter) (x:[])   
      | count == 1 = new ++ [Single letter]
      | otherwise  = new ++ [Multiple (count, letter)]
    helper new (0, letter)     (x:y:xs) = helper new (1, x) (y:xs)
    helper new (count, letter) (x:y:xs)
      | x == y     = helper new                                 (count+1, x) (y:xs)
      | count == 1 = helper (new ++ [Single letter])            (1, y)       (y:xs)
      | otherwise  = helper (new ++ [Multiple (count, letter)]) (1, y)       (y:xs)

------ 14 ------
duplicateElements :: [a] -> [a]
duplicateEements  []     = []
duplicateElements list = fn [] list False
  where
    fn :: [a] -> [a] -> Bool -> [a]
    fn copy (x:[]) _ = copy ++ [x] ++ [x]
    fn copy (x:xs) wasDoubled
      | wasDoubled = fn copy xs      False
      | otherwise  = fn (copy ++ [x] ++ [x]) (x:xs) True

------ 15 ------
replicateElements :: [a] -> Int -> [a]
replicateEements  []   _ = []
replicateElements list n = fn [] list n 0
  where
    fn :: [a] -> [a] -> Int -> Int -> [a]
    fn copy (x:[]) n replCount 
      | replCount == n = copy
      | otherwise      = fn (copy ++ [x] ) (x:[]) n (replCount+1)
    fn copy (x:xs) n replCount
      | replCount == n = fn copy xs      n 0
      | otherwise      = fn (copy ++ [x] ) (x:xs) n (replCount+1)

------ 16 ------












