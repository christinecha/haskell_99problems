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