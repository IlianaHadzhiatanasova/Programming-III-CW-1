import Data.Char (isNumber, digitToInt)

--Ex1
subtotal :: Num a => [a] -> [a]
subtotal' i xs = sum(take (i+1) xs)
subtotal xs = [subtotal' y xs | y <- [0..length xs - 1]]


--Ex2
histogram :: Int -> [Int] -> [Int]
histogram' n xs = [[n*i..(n*(i+1) - 1)] | i <- [0..((maximum xs) `div` n)]]
histogram'' x xs = length[x' | x' <- xs, x == x']
histogram n xs = [ sum[histogram'' xi xs|xi <- x] | x <- histogram' n xs]

--Ex3
meetsOffer :: String -> Int -> Bool
meetsOffer' [] = 0
meetsOffer' ('A':'*':xs) = 56 + meetsOffer' xs
meetsOffer' (x:xs) |x == 'A' = 48 + meetsOffer' xs
                   |x == 'B' = 40 + meetsOffer' xs
                   |x == 'C' = 32 + meetsOffer' xs
                   |x == 'D' = 24 + meetsOffer' xs
                   |x == 'E' = 16 + meetsOffer' xs
                   |otherwise = error "Wrong input"

meetsOffer xs n | meetsOffer' xs < n = False
                | otherwise = True

--Ex4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show
sortType :: Ord a => [a] -> TypeOfSort 
sortType' xs = zip xs (tail xs)

sortType [] = Ascending
sortType [a] = Ascending
sortType xs | and [ x < y | (x,y) <- sortType' xs ] = Ascending
            | and [ x == y | (x,y) <- sortType' xs ] = Constant
            | and [ x <= y | (x,y) <- sortType' xs ] = NonDescending
            | and [ x >= y | (x,y) <- sortType' xs ] = NonAscending
            | and [ x > y | (x,y) <- sortType' xs ] = Descending
            | otherwise = NotSorted

--Ex5 
rpcalc xs | null result = error "Empty stack"
          | length result > 1 = error "Invalid stack"
          | otherwise = head result
          where result = rpcalc' xs []

rpcalc' [] stack = stack
rpcalc' (x:xs) stack | x == '+' = rpcalc' xs ((head $ tail stack) + (head stack): drop 2 stack)
                     | x == '*' = rpcalc' xs ((head $ tail stack) * (head stack): drop 2 stack)
                     | x == '/' = rpcalc' xs ((head $ tail stack) `div` (head stack): drop 2 stack)
                     | x == '-' = rpcalc' xs ((head $ tail stack) - (head stack): drop 2 stack)
                     | isNumber x = rpcalc' xs ((digitToInt x) : stack)
                     | otherwise = error "Wrong input"
              

















