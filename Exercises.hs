import Data.Char (isNumber, digitToInt)
import Data.List 

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
rpcalc :: [Char] -> Int
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

--Ex6
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k p xs = take k (sortBy (neighbours' p) xs)

--could remove sqrt 
neighbours' (px, py) (x1, y1) (x2, y2) =  compare (sqrt((x1-px)^2 + (y1-py)^2)) (sqrt((x2-px)^2 + (y2-py)^2))


--Ex7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show

balanced' (Leaf x) = 1
balanced' (Node lt _ rt) = balanced' lt + balanced' rt + 1      

balanced (Leaf _ ) = True
balanced (Node lt _ rt) | diffInDepth <= 1 = True
                        | otherwise = False
                         where diffInDepth = abs(balanced' lt - balanced' rt)
                      
--Ex8
newtonRootSequence :: Double -> [Double]
newtonRootSequence d = iterate (\i -> (i + d/i) / 2) 1

newtonRoot :: Double -> Double -> Double
newtonRoot d epsilon = head [j | (i,j) <- zip (newtonRootSequence d) (tail (newtonRootSequence d)), abs(i-j) <= epsilon]
                    
--Ex9
hyperOperator :: Int -> Int -> Int -> Int
hyperOperator operator a b | operator == 0 = b + 1
                           | operator == 1 = a + b
                           | operator == 2 = a*b
                           | operator == 3 = a^b
                           | otherwise = hyperOperator (operator - 1) a (hyperOperator operator a (b - 1))

hyperOperator' operator a b | operator == 0 = b + 1
                            | operator == 1 && b == 0 = a
                            | operator == 2 && b == 0 = 0
                            | operator >= 3 && b == 0 = 1 
                            | otherwise = hyperOperator' (operator - 1) a (hyperOperator' operator a (b - 1))








