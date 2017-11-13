import Data.Char 
import Data.List 

--Ex1
subtotal :: Num a => [a] -> [a]

subtotal' i xs = sum(take (i+1) xs)

subtotal xs = [subtotal' y xs | y <- [0..length xs - 1]]


--Ex2
histogram :: Int -> [Int] -> [Int]

histogram' n xs = [[n*i..(n*(i+1) - 1)] | i <- [0..((maximum xs) `div` n)]]

histogram'' x xs = length[x' | x' <- xs, x == x']

histogram n xs = [sum[histogram'' xi xs | xi <- x] | x <- histogram' n xs]


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

sortType xs | and [ x < y | (x,y) <- sortType' xs ] = Ascending
            | and [ x == y | (x,y) <- sortType' xs ] = Constant
            | and [ x <= y | (x,y) <- sortType' xs ] = NonDescending
            | and [ x >= y | (x,y) <- sortType' xs ] = NonAscending
            | and [ x > y | (x,y) <- sortType' xs ] = Descending
            | otherwise = NotSorted

--Ex5 
rpcalc :: [Char] -> Int

rpcalc' [] stack = stack
rpcalc' (x:xs) stack | x == '+' = rpcalc' xs ((head $ tail stack) + (head stack): drop 2 stack)
                     | x == '*' = rpcalc' xs ((head $ tail stack) * (head stack): drop 2 stack)
                     | x == '/' = rpcalc' xs ((head $ tail stack) `div` (head stack): drop 2 stack)
                     | x == '-' = rpcalc' xs ((head $ tail stack) - (head stack): drop 2 stack)
                     | isNumber x = rpcalc' xs ((digitToInt x) : stack)
                     | otherwise = error "Wrong input"

rpcalc xs | null result = error "Empty stack"
          | length result > 1 = error "Invalid stack"
          | otherwise = head result
          where result = rpcalc' xs []

--Ex6
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]

neighbours' (px, py) (x1, y1) (x2, y2) =  compare ((x1-px)^2 + (y1-py)^2) ((x2-px)^2 + (y2-py)^2)

neighbours k p xs = take k (sortBy (neighbours' p) xs)

--Ex7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show
balanced :: SearchTree -> Bool

balanced' (Leaf _) = 1
balanced' (Node lt _ rt) = max (balanced' lt) (balanced' rt) + 1

balanced'' (Leaf i) = [i]
balanced'' (Node lt i rt) = balanced'' lt ++ [i] ++ balanced'' rt      

balanced (Leaf _ ) = True
balanced (Node lt i rt) = depthDifference <= 1 && ordered
                         where depthDifference = abs(balanced' lt - balanced' rt) 
                               check = balanced'' (Node lt i rt) 
                               ordered = and [ x < y | (x,y) <- zip check (tail check)]
                      
--Ex8
newtonRootSequence :: Double -> [Double]

newtonRootSequence d = iterate (\i -> (i + d/i) / 2) 1

newtonRoot :: Double -> Double -> Double

newtonRoot d epsilon = head [j | (i,j) <- zip (newtonRootSequence d) (tail (newtonRootSequence d)), abs(i-j) <= epsilon]
                    
--Ex9
hyperOperator :: Int -> Int -> Int -> Int

hyperOperator operator a b  | (operator >= 3 && b == 0) || (operator >= 4 && even b && a == 0) = 1
                            | operator == 0 = b + 1
                            | operator == 1 = a + b 
                            | operator == 2 && b == 0 = 0
                            | operator > 2 && b == 1 = a 
                            | otherwise = hyperOperator (operator - 1) (hyperOperator operator a (b-1)) a

--Ex10
encode :: String -> [Int]

encode' n i | i == (-1) = []
            | n - 2^i >= 0 = 1 : encode' (n-2^i) (i - 1)
            | otherwise = 0 : encode' n (i-1)

encode'' xs | odd (sum xs) = xs ++ [1]
            | otherwise = xs ++ [0]

encode xs = concat $ map (\x -> encode'' (encode' (ord x) 7)) xs


--Ex11
decode' xs = chr $ sum [ a*(2^b) | (a,b) <- zip xs [length xs - 1, length xs - 2..0]]

decode'' xs = even (sum (init xs) + (last xs)) 

decode''' xs | length xs == 0 = [] 
             | length xs `mod` 9 /= 0 = []
             | length (filter (\x -> x/= 0 && x/= 1) xs) /= 0 = []
             | not (decode'' (take 9 xs)) = []
             | otherwise = decode' (take 8 xs) : decode (drop 9 xs)  

decode xs | length result == length xs `div` 9 = result
          | otherwise = []
          where result = decode''' xs 

--Ex12
makeChange :: Integral t => t -> [t] -> [t]

makeChange 0 [] = []
makeChange n [] = [-1]
makeChange n (x:xs) | length solutions >=1 = head $ sortBy (\a b -> compare (sum a) (sum b)) solutions
                    | otherwise = [-1]
                     where solutions = [solution | y <- [0..n `div` x], let solution = y: makeChange (n - x*y) xs, length (filter (<0) solution) == 0]

--Ex13 numbers no bigger than the base 
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

--Ex14
type Subst = Assoc Char Bool
type Assoc k v = [(k,v)]
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop 

isSat :: Prop -> [Subst]

isSat' _ (Const b) = b
isSat' a (Var b) = isSat''''' b a
isSat' a (Not b) = not $ isSat' a b
isSat' a (And b c) = isSat' a b && isSat' a c
isSat' a (Imply b c) = isSat' a b <= isSat' a c 

isSat'' (Const _) = []
isSat'' (Var a) = [a]
isSat'' (Not a) = isSat'' a
isSat'' (And a b) = isSat'' a ++ isSat'' b
isSat'' (Imply a b) = isSat'' a ++ isSat'' b

--all possible combinations
isSat''' 0 = [[]]
isSat''' a = map (False :) all ++ map (True :) all
             where all = isSat''' (a - 1)

isSat'''' a = map (zip vs) (isSat''' (length vs))
           where vs = nub (isSat'' a)

isSat''''' :: Eq k => k -> Assoc k v -> v
isSat''''' k t = head [v | (k', v) <- t, k == k']

isSat s = [binding | binding <- isSat'''' s, isSat' binding s]
            

--Ex15
isCantorPair :: Integral t => t -> Bool

pair x y = y + (xysum) * (xysum +1) `div` 2 
           where xysum = x+y

pair' z = (x,y) 
          where t = floor ((sqrt(8 * (fromIntegral z) +1) -1) / 2)
                x = (t*(t+3) `div` 2) - z
                y = z - t * (t+1) `div` 2 

isCantorPair n = fst y + snd y == snd x
                 where x = pair' n
                       y = pair' $ fst x

--TODO finish ex 13 
--TODO add comments
--TODO check types 
--TODO test 
                    
              


