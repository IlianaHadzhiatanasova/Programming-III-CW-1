
main :: IO()
main = do
    printEx 1
    assert "Given subtotal" $ subtotal [1,2,3,4] == [1,3,6,10]
    assert "subtotal 1" $ subtotal [15, 2] == [15, 17]

    printEx 2
    assert "Given histogram" $ histogram 5 [1,2,10,4,7,12] == [3,1,2]
    assert "Given histogram 2" $ histogram 5 [1,2,10,4,12] == [3,0,2]
    assert "histogram 1" $ histogram 4 [0,1,3,4,5] == [3,2]
    assert "histogram 2" $ length (histogram 2 [1..100]) == 51
    assert "histogram 3" $ histogram 18 [0, 19, 20] == [1, 2]

    printEx 3
    assert "Given meetsOffer" $ meetsOffer "A*BC" 150 == False
    assert "Given meetsOffer 2" $ meetsOffer' "A*BC" == 128
    assert "meetsOffer 2" $ meetsOffer' "ABC" == 120
    assert "meetsOffer 3" $ meetsOffer "ABC" 120
    assert "meetsOffer 4" $ meetsOffer "ABC" 121 == False
    assert "meetsOffer A*" $ meetsOffer' "A*" == 56

    printEx 4
    assert "Given sortType 1" $ sortType [1, 2, 3] == Ascending
    assert "Given sortType 2" $ sortType [5, 4, 3, 3, 1] == NonAscending
    assert "Given sortType 3" $ sortType [1, 2, 0] == NotSorted
    assert "sortType 1" $ sortType [1,1,1,1,1,2] == NonDescending
    assert "sortType 2" $ sortType [100,99..0] == Descending

    printEx 5
    assert "Given rpcalc 1" $ rpcalc "345*-6+7/" == (-2)
    assert "Given rpcalc 2" $ rpcalc "345*-" == (-17)
    assert "rpcalc 1" $ rpcalc "532*+" == 11
    assert "rpcalc 2" $ rpcalc "63-3/" == 1

    printEx 6
    assert "Given neighbours" $ neighbours 1 (0, 0) [(1, 1), (1, -1)] == [(1, 1)]
    assert "Given neighbours" $ neighbours 3 (10, 10) [(5, 5), (-2.5, -2.5), (2, 2), (10, 10), (100, 100), (12.5, 12.5)] == [(10.0,10.0),(12.5,12.5),(5.0,5.0)]

    printEx 7
    assert "Iliana's 1" $ balanced (Leaf 1)
    assert "Iliana's 2" $ balanced (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))
    assert "Iliana's 3" $ balanced (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Leaf 6))
    assert "Mikey's 1" $ balanced (Node (Node (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)) 6 (Leaf 7)) 8 (Node (Leaf 9) 10 (Node (Leaf 11) 12 (Leaf 13))))
    assert "Billy's 1" $ balanced (Node (Leaf 5) 6 (Node (Leaf 7) 8 (Leaf 9)))
    assert "Billy's 2" $ balanced (Node (Leaf 5) 6 (Node (Leaf 7) 8 (Node (Leaf 9) 10 (Leaf 11)))) == False
    assert "" $ balanced (Node (Leaf 0 ) 1 (Leaf 2))
    assert "" $ balanced (Node (Leaf 0) 2 (Leaf 1)) == False


    printEx 8
    assert "newtonRoot 1" $ newtonRoot 4 0.44 == 2.000609756097561
    assert "newtonRoot 2" $ newtonRoot 4 0.46 == 2.05
    assert "newtonRoot 3" $ newtonRoot 4 0.45 == 2.000609756097561


    printEx 9
    assert "Given hyperOperator 1" $ hyperOperator 1 4 3 == 7
    assert "Given hyperOperator 2" $ hyperOperator 2 4 3 == 12
    assert "Given hyperOperator 3" $ hyperOperator 3 4 3 == 4^3
    --assert "Given hyperOperator test 4" $ hyperOperator 4 4 3 == (4^4)^4

    printEx 10
    assert "Given encode" $ encode "ab0" == [0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,0,1,0,0,1,1,0,0,0,0,0]

    printEx 11
    assert "Given decode" $ decode (encode "ab0") == "ab0"
    assert "Given decode" $ decode (encode "asf8asg") == "asf8asg"
    assert "'Random' encode/decode" $ and [decode(encode x) == x | x <- mapM (const "a\\!?s") "aaaaa"]
    assert "'Random' encode/decode 2" $ and [decode(encode x) == x | x <- mapM (const "*!$%@>") "aaaaaa"]

    printEx 12
    assert "Given makeChange" $ sum [n*d | (n, d) <- zip (makeChange 6 [1,5,10]) [1,5,10] ] == 6
    assert "makeChange custom 1" $ makeChange 39 [2,3] == [0, 13]

    let denoms = [3, 7, 10, 2, 1]
    assert "makeChange solutions for all numbers up to 1000" $ null [sum total == n | n <- [0..100], let solution = makeChange n denoms, let total = [n*d | (n, d) <- zip solution denoms] , sum total /= n]

    printEx 13
    assert "Given goodsteinSequence 1" $ goodsteinSequence (2, [1,1]) == [(2, [1,1]), (3, [0, 1]), (4, [3]), (5, [2]), (6, [1]), (7, [])]

    printEx 14
    assert "Custom isSat 1" $ isSat (And (Const True) (Var 'x')) == [[('x', True)]]
    assert "Custom isSat 2" $ isSat (And (Var 'x') (Var 'x')) == [[('x', True)]]
    assert "Custom isSat 3" $ isSat (And (Var 'x') (Var 'y')) == [[('x', True), ('y', True)]] -- might be the other way round of course
    assert "Custom isSat 4" $ isSat (Imply (Var 'x') (Not (Var 'y'))) == [[('x', True), ('y', False)]] -- might be the other way round of course
    assert "Custom isSat 5" $ isSat (Imply (Var 'x') (Not (Var 'x'))) == [[('x', False)]] -- might be the other way round of course
    assert "isSat" $ isSat (Const True) == [[]]
    assert "isSat" $ isSat (Const False) == []

    printEx 15
    assert "Custom isCantorPair" $ isCantorPair (pair (pair 5 3) 8) == True
    assert "All cantor pairs up to 1000" $ and [isCantorPair (pair (pair x y) (x + y)) | x <- [0..1000], y <- [0..1000]]
    assert "isCantorPair pair' check" $ and [ (x, y) == pair' (pair x y) | x <- [0..1000], y <- [0..1000]]

assert name b = do
    if b then
        putStrLn ("   " ++ name ++ ": passed!")
    else
        putStrLn ("X  " ++ name ++ ": FAILED")

printEx number = do
    putStrLn ""
    putStrLn ("     Exercise " ++ [intToDigit(number)])
    putStrLn "===================="
    putStrLn ""
                    
