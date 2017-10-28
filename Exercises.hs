--Ex1
--subtotal :: Num a => [a] -> [a]
subtotal' i xs = sum(take (i+1) xs)
subtotal xs = [subtotal' y xs | y <- [0..length xs - 1]]



