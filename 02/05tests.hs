sublist :: Int -> Int -> [a] -> [a]
sublist n k xs = undefined

test1 = sublist 2 5 [0..] == [2..4]
test2 = sublist 5 2 [0..] == []
test3 = sublist (-43) (-100) [0..] == []
test4 = sublist 3 3 [0..] == []
test5 = sublist 3 4 [0..] == [3]
test6 = sublist (-3) 3 [0..] == [0..2]
test7 = sublist 3 4 [1..1] == []

testAll =
  sequence_ $
  map (\(i, t) -> if not t then putStrLn ("test " ++ show i ++ " failed") else mempty) $
  zip
  [1..]
  [ test1,
    test2,
    test3,
    test4,
    test5,
    test6,
    test7
  ]

