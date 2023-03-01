palindrom :: [Int] -> Bool --definesc tipul elementelor din lista ca fiind Int
palindrom l = l == (reverse l)

check l = l == (reverse (reverse l))

id x = x

sumThree x y z = x + y + z

prodThree x y z = x * y * z