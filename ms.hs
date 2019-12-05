prefixes :: [a] -> [[a]]
prefixes []       = [[]]
prefixes (x : xs) = [] : [ x : ys | ys <- prefixes xs ]

suffixes :: [a] -> [[a]]
suffixes []       = [[]]
suffixes (x : xs) = (x : xs) : suffixes xs

interleave :: a -> [a] -> [[a]]
interleave x xs = zipWith f (prefixes xs) (suffixes xs)
  where f ys zs = ys ++ [x] ++ zs

permutations :: [a] -> [[a]]
permutations []       = [[]]
permutations (x : xs) = [ zs | ys <- permutations xs, zs <- interleave x ys ]

ok :: [Int] -> Bool
ok [x1,x2,x3,x4,x5,x6,x7,x8,x9] =
  x1 + x2 + x3 == 15 &&
  x4 + x5 + x6 == 15 &&
  x7 + x8 + x9 == 15 &&
  x1 + x4 + x7 == 15 &&
  x2 + x5 + x8 == 15 &&
  x3 + x6 + x9 == 15 &&
  x1 + x5 + x9 == 15 &&
  x3 + x5 + x7 == 15

-- replica
_map :: (a -> b) -> [a] -> [b]
_map f [] = []
_map f (x:xs) = (f x) : (_map f xs)

  {-

_zipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
_zipWith f [] [] = []
_zipWith f (x:xs) (y:ys) = (f x y) : (_zipWith f xs ys)
-}

_add :: Int -> Int -> Int 
_add x y = x + y

_concat :: [[a]] -> [a]
_concat [] = []
_concat (xs:xss) = xs ++ _concat xss 

_eq :: Eq a => a -> a -> Bool
_eq x y = x == y

_and False True = False
_and True False = False
_and True True  = True
_and False False = False

_append :: [a] -> [a] -> [a]
_append xs ys = xs ++ ys

_cons :: a -> [a] -> [a]
_cons x xs = x:xs 

_nil = []

_map_prefixes :: a -> [[a]] -> [[a]]
_map_prefixes x [] = []
_map_prefixes x (y:ys) = _append [x:y] (_map_prefixes x ys)

_prefixes :: [a] -> [[a]]
_prefixes []       = [[]]
_prefixes (x : xs) = _cons _nil (_map_prefixes x (_prefixes xs))
--_prefixes (x : xs) = [] : (_map (\ys -> x:ys) (_prefixes xs))

_suffixes :: [a] -> [[a]]
_suffixes []       = [[]]
_suffixes (x : xs) = _cons (_cons x xs) (_suffixes xs)

_zip_interleave :: a -> [[a]] -> [[a]] -> [[a]]
_zip_interleave x [] [] = []
_zip_interleave x (ys:yss) (zs:zss) = _cons (_append (_append ys [x]) zs) (_zip_interleave x yss zss)

_interleave :: a -> [a] -> [[a]]
_interleave x xs = _zip_interleave x (_prefixes xs) (_suffixes xs)

_map_permutations :: a -> [[a]] -> [[a]]
_map_permutations x []       = []
_map_permutations x (ys:yss) = (_interleave x ys)++(_map_permutations x yss)

_permutations :: [a] -> [[a]]
_permutations []       = [[]]
_permutations (x:xs) = _map_permutations x (_permutations xs)

--_permutations :: [a] -> [[a]]
--_permutations []       = [[]]
--_permutations (x:xs) = _concat (_map (_interleave x) (_permutations xs))

power xs 0 = [[]]
power xs n = [x:ys | x<-xs, ys<-power xs (n-1)]

--_power xs 0 = [[]]
--power xs n = 

--_ok :: [Int] -> Bool
--_ok [x1,x2,x3,x4,x5,x6,x7,x8,x9] =
--  (_and (_and (_and (_and (_and (_and (_and (_eq (_add (_add x1 x2) x3) 15) (_eq (_add (_add x4 x5) x6) 15)) (_eq (_add (_add x7 x8) x9) 15)) (_eq (_add (_add x1 x4) x7) 15)) (_eq (_add (_add x2 x5) x8) 15))(_eq (_add (_add x3 x6) x9) 15)) (_eq (_add (_add x1 x5) x9) 15)) (_eq (_add (_add x3 x5) x7) 15))

_ok :: [Int] -> Bool
_ok xs =
  (_and (_and (_and (_and (_and (_and (_and (_eq (_add (_add (_vl 0 xs) (_vl 1 xs)) (_vl 2 xs)) 15) (_eq (_add (_add (_vl 3 xs) (_vl 4 xs)) (_vl 5 xs)) 15)) (_eq (_add (_add (_vl 6 xs) (_vl 7 xs)) (_vl 8 xs)) 15)) (_eq (_add (_add (_vl 0 xs) (_vl 3 xs)) (_vl 6 xs)) 15)) (_eq (_add (_add (_vl 1 xs) (_vl 4 xs)) (_vl 7 xs)) 15))(_eq (_add (_add (_vl 2 xs) (_vl 5 xs)) (_vl 8 xs)) 15)) (_eq (_add (_add (_vl 0 xs) (_vl 4 xs)) (_vl 8 xs)) 15)) (_eq (_add (_add (_vl 2 xs) (_vl 4 xs)) (_vl 6 xs)) 15))

_vl i [] = 0
_vl 0 (x:xs) = x
_vl n (x:xs) = _vl (n-1) xs

_ms [] = []
_ms (x:xs)
  | _ok x = x:(_ms xs)
  | otherwise = _ms xs

rs = [[6,1,8,7,5,3,2,9,4],[8,1,6,3,5,7,4,9,2],[8,3,4,1,5,9,6,7,2],[4,3,8,9,5,1,2,7,6],[6,7,2,1,5,9,8,3,4],[2,7,6,9,5,1,4,3,8],[2,9,4,7,5,3,6,1,8],[4,9,2,3,5,7,8,1,6]]

magicSquare = [ xs | xs <- permutations [1..9], ok xs ]  
_magicSquare = _ms (_permutations [1..9])
check = rs == _magicSquare

  {-
ok (cons (cons (cons (cons (cons (cons (cons (cons X1 X2) X3) X4) X5) X6) X7) X8) X9) =
  (and (and (and (and (and (and (and (eq (add (add X1 X2) X3) 15) (eq (add (add X4 X5) X6) 15)) (eq (add (add X7 X8) X9) 15)) (eq (add (add X1 X4) X7) 15)) (eq (add (add X2 X5) X8) 15))(eq (add (add X3 X6) X9) 15)) (eq (add (add X1 X5) X9) 15)) (eq (add (add X3 X5) X7) 15)) .
    -}
