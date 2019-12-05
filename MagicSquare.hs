_add :: Int -> Int -> Int 
_add x y = x + y

_eq :: Int -> Int -> Bool
_eq 0 0 = True
_eq x 0 = False
_eq 0 y = False
_eq x y = _eq (x-1) (y-1)

_and True True  = True
_and x False = False
_and False y = False

_append :: [a] -> [a] -> [a]
_append [] ys = ys
_append (x:xs) ys = x : (_append xs ys)

_cons :: a -> [a] -> [a]
_cons x xs = x:xs 

_nil = []

_map_prefixes :: a -> [[a]] -> [[a]]
_map_prefixes x [] = []
_map_prefixes x (y:ys) = _append [_cons x y] (_map_prefixes x ys)

_prefixes :: [a] -> [[a]]
_prefixes []       = [[]]
_prefixes (x : xs) = _cons _nil (_map_prefixes x (_prefixes xs))

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
_map_permutations x (ys:yss) = _append (_interleave x ys) (_map_permutations x yss)

_permutations :: [a] -> [[a]]
_permutations []       = [[]]
_permutations (x:xs) = _map_permutations x (_permutations xs)

values i [] = 0
values 0 (x:xs) = x
values n (x:xs) = values (n-1) xs

_ok :: [Int] -> Bool
_ok xs =
  (_and (_and (_and (_and (_and (_and (_and (_eq (_add (_add (values 0 xs) (values 1 xs)) (values 2 xs)) 15) (_eq (_add (_add (values 3 xs) (values 4 xs)) (values 5 xs)) 15)) (_eq (_add (_add (values 6 xs) (values 7 xs)) (values 8 xs)) 15)) (_eq (_add (_add (values 0 xs) (values 3 xs)) (values 6 xs)) 15)) (_eq (_add (_add (values 1 xs) (values 4 xs)) (values 7 xs)) 15))(_eq (_add (_add (values 2 xs) (values 5 xs)) (values 8 xs)) 15)) (_eq (_add (_add (values 0 xs) (values 4 xs)) (values 8 xs)) 15)) (_eq (_add (_add (values 2 xs) (values 4 xs)) (values 6 xs)) 15))

magic_square [] = []
magic_square (x:xs)
  | _ok x = x:(magic_square xs)
  | otherwise = magic_square xs

expect = [[6,1,8,7,5,3,2,9,4],[8,1,6,3,5,7,4,9,2],[8,3,4,1,5,9,6,7,2],[4,3,8,9,5,1,2,7,6],[6,7,2,1,5,9,8,3,4],[2,7,6,9,5,1,4,3,8],[2,9,4,7,5,3,6,1,8],[4,9,2,3,5,7,8,1,6]]
actual = magic_square (_permutations [1..9])

check = actual == expect
