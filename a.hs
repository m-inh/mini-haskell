import Debug.Trace 

nil = []
cons x xs = x : xs
add x y = x + y
eq x y = x == y

not True  = False
not False = True

-- eq 0 0         = True
-- eq (s x) 0     = False
-- eq 0 (s y)     = False
-- eq (s x) (s y) = eq x y
--
_eq :: Int -> Int -> Bool
_eq 0 0 = True
_eq x 0 = False
_eq 0 y = False
_eq x y = _eq (x-1) (y-1)

_and True True = True
_and False y   = False
_and x False   = False

append [] ys         = ys
append (x : xs) ys = cons x (append xs ys)

-- add 0 y     = y
-- add (s x) y = s (add x y)

map_prefixes x [] = []
map_prefixes x (y : ys) = append [x : y] (map_prefixes x ys)

prefixes [] = [[]]
prefixes (x : xs) = cons [] (map_prefixes x (prefixes xs))

suffixes [] = [[]]
suffixes (x : xs) = cons (x : xs) (suffixes xs)

zip_interleave x [] [] = []
zip_interleave x (ys : yss) (zs : zss) = cons (append (append ys [x]) zs) (zip_interleave x yss zss)

interleave x xs = zip_interleave x (prefixes xs) (suffixes xs)

map_permutations x [] = []
map_permutations x (ys : yss) = append (interleave x ys) (map_permutations x yss)

permutations []       = [[]]
permutations (x : xs) = map_permutations x (permutations xs)

values i [] = 0
values 0 (x : xs) = x
values i (x : xs) = values (i - 1) xs

ok xs = (_and (_and (_and (_and (_and (_and (_and (eq (add (add (values 0 xs) (values 1 xs)) (values 2 xs)) 15) (eq (add (add (values 3 xs) (values 4 xs)) (values 5 xs)) 15)) (eq (add (add (values 6 xs) (values 7 xs)) (values 8 xs)) 15)) (eq (add (add (values 0 xs) (values 3 xs)) (values 6 xs)) 15)) (eq (add (add (values 1 xs) (values 4 xs)) (values 7 xs)) 15))(eq (add (add (values 2 xs) (values 5 xs)) (values 8 xs)) 15)) (eq (add (add (values 0 xs) (values 4 xs)) (values 8 xs)) 15)) (eq (add (add (values 2 xs) (values 4 xs)) (values 6 xs)) 15))

_add = add

_ok xs =
  (_and (_and (_and (_and (_and (_and (_and (_eq (_add (_add (values 0 xs) (values 1 xs)) (values 2 xs)) 15) (_eq (_add (_add (values 3 xs) (values 4 xs)) (values 5 xs)) 15)) (_eq (_add (_add (values 6 xs) (values 7 xs)) (values 8 xs)) 15)) (_eq (_add (_add (values 0 xs) (values 3 xs)) (values 6 xs)) 15)) (_eq (_add (_add (values 1 xs) (values 4 xs)) (values 7 xs)) 15))(_eq (_add (_add (values 2 xs) (values 5 xs)) (values 8 xs)) 15)) (_eq (_add (_add (values 0 xs) (values 4 xs)) (values 8 xs)) 15)) (_eq (_add (_add (values 2 xs) (values 4 xs)) (values 6 xs)) 15))

expect = [[6,1,8,7,5,3,2,9,4],[8,1,6,3,5,7,4,9,2],[8,3,4,1,5,9,6,7,2],[4,3,8,9,5,1,2,7,6],[6,7,2,1,5,9,8,3,4],[2,7,6,9,5,1,4,3,8],[2,9,4,7,5,3,6,1,8],[4,9,2,3,5,7,8,1,6]]
 
magic_square [] = []
magic_square (x : xs) = if_magic_square x xs (ok x)
if_magic_square c xs True = cons c (magic_square xs)
if_magic_square c xs False = magic_square xs

main = print $ magic_square (permutations [1,2,3,4,5,6,7,8,9])



