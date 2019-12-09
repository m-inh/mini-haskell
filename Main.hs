-- name: NGUYEN Minh Tien
-- id  : 1810445
-- acknowledgements: VU Chi Kien

import Debug.Trace
import TRS
import Parser
import System.Environment

-- Pretty printers
showRule (s, t) = show s ++ " = " ++ show t ++ " ."
showTRS rules = unlines [ showRule rule | rule <- rules ]
--

type Substitution = [(String,Term)]

substitute :: Term -> Substitution -> Term
substitute x [] = x
substitute (Con x) sigma = Con x
substitute (Var x) sigma 
  | Just t <- lookup x sigma = t
substitute (App s t) sigma = App (substitute s sigma) (substitute t sigma)

match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma [] = Just sigma
match' sigma ((App x y, App x' y') : tus)
  = match' sigma ((x, x') : (y, y') : tus)
match' sigma ((Var x, u) : tus)
  | Just u' <- lookup x sigma, u' == u = match' sigma tus
  | otherwise                          = match' ((x, u) : sigma) tus
match' sigma ((Con x, Con y) : tus)
  | x == y    = match' sigma tus
  | otherwise = Nothing
match' _ _ = Nothing

match :: Term -> Term -> Maybe Substitution
match t u  = match' [] [(t, u)]

rewriteAtRoot :: TRS -> Term -> Maybe Term
rewriteAtRoot [] t = Nothing
rewriteAtRoot ((l, r): trs) t 
  | Just sigma <- match l t = Just (substitute r sigma)
  | otherwise               = rewriteAtRoot trs t

rewrite :: TRS -> Term -> Maybe Term
rewrite trs (Var x) = rewriteAtRoot trs (Var x)
rewrite trs (Con x)  = rewriteAtRoot trs (Con x)
rewrite trs (App l r)
  | Just r' <- rewrite trs r = Just (App l r') 
  | Just l' <- rewrite trs l = Just (App l' r)
  | otherwise                = rewriteAtRoot trs (App l r)

nf1 :: TRS -> Term -> Term
nf1 trs t 
  | Just u <- rewrite trs t = nf1 trs u 
  | otherwise               = t 


-- nf2
data MTerm = MApp MTerm MTerm | MCon String | NF Term
  deriving Show

--myTrace :: String -> v -> c -> c
myTrace tag var c = trace (tag ++ ": " ++ show var) c
_trace tag var c  = trace (tag ++ ": " ++ show var) c
---------

substitute2 :: Term -> Substitution -> MTerm
substitute2 (Con x) sigma          = MCon x
substitute2 (Var x) sigma 
  | Just t <- lookup x sigma       = NF t
substitute2 (App (Con "s") t) sigma 
  | (NF t') <- substitute2 t sigma = NF (App (Con "s") t')
substitute2 (App s t) sigma        = (MApp (substitute2 s sigma) (substitute2 t sigma))

rewriteAtRoot2 :: TRS -> Term -> Maybe MTerm
rewriteAtRoot2 [] t         = Nothing
rewriteAtRoot2 ((l, r): trs) t 
  | Just sigma <- match l t = Just (substitute2 r sigma)
  | otherwise               = rewriteAtRoot2 trs t

toTerm :: MTerm -> Term
toTerm (NF t)     = t
toTerm (MCon x)   = Con x
toTerm (MApp l r) = App (toTerm l) (toTerm r)

rewrite2 :: TRS -> MTerm -> Maybe MTerm
rewrite2 trs (MCon t)  = rewriteAtRoot2 trs (Con t)
rewrite2 trs (NF t)    = Just (NF t)
rewrite2 trs (MApp l r) 
  | MApp x y <- r, Just a <- rewrite2 trs r = Just (MApp l a)
  | MApp x y <- l, Just b <- rewrite2 trs l = Just (MApp b r)
  | otherwise                               = (rewriteAtRoot2 trs (toTerm (MApp l r)))

nf2 :: TRS -> MTerm -> MTerm
nf2 trs (NF t)               = NF t
nf2 trs t 
  | Just u <- rewrite2 trs t = nf2 trs u
  | otherwise                = t

-- nf3
data Context = Hole | CApp1 Context Term | CApp2 Context MTerm
  deriving Show

type Zipper = (Context, MTerm)

toMTerm :: Term -> MTerm
toMTerm (Con x)   = MCon x
toMTerm (App l r) = MApp (toMTerm l) (toMTerm r)

rat3 :: TRS -> Term -> MTerm
rat3 [] t         = NF (t) 
rat3 ((l, r): trs) t 
  | Just sigma <- match l t = substitute2 r sigma
  | otherwise               = rat3 trs t

rewrite3 :: TRS -> Zipper -> Zipper
rewrite3 trs (c, NF t) 
  | (Hole) <- c = (Hole, NF t)
  | (CApp2 c' mt') <- c = (CApp1 c' t, mt')
  | (CApp1 c' t') <- c = (c', (MApp (NF t) (NF t')))
rewrite3 trs (c, MCon x) = (c, rat3 trs (Con x))
rewrite3 trs (c, MApp x y)
  | (MApp x' y') <- y = rewrite3 trs (CApp2 c x, y)
  | (MApp x' y') <- x = rewrite3 trs (CApp1 c (toTerm y), x)
  | otherwise = (c, rat3 trs (toTerm (MApp x y)))

nf3 :: TRS -> Zipper -> Term
nf3 trs (Hole, NF t) = t
nf3 trs (c, mt) | z' <- rewrite3 trs (c, mt) = (nf3 trs z')

------

main = do
  file : _ <- getArgs
  m <- readTRSFile file
  case m of
    Left e    -> print e
    Right trs -> do
      putStrLn (showTRS trs)
      putStrLn (show rs)
      -- putStrLn ("----->" ++ show (toTerm rs))
      -- putStrLn (show (nf3 trs (Hole, MCon "main")))
      where 
        --rs = nf1 trs (Con "main")
        --rs = toTerm (nf2 trs (MCon "main"))
        rs = nf3 trs (Hole, (MCon "main"))

