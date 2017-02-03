module Lambda.Expressions where

import           Prelude hiding (and, curry, fst, not, or, snd, succ, uncurry)

import           Lambda

-- true = λx y.x
true :: Expr
true = ["x", "y"] -->> Var "x"
-- false = λx y.y
false :: Expr
false = ["x", "y"] -->> Var "y"

-- ifThenElse = p ? x : y = λp x y.p x y
ifThenElse :: Expr
ifThenElse = ["p", "x", "y"] -->> Var "p" -$- Var "x" -$- Var "y"
-- not = p ? false : true = λp.ifThenElse p false true
not :: Expr
not = "p" --> ifThenElse -$- Var "p" -$- false -$- true
-- and = p ? q : false = λp q.ifThenElse p q false
and :: Expr
and = ["p", "q"] -->> ifThenElse -$- Var "p" -$- Var "q" -$- false
-- or = p ? true : q = λp q.ifThenElse p true q
or :: Expr
or = ["p", "q"] -->> ifThenElse -$- Var "p" -$- true -$- Var "q"

-- (l, r) = λl r f.f l r
pair :: Expr
pair = ["l", "r", "f"] -->> Var "f" -$- Var "l" -$- Var "r"
-- fst = λp.p true
fst :: Expr
fst = "p" --> Var "p" -$- true
-- snd = λp.p false
snd :: Expr
snd = "p" --> Var "p" -$- false

-- curry = λf x y.f (x, y)
curry :: Expr
curry = ["f", "x", "y"] -->> Var "f" -$- (pair -$- Var "x" -$- Var "y")
-- uncurry = λg p.g (fst p) (snd p)
uncurry :: Expr
uncurry = ["g", "p"] -->> Var "g" -$- (fst -$- Var "p") -$- (snd -$- Var "p")

-- n = λf x.f^n x
num :: Int -> Expr
num n = ["f", "x"] -->> iterate (Var "f" -$-) (Var "x") !! n

-- succ = λn f x.n f (f x)
succ :: Expr
succ = ["n", "f", "x"] -->> Var "n" -$- Var "f" -$- (Var "f" -$- Var "x")
--zero = λn.n (λx.false) true
zero :: Expr
zero = "n" --> Var "n" -$- ("x" --> false) -$- true

--gte (>=) = λn m.zero(n pre m)
gte :: Expr
gte = ["n", "m"] -->> zero -$- (Var "n" -$- pre -$- Var "m")

--eq (==) = λn m.(zero (n pre m)) and (zero (m pre n))
eq :: Expr
eq = ["n", "m"] -->> and -$- (zero -$- (Var "n" -$- pre -$- Var "m")) -$- (zero -$- (Var "m" -$- pre -$- Var "n"))

-- m + n = λm n f x.m f (n f x)
add :: Expr
add = ["m", "n", "f", "x"] -->> Var "m" -$- Var "f" -$- (Var "n" -$- Var "f" -$- Var "x")
-- m * n = λm n f x.m (n f) x
mult :: Expr
mult = ["m", "n", "f", "x"] -->> Var "m" -$- (Var "n" -$- Var "f") -$- Var "x"

-- prefn = λf p. (false, (fst p) ? (snd p) : f (snd p))
prefn :: Expr
prefn = ["f", "p"] -->> pair -$- false -$- (ifThenElse -$- (fst -$- Var "p") -$- (snd -$- Var "p") -$- (Var "f" -$- (snd -$- Var "p")))
-- pre = λn f x. snd(n (prefn f) (true, x))
pre :: Expr
pre = ["n", "f", "x"] -->> snd -$- (Var "n" -$- (prefn -$- Var "f") -$- (pair -$- true -$- Var "x"))

fixed :: Expr
fixed = fixedT

fixedY :: Expr
fixedY = "y" --> ("x" --> Var "y" -$- (Var "x" -$- Var "x"))
            -$- ("x" --> Var "y" -$- (Var "x" -$- Var "x"))

fixedT :: Expr
fixedT = (["x", "y"] -->> Var "y" -$- (Var "x" -$- Var "x" -$- Var "y"))
     -$- (["x", "y"] -->> Var "y" -$- (Var "x" -$- Var "x" -$- Var "y"))

-- fact = rec (λf n.(zero n) ? 1 : n * f (pre n))
fact :: Expr
fact = fixed -$- factF
  where factF = "f" --> "n" --> ifThenElse -$- (zero -$- Var "n") -$- num 1 -$- (mult -$- Var "n" -$- (Var "f" -$- (pre -$- Var "n")))

--letIn
letIn = undefined
