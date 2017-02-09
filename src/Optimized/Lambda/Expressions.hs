module Optimized.Lambda.Expressions where

import           Prelude          hiding (and, curry, fst, not, or, pred, snd,
                                   succ, uncurry)

import           Optimized.Lambda

v0, v1, v2, v3 :: Expr
v0 = Var 0
v1 = Var 1
v2 = Var 2
v3 = Var 3

lam2, lam3, lam4 :: Expr -> Expr
lam2 = lamn 2
lam3 = lamn 3
lam4 = lamn 4

-- true = λ.λ.1
true :: Expr
true = lam2 v1
-- false = λ.λ.0
false :: Expr
false = lam2 v0

-- ifThenElse = p ? x : y = λ.λ.λ.2 1 0
ifThenElse :: Expr
ifThenElse = lam3 (v2 <> v1 <> v0)
-- not = p ? false : true = λ.ifThenElse 0 false true
not :: Expr
not = lam (ifThenElse <> v0 <> false <> true)
-- and = p ? q : false = λ.λ.ifThenElse 1 0 false
and :: Expr
and = lam2 (ifThenElse <> v1 <> v0 <> false)
-- or = p ? true : q = λ.λ.ifThenElse 1 true 0
or :: Expr
or =  lam2 (ifThenElse <> v1 <> true <> v0)

-- (l, r) = λ.λ.λ.0 2 1
pair :: Expr
pair = lam3 (v0 <> v2 <> v1)
-- fst = λ.0 true
fst :: Expr
fst = lam (v0 <> true)
-- snd = λ.0 false
snd :: Expr
snd = lam (v0 <> false)

-- curry = λ.λ.λ.2 (1, 0)
curry :: Expr
curry = lam3 (v2 <> (pair <> v1 <> v0))
-- uncurry = λ.λ.0 (fst 1) (snd 1)
uncurry :: Expr
uncurry = lam2 (v1 <> (fst <> v0) <> (snd <> v0))

-- n = λ.λ.1^n 0
num :: Int -> Expr
num n = lam2 (iterate (v1 <>) v0 !! n)

-- succ = λ.λ.λ.2 1 (1 0)
succ :: Expr
succ = lam3 (v2 <> v1 <> (v1 <> v0))
--zero = λ.0 (λ.false) true
zero :: Expr
zero = lam (v0 <> lam false <> true)

--gte (>=) = λ.λ.zero(1 pre 0)
gte :: Expr
gte = lam2 (zero <> (v1 <> pred <> v0))

--eq (==) = λ.λ.(zero (1 pre 0)) and (zero (0 pre 1))
eq :: Expr
eq = lam2 (and <> (zero <> (v1 <> pred <> v0)) <> (zero <> (v0 <> pred <> v1)))

-- m + n = λ.λ.λ.λ.3 1 (2 1 0)
add :: Expr
add = lam4 (v3 <> v1 <> (v2 <> v1 <> v0))
-- m * n = λ.λ.λ.λ.3 (2 1) 0
mult :: Expr
mult = lam4 (v3 <> (v2 <> v1) <> v0)

-- prefn = λ.λ.(false, (fst 0) ? (snd 0) : 1 (snd 0))
prefn :: Expr
prefn = lam2 (pair <> false <> (ifThenElse <> (fst <> v0) <> (snd <> v0) <> (v1 <> (snd <> v0))))
-- pred = λ.λ.λ. snd(2 (prefn 1) (true, 0))
pred :: Expr
pred = lam3 (snd <> (v2 <> (prefn <> v1) <> (pair <> true <> v0)))

--fixpoint
fixpoint :: Expr
fixpoint = fixpointT

--Y = λ.(λ.1 (0 0)) (λ.1 (0 0))
fixpointY :: Expr
fixpointY = lam (lam (v1 <> (v0 <> v1))
             <> lam (v1 <> (v0 <> v1)))

--T = (λ.λ.0 (1 1 0)) (λ.λ.0 (1 1 0))
fixpointT :: Expr
fixpointT = lam2 (v0 <> (v1 <> v1 <> v0))
        <> lam2 (v0 <> (v1 <> v1 <> v0))

-- fact = rec (λ.λ.(zero 0) ? (num 1) : 0 * 1 (pre 0))
fact :: Expr
fact = fixpointT <> factF
  where factF = lam2 (ifThenElse <> (zero <> v0) <> num 1 <> (mult <> v0 <> (v1 <> (pre <> v0))))
