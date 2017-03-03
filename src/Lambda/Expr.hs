module Lambda.Expr
   ((.>), (.>>), (.>>>),
    true, false, ifThenElse, not, and, or,
    pair, fst, snd, sel, cur, uncur,
    num, succ, isZero, pred,
    eq, neq, gte, gt, lte, lt,
    add, mult, sub,
    div, quot, rem,
    combS, combK, combI,
    fixY, fixT, recurs, recursMany,
    letIn, letRecIn, letRecManyIn) where

import           Lambda.Core
import           Lambda.Eval
import           Prelude     (Int, (!!), ($), (-), (.), (<))
import qualified Prelude     as P

x, y, p, q, f, n, m :: Name
x = "x"
y = "y"
p = "p"
q = "q"
f = "f"
n = "n"
m = "m"

vx, vy, vp, vq, vf, vn, vm :: Expr
vx = Var x
vy = Var y
vp = Var p
vq = Var q
vf = Var f
vn = Var n
vm = Var m

infixl 9 .>
(.>) :: Expr -> (Expr -> Expr)
(.>) e1 = \e2 -> e1 <> e2

infixl 9 .>>
(.>>) :: Expr -> (Expr -> Expr -> Expr)
(.>>) e1 = \e2 e3 -> e1 <> e2 <> e3

infixl 9 .>>>
(.>>>) :: Expr -> (Expr -> Expr -> Expr -> Expr)
(.>>>) e1 = \e2 e3 e4 -> e1 <> e2 <> e3 <> e4


-- Logical
true :: Expr
true = [x, y] -->> vx

false :: Expr
false = [x, y] -->> vy

ifThenElse :: Expr
ifThenElse = [p, x, y] -->> vp <> vx <> vy

not :: Expr
not = p --> vp <> false <> true

and :: Expr
and = [p, q] -->> vp <> vq <> false

or :: Expr
or = [p, q] -->> vp <> true <> vq

-- Tuples
pair :: Expr
pair = [x, y, f] -->> vf <> vx <> vy

fst :: Expr
fst = p --> vp <> true

snd :: Expr
snd = p --> vp <> false

sel :: Int -> Int -> Expr -> Expr
sel k n e = (if k < n - 1 then (fst <>) else P.id) (P.iterate (snd <>) e !! k)

cur :: Expr
cur = [f, x, y] -->> vf <> (pair <> vx <> vy)

uncur :: Expr
uncur = [f, p] -->> vf <> (fst <> vp) <> (snd <> vp)

-- Natural numbers
num :: Int -> Expr
num n = [f, x] -->> P.iterate (vf <>) vx !! n

succ :: Expr
succ = [n, f, x] -->> vn <> vf <> (vf <> vx)

isZero :: Expr
isZero = n --> vn <> (x --> false) <> true

pred :: Expr
pred = [n, f, x] -->> vn <> ([p, q] -->> vq <> (vp <> vf)) <> (y --> vx) <> (y --> vy)

-- Comparison
eq :: Expr
eq = [n, m] -->> and <> (isZero <> (vn <> pred <> vm)) <> (isZero <> (vm <> pred <> vn))

neq :: Expr
neq = [n, m] -->> not <> (eq <> vn <> vm)

gt :: Expr
gt = [n, m] -->> (lt <> vm <> vn)

gte :: Expr
gte = [n, m] -->> isZero <> (vn <> pred <> vm)

lt :: Expr
lt = [n, m] -->> not <> (gte <> vn <> vm)

lte :: Expr
lte = [n, m] -->> (gte <> vm <> vn)

-- Arithmetics
add :: Expr
add = [m, n, f, x] -->> vm <> vf <> (vn <> vf <> vx)

mult :: Expr
mult = [m, n, f, x] -->> vm <> (vn <> vf) <> vx

sub :: Expr
sub = [m, n] -->> vn <> pred <> vm

div :: Expr
div = fixY <> ([f, q, m, n] -->> (lt <> vm <> vn) <> (pair <> vq <> vm) <> (vf <> (succ <> vq) <> (sub <> vm <> vn) <> vn)) <> num 0

quot :: Expr
quot = [m, n] -->> fst <> (div <> vm <> vn)

rem :: Expr
rem = [m, n] -->> snd <> (div <> vm <> vn)

-- S K I Combinators
combS :: Expr
combS = [f, p, x] -->> vf <> vx <> (vp <> vx)

combK :: Expr
combK = [x, y] -->> vx

combI :: Expr
combI = x --> vx

-- Fixpoints and Recursion
fixY :: Expr
fixY = y --> (x --> vy <> (vx <> vx)) <> (x --> vy <> (vx <> vx))

fixT :: Expr
fixT = ([x, y] -->> vy <> (vx <> vx <> vy))
    <> ([x, y] -->> vy <> (vx <> vx <> vy))

recurs :: Name -> Expr -> Expr
recurs n e = fixY <> (n --> e)

recursMany :: Name -> [(Name, Expr)] -> Expr
recursMany name nes = let
  p = P.foldl1 (pair .>>) (P.map P.snd nes)
  ns = P.map P.fst nes
  in recurs name (rename name ns p)

rename :: Name -> [Name] -> Expr -> Expr
rename name ns e = P.foldl (\t (i, n) -> subst t n (sel i (P.length ns) (Var name))) e (P.zip [0..] ns)

-- Let expressions
letIn :: Name -> Expr -> Expr -> Expr
letIn v t e = (v --> e) <> t

letRecIn :: Name -> Expr -> Expr -> Expr
letRecIn v t = letIn v (recurs v t)

letRecManyIn :: Name -> [(Name, Expr)] -> Expr -> Expr
letRecManyIn v nes t = letIn v (recursMany v nes) (rename v (P.map P.fst nes) t)
