module Lambda.Expr where

import           Lambda.Core
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
not = p --> ifThenElse <> vp <> false <> true

and :: Expr
and = [p, q] -->> ifThenElse <> vp <> vq <> false

or :: Expr
or = [p, q] -->> ifThenElse <> vp <> true <> vq

-- Tuples
pair :: Expr
pair = [x, y, f] -->> vf <> vx <> vy

fst :: Expr
fst = p --> vp <> true

snd :: Expr
snd = p --> vp <> false

curry :: Expr
curry = [f, x, y] -->> vf <> (pair <> vx <> vy)

uncurry :: Expr
uncurry = [f, p] -->> vf <> (fst <> vp) <> (snd <> vp)

-- Natural numbers
num :: P.Int -> Expr
num n = [f, x] -->> P.iterate (vf <>) vx P.!! n

succ :: Expr
succ = [n, f, x] -->> vn <> vf <> (vf <> vx)

iszero :: Expr
iszero = n --> vn <> (x --> false) <> true

prefn :: Expr
prefn = [f, p] -->> pair <> false <> (ifThenElse <> (fst <> vp) <> (snd <> vp) <> (vf <> (snd <> vp)))

pred :: Expr
pred = [n, f, x] -->> snd <> (vn <> (prefn <> vf) <> (pair <> true <> vx))

-- Comparison
eq :: Expr
eq = [n, m] -->> and <> (iszero <> (vn <> pred <> vm)) <> (iszero <> (vm <> pred <> vn))

neq :: Expr
neq = [n, m] -->> not <> (eq <> vn <> vm)

gt :: Expr
gt = [n, m] -->> (lt <> vm <> vn)

gte :: Expr
gte = [n, m] -->> iszero <> (vn <> pred <> vm)

lt :: Expr
lt = [n, m] -->> not <> (gte <> vn <> vm)

lte :: Expr
lte = [n, m] -->> (gte <> vm <> vn)

-- Arithmetics
add :: Expr
add = [m, n, f, x] -->> vm <> vf <> (vn <> vf <> vx)

mult :: Expr
mult = [m, n, f, x] -->> vm <> (vn <> vf) <> vx

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

fix :: Expr
fix = fixT

fact :: Expr
fact = fix <> factF
  where factF = [f, n] -->> ifThenElse <> (iszero <> vn) <> num 1 <> (mult <> vn <> (vf <> (pred <> vn)))

-- Let expressions
letIn :: Name -> Expr -> Expr -> Expr
letIn v t e = (v --> e) <> t

letRecIn :: Name -> Expr -> Expr -> Expr
letRecIn v t = letIn v (fix <> (v --> t))
