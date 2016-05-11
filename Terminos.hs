{-# LANGUAGE FlexibleInstances #-}
module Terminos  where

data Term = Var Char
          | Bools Bool
          | Neg Term
          | Conj Term Term
          | Disy Term Term
          | Impl Term Term
          | Equi Term Term
          | Nequ Term Term

instance Show Term where
    show t1 = let s1 = showT t1
              in case s1 of
                    ('(':xs) -> (init . tail) s1
                    _ -> s1

showT :: Term -> String
showT (Bools bool) = if bool then "true" else "false"
showT (Var ch) = show ch
showT (Neg t1) = showT t1
showT (Conj t1 t2) = "("++ showT t1 ++ " /\\ " ++ showT t2 ++ ")"
showT (Disy t1 t2) = "("++ showT t1 ++ " \\/ " ++ showT t2 ++ ")"
showT (Impl t1 t2) = "("++ showT t1 ++ " ==> " ++ showT t2 ++ ")"
showT (Equi t1 t2) = "("++ showT t1 ++ " <==> " ++ showT t2 ++ ")"
showT (Nequ t1 t2) = "("++ showT t1 ++ "!<==>" ++ showT t2 ++ ")"

instance Eq Term where
    (Bools bool1) == (Bools bool2) = (bool1 == bool2)
    (Var ch1) == (Var ch2) = (ch1 == ch2)
    (Neg t1) == (Neg t2) = (t1 == t2)
    (Conj t11 t12) == (Conj t21 t22) = (t11 == t21) && (t12 == t22)
    (Disy t11 t12) == (Disy t21 t22) = (t11 == t21) && (t12 == t22)
    (Impl t11 t12) == (Impl t21 t22) = (t11 == t21) && (t12 == t22)
    (Equi t11 t12) == (Equi t21 t22) = (t11 == t21) && (t12 == t22)
    (Nequ t11 t12) == (Nequ t21 t22) = (t11 == t21) && (t12 == t22)
    _ == _ = False

infixl 7 <==>
(<==>) :: Term -> Term -> Term
t1 <==> t2 = Equi t1 t2
infixl 8 !<==>
(!<==>) :: Term -> Term -> Term
t1 !<==> t2 = Nequ t1 t2
infixl 6 ==>
(==>) :: Term -> Term -> Term
t1 ==> t2 = Impl t1 t2
infixl 9 \/
(\/) :: Term -> Term -> Term
t1 \/ t2 = Disy t1 t2
infixl 9 /\
(/\) :: Term -> Term -> Term
t1 /\ t2 = Conj t1 t2

true :: Term
true = Bools True
false :: Term
false = Bools False



data Sust_1 = Sust_1 Term Term

instance Show Sust_1 where
    show (Sust_1 var term) = show term ++ "=:" ++ show var

(=:) :: Term -> Term -> Sust_1
term1 =: var@(Var ch) = Sust_1 var term1
_ =: _ = error "Argumentos inválidos en =:"

data Equation = Equation Term Term

instance Show Equation where
    show (Equation term1 term2) = show term1 ++ " === " ++ show term2

infix 0 ===
(===) :: Term -> Term -> Equation
term1 === term2 = Equation term1 term2

class Sust a where
    sust :: Term -> a -> Term


instance Sust Sust_1 where
    sust (Neg t1) sus = Neg $ sust t1 sus
    sust (Conj t1 t2) sus = Conj (sust t1 sus) (sust t2 sus)
    sust (Disy t1 t2) sus = Disy (sust t1 sus) (sust t2 sus)
    sust (Impl t1 t2) sus = Impl (sust t1 sus) (sust t2 sus)
    sust (Equi t1 t2) sus = Equi (sust t1 sus) (sust t2 sus)
    sust (Nequ t1 t2) sus = Nequ (sust t1 sus) (sust t2 sus)
    sust t1@(Var c1) (Sust_1 (Var c2) (t2)) = if c1 == c2
                                           then t2
                                           else t1
    sust bo@(Bools b1) sus = bo
    --sust _ _ = error "Error de Sustitución 1"


instance Sust (Term, Sust_1, Term) where
    sust (Neg t1) sus = Neg $ sust t1 sus
    sust (Conj t1 t2) sus = Conj (sust t1 sus) (sust t2 sus)
    sust (Disy t1 t2) sus = Disy (sust t1 sus) (sust t2 sus)
    sust (Impl t1 t2) sus = Impl (sust t1 sus) (sust t2 sus)
    sust (Equi t1 t2) sus = Equi (sust t1 sus) (sust t2 sus)
    sust (Nequ t1 t2) sus = Nequ (sust t1 sus) (sust t2 sus)
    sust t1@(Var ch) (ts1,(Sust_1 v1 ts2), v2)
       | v1 == v2 = error "Sustitución Ambigüa 2"
       | v1 == t1 = ts1
       | v2 == t1 = ts2
       | otherwise = t1
    sust bo@(Bools b1) sus = bo
    --sust _ _ = error "Error de Sustitución 2"


instance Sust (Term, Term, Sust_1, Term, Term) where
    sust (Neg t1) sus = Neg $ sust t1 sus
    sust (Conj t1 t2) sus = Conj (sust t1 sus) (sust t2 sus)
    sust (Disy t1 t2) sus = Disy (sust t1 sus) (sust t2 sus)
    sust (Impl t1 t2) sus = Impl (sust t1 sus) (sust t2 sus)
    sust (Equi t1 t2) sus = Equi (sust t1 sus) (sust t2 sus)
    sust (Nequ t1 t2) sus = Nequ (sust t1 sus) (sust t2 sus)
    sust t1@(Var ch) (ts1,ts2,(Sust_1 v1 ts3),v2, v3)
       | v1 == v2 || v1 == v3 || v2 == v3 = error "Sustitución Ambigüa 3"
       | v1 == t1 = ts1
       | v2 == t1 = ts2
       | v3 == t1 = ts3
       | otherwise = t1
    sust bo@(Bools b1) sus = bo
    --sust _ _ = error "Error de Sustitución 3"



a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'

