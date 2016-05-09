{-# LANGUAGE FlexibleInstances #-}
module Main where

{-
    Estudiante: Georvic Tur
    Carnet: 12-11402
    
    Correo: alexanderstower@gmail.com
-}

data Term = Var Char
          | Bools Bool
          | Neg Term
          | Conj Term Term
          | Disy Term Term
          | Impl Term Term
          | Equi Term Term
          | Nequ Term Term

instance Show Term where
    show (Bools bool) = show bool
    show (Var ch) = show ch
    show (Neg t1) = "("++ show t1 ++")"
    show (Conj t1 t2) = "("++ show t1 ++ " /\\ " ++ show t2 ++ ")"
    show (Disy t1 t2) = "("++ show t1 ++ " \\/ " ++ show t2 ++ ")"
    show (Impl t1 t2) = "("++ show t1 ++ " ==> " ++ show t2 ++ ")"
    show (Equi t1 t2) = "("++ show t1 ++ " <==> " ++ show t2 ++ ")"
    show (Nequ t1 t2) = "("++ show t1 ++ "!<==>" ++ show t2 ++ ")"

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
    sust t1@(Var ch) (ts1,(Sust_1 v2 ts2), v1)
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
    sust t1@(Var ch) (ts1,ts2,(Sust_1 v3 ts3),v2, v1)
       | v1 == v2 || v1 == v3 || v2 == v3 = error "Sustitución Ambigüa 3"
       | v1 == t1 = ts1
       | v2 == t1 = ts2
       | v3 == t1 = ts3
       | otherwise = t1
    sust bo@(Bools b1) sus = bo
    --sust _ _ = error "Error de Sustitución 3"

instantiate :: Sust a => Equation -> a -> Equation
instantiate (Equation t1 t2) sus = Equation (sust t1 sus) (sust t2 sus)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equation t1 t2) (ee) zz@(Var ch)
        = Equation (sust ee sus1) (sust ee sus2)
            where sus1 = Sust_1 zz t1
                  sus2 = Sust_1 zz t2
leibniz _ _ _ = error "Argumentos inválidos en leibniz"


infer :: Sust a => Float -> a -> Term -> Term -> Equation
infer num sus zz@(Var c1) ee =
    let teorema = prop num
        premisa = instantiate teorema sus
    in leibniz premisa ee zz
infer num sus _ ee = error "Argumentos inválidos en infer"

choose :: Term -> Term -> Term  -> Term
choose t1 rt1 rt2
    | t1 == rt1 = rt2
    | t1 == rt2 = rt1
    | otherwise = error $ "Error de Deducción \nt1:"++ show t1
                            ++ " \nrt1:"++show rt1 ++ " \nrt2:"++ show rt2 


step :: Sust a => Term -> Float -> a -> Term -> Term -> Term
step t1 num sus zz@(Var ch) ee =
    let (Equation rt1 rt2) = infer num sus zz ee
    in choose t1 rt1 rt2
step t1 num sus _ ee = error "Argumentos inválidos en step"

with :: ()
with = ()
lambda :: ()
lambda = ()
using :: ()
using = ()


statement :: (Sust a, Show a) => Float -> () -> a -> () -> () -> Term -> Term -> Term -> IO Term
statement num _ sus _ _ zz@(Var ch) ee = \term_arg ->
    let paso = (step term_arg num sus zz ee)
        linea = "=== <statement "++ show num ++ " with "++ show sus ++ " using lambda " ++ show zz ++ " (" ++ show ee ++")>"
    in putStrLn linea >> putStrLn (show paso) >> return paso
statement _ _ _ _ _ _ _ = error "Argumentos inválidos en statement"

prop :: Float -> Equation
prop num
    | num == 3.1 = (p <==> q) <==> r === p <==> (q <==> r)
    | num == 3.2 = (p <==> q) <==> (q <==> p) === true
    | num == 3.3 = (p <==> q) <==> q === p
    | otherwise = error "El statement no existe"


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

proof :: Equation -> IO Term
proof teo@(Equation t1 t2) = 
    putStrLn ("prooving " ++ show teo) 
    >>
    putStrLn ""
    >>
    putStrLn (show t1)
    >> 
    return t1

done :: Equation -> Term -> IO Term
done (Equation t1 t2) t3 = 
    if t2 == t3
    then putStrLn "\nQ.E.D." >> return (Var ' ')
    else putStrLn "Error en demostración" >> return (Var ' ')

verify' :: IO Term
verify' = return true
          >>=
          statement 3.2 with (p=:p) using lambda z (z)
          >>=
          statement 3.1 with (p <==> q,p=:p,r) using lambda z (z)
          >>=
          statement 3.3 with (p=:p) using lambda z (z <==> p)

verify :: IO Term
verify = let theorem = (true === ((p <==> p) <==> (q <==> q))) in
         proof theorem
         >>=
         statement 3.2 with (p=:p) using lambda z (z)
         >>=
         statement 3.1 with (p <==> q,p=:r,p) using lambda z (z)
         >>=
         statement 3.3 with (p=:p) using lambda z (z <==> p)
         >>=
         statement 3.3 with (p=:p) using lambda z (p <==> z)
         >>=
         statement 3.1 with (p <==> q,q=:r,q) using lambda z (z)
         >>=
         statement 3.1 with (q,p=:q,r) using lambda z (z <==> q)
         >>=
         statement 3.1 with (p <==> p,q=:r,p) using lambda z (z)
         >>=
         done theorem

main ::  IO ()
main = putStrLn ""
