module ProofVerifier where
import Terminos
{-
    Estudiante: Georvic Tur
    Carnet: 12-11402
    
    Correo: alexanderstower@gmail.com
-}



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
         statement 3.1 with (p <==> q,p=:p,r) using lambda z (z)
         >>=
         statement 3.3 with (p=:p) using lambda z (z <==> p)
         >>=
         statement 3.3 with (p=:p) using lambda z (p <==> z)
         >>=
         statement 3.1 with (p <==> q,q=:q,r) using lambda z (z)
         >>=
         statement 3.1 with (q,p=:r,q) using lambda z (z <==> q)
         >>=
         statement 3.1 with (p <==> p,q=:p,r) using lambda z (z)
         >>=
         done theorem


