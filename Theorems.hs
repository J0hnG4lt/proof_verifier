module Theorems where

--Hecho por el profesor Federico Flaviani

import Terminos (Term, (===), (<==>), (!<==>), (\/), neg, true, false, p, q, r, Equation)

prop :: Float -> Equation
prop num
  | num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)     -- axiom
  | num == 3.2  = (p <==> q) === (q <==> p)                   -- axiom
  | num == 3.3  = true === p <==> p                           -- axiom
  | num == 3.4  = p === p <==> true                           -- axiom
  | num == 3.5  = (p <==> q) <==> q === p                     -- theorem
  | num == 3.6  = p === (q <==> q) <==> p                     -- theorem
  | num == 3.7  = (p <==> p) <==> (q <==> q) === true         -- theorem
  | num == 3.8  = false === neg true                          -- axiom
  | num == 3.9  = neg (p <==> q) === neg p <==> q             -- axiom
  | num == 3.101 = p !<==> q === neg (p <==> q)               -- axiom
  | num == 3.11 = neg p === q <==> (p <==> neg q)             -- theorem
  | num == 3.12 = neg (neg p) === p                           -- theorem
  | num == 3.13 = neg false === true                          -- theorem
  | num == 3.14 = p !<==> q === neg p <==> q                  -- theorem
  | num == 3.15 = neg p <==> p === false                      -- theorem
  | num == 3.16 = p !<==> q === q !<==> p                     -- theorem
  | num == 3.17 = (p !<==> q) !<==> r === p !<==> (q !<==> r) -- theorem
  | num == 3.18 = (p !<==> q) <==> r === p !<==> (q <==> r)   -- theorem
  | num == 3.19 = (p !<==> q) <==> r === p <==> (q !<==> r)   -- theorem
  | num == 3.201 = neg p <==> q === p <==> neg q              -- theorem
  | num == 3.21 = q === (p !<==> q) <==> neg p                -- theorem
  | num == 3.22 = neg p === p <==> false                      -- theorem
  | num == 3.23 = p !<==> q === r <==> (p <==> (q !<==> r))   -- theorem
  | num == 3.24 = p \/ q === q \/ p                           -- axiom
  | num == 3.25 = (p \/ q) \/ r === p \/ (q \/ r)             -- axiom
  | num == 3.26 = p \/ p === p                                -- axiom
  | num == 3.27 = p \/ (q <==> r) === p \/ q <==> p \/ r      -- axiom
  | num == 3.28 = p \/ neg p === true                         -- axiom
  | num == 3.29 = p \/ true === true                          -- theorem
  | num == 3.301 = p \/ false === p                           -- theorem
  | num == 3.31 = p \/ (q \/ r) === (p \/ q) \/ (p \/ r)      -- theorem
  | num == 3.32 = p \/ q === p \/ neg q <==> p                -- theorem
  | otherwise = error "The statement doesn't exists"
