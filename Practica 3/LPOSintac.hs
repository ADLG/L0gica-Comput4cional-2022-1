{-
LÓGICA DE PRIMER ORDEN: CONCEPTOS SINTÁCTICOS IMPORTANTES
-}

module Laboratorio6 where

import LPO

import Data.List

-- φ = ∀x∃z Q(y, z) ∨ R(z, x, y) ∧ P(z, x).


--Funcion que devuelve la lista con todos los nombres de las variables que figuran en t
-- *Laboratorio6> varT (F "h" [F "b" [], F "f" [F "a" []], V "z"])
-- ["z"]
-- *Laboratorio6> varT (F "h" [F "b" [], F "f" [F "a" []], F "z" []])
-- []
varT :: Term -> [Nombre]
varT (V x) = [x]
varT (F s []) = []
varT (F s xs) = varTL xs where
    varTL [] = []
    varTL (x:xs) = union (varT x) (varTL xs)

--Funcion que devuelve las variables libres de una formula
-- fv (All "x" (Pr "Q" [V "x",F "g" [V "y"]]))
-- ["y"]
-- fv (All "x" (Pr "Q" [V "x",F "g" [V "x"]]))
-- []
fv :: Form -> [Nombre]
fv TrueF = []
fv FalseF = []
fv (Pr p lt) = concat (map varT lt)
fv (Eq t1 t2) = union (varT t1) (varT t2)
fv (Neg f) = fv f
fv (Conj f1 f2) = union (fv f1) (fv f2)
fv (Disy f1 f2) = union (fv f1) (fv f2)
fv (Imp f1 f2) = union (fv f1) (fv f2)
fv (Equi f1 f2) = union (fv f1) (fv f2)
fv (All x f) = [y | y <- fv f, x /= y]
fv (Ex x f) = [y | y <- fv f, x /= y]


--Función bv que devuelve el conjunto(lista) con los nombre de las variables ligadas presentas en esta formula.
-- bv (All "x" (Ex "y" (Pr "P" [V "x",V "y"])))
-- ["x","y"]
-- bv (All "y" (Pr "Q" [V "x",F "g" [V "x"]]))
-- ["y"]
bv :: Form -> [Nombre]
bv TrueF = []
bv FalseF = []
bv (Pr p t) = []
bv (Eq t1 t2) = []
bv (Neg x) = bv x
bv (Conj x y) = union (bv x) (bv y)
bv (Disy x y) = union (bv x) (bv y)
bv (Imp x y) = union (bv x) (bv y)
bv (Equi x y) = union (bv x) (bv y)
bv (All s f) = union [s] (bv f)
bv (Ex s f) = union [s] (bv f)


--Función subFCuan que obtiene una lista con todas las subformulas cuantificadas de esta formula.
-- subFCuan (All "x" (Pr "Q" [V "x",F "g" [V "x"]]))
-- [All "x" (Pr "Q" [V "x",F "g" [V "x"]])]
-- subFCuan (Pr "Q" [V "x",F "g" [V "x"]])
-- []
subFCuan :: Form -> [Form]
subFCuan (Neg x) = subFCuan x
subFCuan (Conj x y) = (subFCuan x) ++ (subFCuan y) 
subFCuan (Disy x y) = (subFCuan x) ++ (subFCuan y)
subFCuan (Imp x y) = (subFCuan x) ++ (subFCuan y)
subFCuan (Equi x y) = (subFCuan x) ++ (subFCuan y)
subFCuan (All s x) = [All s x] ++ (subFCuan x)
subFCuan (Ex s x) = [Ex s x] ++ (subFCuan x)
subFCuan x = []


--Función auxiliar duplaCuan que dada una fórmula cuantificada devuelve la 3-tupla que representa el alcance de su cuantificador.
duplaCuan :: Form -> (Nombre, Form)
duplaCuan (All s x) = (s, x)
duplaCuan (Ex s x) = (s, x)

--al tomar como entrada P esta función
--devuelve una lista de pares de la forma 
--(A, B) donde A es una cuantificación 
--que es subfórmula de P y B
--es el alcance de A.
--alcF (All "x" (Pr "Q" [V "x",F "g" [V "x"]]))
-- [("x",Pr "Q" [V "x",F "g" [V "x"]])]
-- alcF (All "x" (Ex "y" (Pr "P" [V "z",V "w"])))
-- [("x",Ex "y" (Pr "P" [V "z",V "w"])),("y",Pr "P" [V "z",V "w"])]
alcF :: Form -> [(Nombre, Form)]
alcF f = map (duplaCuan) (subFCuan f)


-- Sinónimo para representar una sustitución
type Subst = [(Nombre, Term)]

--Función auxiliar verifVector que se encarga de verificar si los nombres de variables en esta lìsta de sustituciònes no aparecen còmo tèrminos que sustituiràn a variables.
-- verifVector [("x", V "x")]
-- False
-- verifVector [("x", V "y"), ("y", V "x")]
-- True
verifVector :: Subst -> Bool
verifVector [] = True
verifVector (x:xs) = (verifV (fst x) (snd x)) && (verifVector xs)

--Funciòn auxiliar verifV que verifica si una variable(un nombre) es distinta a un termino dado.
-- verifV ("x") (V "x")
-- False
-- verifV ("x") (V "y")
-- True
verifV :: Nombre -> Term -> Bool
verifV x (V y) = x /= y
verifV x (F _ _) = True


--Función auxiliar names que devuelve la lista con los nombres(variables a sustituir) que figuran en esta lista de sustituciònes(con repeticiòn de nombres si un nombre aparece màs de una vez).
-- names [("y", F "x"[ V "x"]), ("x", V "y")]
-- ["y","x"]
-- names [("y", F "x"[ V "x"]), ("x", V "y"), ("y",V "w")]
-- ["y","x","y"]
names :: Subst -> [Nombre]
names [] = []
names (x:xs) = [fst x] ++ (names xs)

--Función auxiliar namesU que hace lo mismo que la funciòn names pero no repite nombres aunque aparezcan màs de una vez.
-- namesU [("y", F "x"[ V "x"]), ("x", V "y"), ("y",V "w")]
-- ["y","x"]
namesU :: Subst -> [Nombre]
namesU [] = []
namesU (x:xs) = union [fst x] (namesU xs)


--verifSus [("x", V "y")]
--True
-- verifSus [("y", V "y")]
--False
--Funcion que verifica si la lista dada es una sustitución, es decir, que cumplan con la definición
verifSus :: Subst -> Bool
verifSus x = (verifVar x) && (verifVector x) where
verifVar x = (names x) == (namesU x)


--Funciòn auxiliar apSubTaux que aplica una lista de sustituciònes a un termino en caso de que la sustituciòn sea valida, en caso contrario lanza un mensaje de error.
apSubTaux :: Term -> Subst -> Term
apSubTaux t [] = t 
apSubTaux (V z) (x:xs) = if (z == (fst x)) then snd x else (apSubT (V z) xs)
apSubTaux (F s []) _ = F s []
apSubTaux (F s r) l = F s (apSubTlist r l) where

apSubTlist [] _ = []
apSubTlist (x:xs) l = [apSubT x l] ++ (apSubTlist xs l)


--Funciòn apSubT que sòlo es una fachada para la funciòn apSubTaux que aplica una lista de sustituciònes a un termino en caso de que la sustituciòn sea valida, en caso contrario lanza un mensaje de error.
-- apSubT (V "x") [("x", V "y")]
-- V "y"
-- apSubT (V "x") [("y", V "y")]
-- *** Exception: Sustituciónn inválida.
apSubT :: Term -> Subst -> Term
apSubT t l = if (verifSus l) then (apSubTaux t l) else error "Sustituciónn inválida."