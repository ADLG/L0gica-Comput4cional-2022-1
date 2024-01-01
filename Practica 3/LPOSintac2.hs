module LPOSintac2 where

import Data.List
import Data.Char
import Data.String

import LPO
import LPOSintac


-- apsubF (All "z" (Pr "P" [V "x"])) [("x", V "y")]
-- All "z" (Pr "P" [V "y"])
-- apsubF (All "z" (Pr "P" [V "x", F "f" [V "t"]])) [("x", V "y")]
-- All "z" (Pr "P" [V "y",F "f" [V "t"]])
apsubF :: Form -> Subst -> Form
apsubF f ls = if (verifSus ls) then (apsubFaux f ls) else error "Sustitución inválida."


--Función auxiliar apsubFaux que se encarga de realmente aplicar una lista de sustitucines a una formula 
--en caso de que la sustitucion sea valida y se pueda realizar en caso contrario lanza un mensaje de error.
apsubFaux :: Form -> Subst -> Form
apsubFaux TrueF _ = TrueF
apsubFaux FalseF _ = FalseF
apsubFaux (Pr s r) l = Pr s (apsubFlist r l)
apsubFaux (Eq x y) l = Eq (apSubT x l) (apSubT y l)
apsubFaux (Neg x) l = Neg (apsubF x l)
apsubFaux (Conj x y) l = Conj (apsubF x l) (apsubF y l)
apsubFaux (Disy x y) l = Disy (apsubF x l) (apsubF y l)
apsubFaux (Imp x y) l = Imp (apsubF x l) (apsubF y l)
apsubFaux (Equi x y) l = Equi (apsubF x l) (apsubF y l)
apsubFaux (All s x) l = if ((notElem s (namesU l)) && (notElem (V s) (terms l))) then (All s (apsubFaux x l)) else error "No es posible  hacer la sustitución."
apsubFaux (Ex s x) l = if ((notElem s (namesU l)) && (notElem (V s) (terms l))) then (Ex s (apsubFaux x l)) else error "No es posible hacer la sustitución." where

apsubFlist [] _ = []
apsubFlist (x:xs) l = [apSubT x l] ++ (apsubFlist xs l)

--Función auxiliar terms que devuelve la lista con los terminos que figuran en la segunda entrada de una lista de sustituciones
--(si un elemento figura mas de una vez sólo se considera una en la lista.).
-- terms [("x", V "y"),("z", V "w")]
-- [V "y",V "w"]
terms :: Subst -> [Term]
terms [] = []
terms (x:xs) = union [snd x] (terms xs)


-- *LPOSintac2> vAlfaEq (Imp (All "x" (Pr "P" [V "x", V "y"])) (Ex "y" (Pr "R" [V "x", V "y", V "z"]))) (Imp (All "w" (Pr "P" [V "w", V "y"])) (Ex "v" (Pr "R" [V "x", V "v", V "z"]))) 
-- True
-- *LPOSintac2> vAlfaEq (Imp (All "w" (Pr "P" [V "w", V "y"])) (Ex "v" (Pr "R" [V "x", V "v", V "z"]))) (Imp (All "z" (Pr "P" [V "z", V "y"])) (Ex "u" (Pr "R" [V "x", V "u", V "z"])))
-- True
-- vAlfaEq (All "x" (Pr "P" [V "x", V "y"])) (Ex "y" (Pr "R" [V "x", V "y", V "z"]))
-- False
vAlfaEq :: Form -> Form -> Bool
vAlfaEq (Pr n1 lt1) (Pr n2 lt2) = (n1 == n2) && (lt1 == lt2)
vAlfaEq (Eq t1 t2) (Eq t1' t2') = (t1 == t1') && (t2 == t2')
vAlfaEq (Neg f1) (Neg f2) = vAlfaEq f1 f2
vAlfaEq (Conj f1 f2) (Conj f1' f2') = (vAlfaEq f1 f1') && (vAlfaEq f2 f2')
vAlfaEq (Disy f1 f2) (Disy f1' f2') = (vAlfaEq f1 f1') && (vAlfaEq f2 f2')
vAlfaEq (Imp f1 f2) (Imp f1' f2') = (vAlfaEq f1 f1') && (vAlfaEq f2 f2')
vAlfaEq (Equi f1 f2) (Equi f1' f2') = (vAlfaEq f1 f1') && (vAlfaEq f2 f2')
vAlfaEq (All n f) (All n' f') = vAlfaEq f (apsubF f' [(n', V n)])
vAlfaEq (Ex n f) (Ex n' f') = vAlfaEq f (apsubF f' [(n', V n)])
vAlfaEq _ _ = False


-- renVLConj (All "x" (Pr "P" [V "x"])) ["x"]
-- All "x1" (Pr "P" [V "x1"])
-- renVLConj (All "y" (Pr "P" [V "x"])) ["y"]
-- All "y1" (Pr "P" [V "x"])
renVLConj :: Form -> [Nombre] -> Form
renVLConj (Pr n lt) xs = (Pr n lt)
renVLConj (Eq t1 t2) xs = (Eq t1 t2)
renVLConj (Neg f1) xs = renVLConj f1 xs
renVLConj (Conj f1 f2) xs = Conj (renVLConj f1 xs) (renVLConj f2 xs)
renVLConj (Disy f1 f2) xs = Disy (renVLConj f1 xs) (renVLConj f2 xs)
renVLConj (Imp f1 f2) xs = Imp (renVLConj f1 xs) (renVLConj f2 xs)
renVLConj (Equi f1 f2) xs = Equi (renVLConj f1 xs) (renVLConj f2 xs)
renVLConj (All x f) xs
    | elem x xs = (All z f2)
    | otherwise = (All x (renVLConj f xs))
    where z = newVar xs
          f2 = renVLConj (apsubF f [(x, V z)]) xs
renVLConj (Ex x f) xs
    | elem x xs = (Ex z f2)
    | otherwise = (Ex x (renVLConj f xs))
    where z = newVar xs
          f2 = renVLConj (apsubF f [(x, V z)]) xs

newVar :: [Nombre] -> Nombre
newVar xs = find' (head xs) xs

find' :: Nombre -> [Nombre] -> Nombre
find' x xs = let x' = (incrVar x)
            in 
              if elem x' xs
              then find' x' xs
              else x'

{--
Función incrVar: dado un identificador renombra la cadena de caracteres
si termina en cualquier char agrega un "1" al final de la cadena
suma 1 si el ultimo caracter de la cadena termina en numero
si string = char -> return: char1 
si string = char1 -> return: char2
--}
incrVar :: Nombre -> Nombre
incrVar x 
    | (last x == '9') = incrVar(take ((length x) -1) x) ++ '0':[]
    | (isDigit(last x)) = let x1 = ((getInt(last x))+1) in take ((length x) -1) x ++ (intToDigit x1):[]
    | otherwise = x ++ "1"

{--
Función auxiliar getInt: dado un char lo convierte a integer
return: un entero si su entrada en un char
--}
getInt :: Char -> Int
getInt x = digitToInt x


renVlL :: Form -> Form
renVlL f = renVLConj f (fv f)