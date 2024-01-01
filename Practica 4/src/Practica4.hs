{-
-- | Lógica Computacional 2022-01
-- | Práctica 4: Unificación
-- | Profesor: Dr. FEMP
-- | Ayudante: JEM
-- | Ayudante: RAA
-- | Laboratorio: DLC

-- | Alumno: ADLG.
-}

module Practica4 where

import LPO
import LPOSintac
import Data.List

-- |1| Funcion simpSus que dada una sustitución elimina de ella los pares con componentes iguales
simpSus :: Subst -> Subst
simpSus [] = []
simpSus (x:xs) = if verifSus([x]) == False then eElem (x:xs) x else x:simpSus(xs)

-- Funcion para eliminar un elemento de una lista.
eElem :: (Eq a) => [a] -> a -> [a]
eElem l n = [ x | x <- l , x /= n]

-- Funcion para obtener el nombre de una variable de una sustitucion.
nombreVS :: Subst -> [Nombre]
nombreVS = nub . map fst

-- Funcion que aplica una sustitucion a una variable.
susVar :: Subst -> Nombre -> Term
susVar [] y = V y
susVar ((x,x1):xs) y | x == y = x1 | otherwise = susVar xs y

-- Funcion que aplica una sustitucion con terminos
susTerm :: Subst -> Term -> Term
susTerm s (V y) = susVar s y
susTerm s (F f ts) = F f (susListTe s ts)

-- Funcion que aplica una sustitucion entre listas de terminos
susListTe :: Subst -> [Term] -> [Term]
susListTe = map . susTerm

-- |2| Funcion compSus la cual recibe dos Subst y devuelve su compisición.
compSus :: Subst -> Subst -> Subst
compSus l1 l2 = simpSus [(y,susTerm l2 y') | (y,y') <- l1 ] ++ [x | x <- l2, fst x `notElem` nombreVS l1]

-- |3| Funcion que dados dos términos devuelva una lista de sustituciones que cumplan las condiciones dadas
unifica :: Term -> Term -> [Subst]
unifica (V x) (V y) = if x == y then [] else [[(x,V y)]]
unifica (V x) t = [[(x,t)] | x `notElem` varT t]
unifica t (V y) = [[(y,t)] | y `notElem` varT t]
unifica (F f ts) (F g rs) = [u | f == g, u <- unificaListas ts rs]

-- |4| Funcion que unifica dos listas de términos de la misma longitud componente a componente
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = []
unificaListas (t:ts) (r:rs) = [nub (compSus x1 x2) | x1 <- unifica t r , x2 <- unificaListas (susListTe x1 ts) (susListTe x1 rs)]

-- |5| Funcion unificaConj que implementa el caso general para unificar un conjunto (lista)
unificaConj :: [Term] -> [Subst]
unificaConj = error "Implementar"

-- |6| Funcion que inifica dos literales
unificaLit :: Form -> Form -> [Subst]
unificaLit = error "Implementar"


phi = [("x", F "f" [V "y"]), ("y", V "z")]
gamma = [("x", F "g" [V "w"]), ("z", V "m"), ("z", V "w")]
theta = [("y", V "m"), ("w", F "f" [V "n"]), ("v", V "w")]
