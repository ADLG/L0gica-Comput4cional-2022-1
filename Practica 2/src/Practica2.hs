{-
-- | Lógica Computacional 2022-01
-- | Práctica 2: Forma normal negativa y conjuntiva
-- | Profesor: Dr. FEMP
-- | Ayudante: JEM
-- | Ayudante: RAA
-- | Laboratorio: DLC

-- | Alumno: ADLG.
-}

module Practica2 where

import LProp
import Data.List

-- |1| Funcion fnn que recibe una formula φ y devuelve su forma normal negativa
fnn :: Prop -> Prop
fnn (P x) = (P x)
fnn (Neg x) = fnnax x
fnn (Or x y) = Or (fnn x) (fnn y)
fnn (And x y) = And (fnn x) (fnn y)
fnn (Impl x y) = fnn(elimImp(Impl (fnn x) (fnn y)))
fnn (Syss x y) = fnn(elimImp(elimEquiv(Syss (fnn x) (fnn y))))

-- Funcion auxiliar que recibe una formula φ y devuelve la formula aplicandole las leyes negacion.
fnnax :: Prop -> Prop
fnnax (P x) = Neg(P x)
fnnax (Neg x) = fnn x
fnnax (Or x y) = And (fnnax x) (fnnax y)
fnnax (And x y) = Or (fnnax x) (fnnax y)
fnnax (Impl x y) = fnnax(elimImp(Impl (fnnax x) (fnnax y)))
fnnax (Syss x y) = fnnax(elimImp(elimEquiv(Syss (fnnax x) (fnnax y))))


-- |2| Funcion distr la cual aplica adecuadamente las leyes distributivas a una formula proposicional
distr :: Prop -> Prop
distr (Or (And x y) z) = distr (And (Or (distr x)(distr z)) (Or (distr y)(distr z)))
distr (Or a (And b c)) = distr (And (Or (distr a)(distr b)) (Or (distr a)(distr c)))
distr (And z a) = And (distr z)(distr a)
distr (Or z a) = if saberOp (Or z a) then distr (Or (distr z)(distr a)) else Or (distr z)(distr a)
distr z = z

-- Funcion que recibe una formula φ y nos hace saber si dicha formula tiene o no una conjuncion.
saberOp :: Prop -> Bool
saberOp (And x y) = True
saberOp (Or x y) = (saberOp x) || (saberOp y)
saberOp _ = False

-- |3| Funcion fnc que recibe una formula φ y devuelve su forma normal conjuntiva, es decir:
--     Permite expresar cualquier formula proposicional como una conjunción de disyunciones.
-- IMPORTANTE: Se puede suponer que la formula de entrada ya se encuentra en forma normal negativa
fnc :: Prop -> Prop
fnc x = distr (fnn x)

--Definimos un tipo de dato Literal como sinónimo de fórmula proposicional
type Literal = Prop

-- Funcion auxiliar que recibe una foruma φ y nos dice si tiene literales.
literalax :: Prop -> Bool
literalax (P x)       = True
literalax (Neg (P x)) = True
literalax (Impl (P x) (P y)) = True
literalax (Syss (P x) (P y)) = True
literalax _ = False

--Definimos un tipo de dato Clausula como una lista de literales
type Clausula = [Literal]

-- |4| Función ctolist recibe una proposicion y devuelve la forma clausular de la proposicion
ctolist :: Prop -> Clausula
ctolist x | literalax x = [x]
ctolist (Or f g) = ordenaax ((ctolist f) `union` (ctolist g))

-- Funcion auxiliar que ordena los elementos de una lista.
ordenaax :: Ord a => [a] -> [a]
ordenaax [] = []
ordenaax (x:xs) = ordenaax(mayor) ++ [x] ++ ordenaax(menor)
 where
 menor = [a | a <- xs, a < x]
 mayor = [b | b <- xs, b >= x]

-- |5| Función fncC recibe una fórmula en forma normal conjuntiva y debe devolver su conversión en una lista de cláusulas.
fncC :: Prop -> [Clausula]
fncC (And x y) = (fncC x) `union` (fncC y)
fncC x = [ctolist x]

-- |6| Funcion fncConj recibe una proposicion y devuelve el conjunto de clausulas equivalentes a esa proposicion
fncConj :: Prop -> [Clausula]
fncConj x = fncC (fnc x)

{--
PUNTOS EXTRA
--}
-- |1| Función argcorrecto que verifica si un argumento con premisas y una conclusión es lógicamente correcto
argcorrecto :: [Prop] -> Prop -> Bool
argcorrecto = error"---"

