{-
-- | Lógica Computacional 2022-01
-- | Práctica 1: Introducción a Haskell 
-- | Profesor: Dr. FEMP
-- | Ayudante: JEM
-- | Ayudante: RAA
-- | Laboratorio: DLC

-- | Alumno: ADLG.
-}

module Practica1 where 

-- | Tipo de dato Binario, es la representacion de todos los numero binarios que empiezan con uno
data Binario = U | Cero Binario | Uno Binario 

-- |1| Definicion de la clase Show para el tipo de dato Binario
instance Show Binario where
    show U = "1"
    show (Cero a) = show a ++ "0"
    show (Uno a) = show a ++ "1"

-- |2| sucesor. Regresa el sucesor de un Binario
-- -> Ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor :: Binario -> Binario
sucesor U = Cero U
sucesor (Cero a) = Uno a
sucesor (Uno a) = Cero(sucesor a)

-- |3| suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma U U = Cero U
suma (Cero U) (Cero U) = Cero(Cero U)
suma (Uno U) (Uno U) = Cero(Uno U)
suma (Cero U) (Uno a) = Uno(Cero a)
suma (Uno U) (Uno a) = Cero(Uno a)
suma (Cero b) a = (suma a b)
suma (Uno b) a = Uno(suma a b)

-- |4| producto. Regresa el producto de 2 numeros Binarios
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
producto U U = U
producto U a = a
producto a U = a
producto (Cero U) (Cero a) = Cero(Cero(producto U a))
producto (Cero U) (Uno a) = Cero(Uno(producto U a))
producto (Uno U) (Uno a) = Uno(Cero(Cero(producto U a)))
producto (Uno U) (Cero a) = Cero(producto a a)


-- |5| natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 1 = [1]
natBinLista n = reversa (nauxBin n)

-- Función auxiliar que coniverte un numero de tipo Int a una lista con su representacion en Binario
nauxBin :: Int -> [Int]
nauxBin 0 = []
nauxBin n = n `mod` 2 : nauxBin (div n 2)

-- Función auxiliar para la reversa de una Lista
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]


-- |6| sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista [1] [1] = Cero U
sumaBinLista [1,0] [1] = (Uno U) 
sumaBinLista [1,0] [1,0] = suma (Cero U) (Cero U)
sumaBinLista [1,0] [1,1] = suma (Uno U) (Uno U)

-- Función que pretendia representar una Lista de enteros en su notacion Binaria
auxsumaBin :: [Int] -> Binario
auxsumaBin [1] = U
auxsumaBin [1,0] = Cero U
auxsumaBin [1,1] = Uno U
auxsumaBin [1,0,0] = Cero(Cero U)
auxsumaBin [1,0,1] = Uno(Cero U)
auxsumaBin [1,1,0] = Cero(Uno U)
auxsumaBin [1,1,1] = Uno(Uno U)


{-- PUNTOS EXTRA --}

-- |1| natABin: Recibe un natural (mayor que 1) y devuelve un binario
natABin :: Int -> Binario
natABin = error "Implementar"

-- |2| binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat = error "Implementar"

-- |3| predecesor: Recibe un binario y devuelve su binario anterior
predecesor :: Binario -> Binario
predecesor = error "Implementar"