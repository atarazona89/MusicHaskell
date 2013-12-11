module Main where

import Prelude hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function

{- Esto es lo que yo (Alejandro) he agregado
 - tomando en cuenta los modelos que debemos generar
 - me di cuenta que debemos manejar dos tipos nuevos
 - de datos, los llamé "modeloK1" (modelo de probabilidad
 - de ocurrencia somple) y modeloK2 (modelo de probabilidad
 - de ocurrencia condicional).
 -}

type Proba = (Evento, Integer)		-- Este es un tipo auxiliar para poder visualizar las operaciones a realizar

eveIgual :: Evento -> Proba -> Bool
eveIgual e p = (e == fst p)

type ModeloK1 = [Proba]	-- Un evento y su proba de ocurrir

listEvent:: ModeloK1 -> [Evento]
listEvent m = map fst m

igual :: Evento -> Evento -> Bool
igual a b = (((fst a) == (fst b)) && ((snd a) == (snd b)))

eveEnMod ::ModeloK1 -> Evento -> Bool
eveEnMod m e = head $ filter (==True) (map (eveIgual e) m)   -- Ya ni se que me vendieron por cigarros en la panaderia.



type ModeloK2 = [(Evento, Proba)] -- Un evento y su proba de ocurrir dado un evento previo

addEvent:: ModeloK1 -> Evento -> ModeloK1
addEvent m e = if (elem e $ listEvent m) 
		then (filter (not.eveIgual e) m) ++ [(e , (snd.head $ (filter (eveIgual e) m)) + 1)]
		else m ++ [(e,1)]


-- addEvent:: (Evento a) => modeloK2 -> a -> a -> odeloK2

-- Directorio predeterminado
directorio :: String
directorio = "./xml/"

-- Longitud de las secuencias musicales generadas
longitud :: Int
longitud = 50

{- Induce un modelo de contexto a partir de la colección musical 
   en el directorio por defecto, genera una secuencia musical 
   nueva a partir de este modelo, la imprime por pantalla y la 
   reproduce.
   -}
componer :: IO ()
componer = componer' directorio

componer' :: String -> IO ()
componer' dir = do
  (seqs, filenames) <- loadMusicXmls dir
  -- let modelo = ...
  let composicion = (seqs, filenames)
  putStrLn $ show composicion
  --play $ sequenceToMusic composicion

{- Recupera las diez secuencias más similares a la k-ésima secuencia 
   de la colección musical en el directorio por defecto, donde la 
   colección musical ha sido ordenada en orden alfabético por el 
   nombre de archivo. Imprime una lista ordenada de las diez 
   secuencias más similares. En cada fila de la lista se debe indicar 
   el número de la secuencia (relativo al orden alfabético de la 
   colección), el nombre de archivo y la distancia a la consulta.
   -}
{- buscar :: Int -> IO ()
buscar = buscar' directorio
  
buscar' :: String -> Int -> IO ()
buscar' dir = do
  seqfns <- loadMusicXmls dir
  let seqfns_ordenados = unzip $ sortBy (compare `on` snd) $ zip seqfns
  -- ...
-}
tocar :: Int -> IO ()
tocar n = do
  seqfns <- loadMusicXmls directorio
  let (seqs, filenames) = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  if (n > 0) && (n <= length seqs) then
    putStrLn (filenames !! (n-1)) >>
    play (sequenceToMusic (seqs !! (n-1)))
    else
      putStrLn "Indice fuera de rango"
          
eventToNote :: Evento -> Music Note1
eventToNote e = note
  where
  d = (fromIntegral $ snd e) / 16
  p = Euterpea.pitch $ fst e
  note = Prim (Note d (p,[]))
  
sequenceToMusic :: [Evento] -> Music Note1
sequenceToMusic es = line $ map eventToNote es

main :: IO()
main = componer
