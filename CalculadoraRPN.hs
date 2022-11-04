import Data.List --Importa la librería que contiene las funciones para operar listas

solveRPN :: String -> Float --Establece que la función recibirá una cadena y retornará un valor flotante
{- words crea una nueva lista conformada por las palabras de la cadena
fold es un pliegue que recorre la lista de izquierda a derecha -}
solveRPN = head.foldl newList[].words
  {- el patrón (x:y:ys), liga los dos primeros elementos de la lista a variables y el resto a otra variable
  el patrón (x:ys), liga la cabeza de la lista con x y el resto con ys
  xs envía todos los datos de la lista a la función -}
  where newList (x:y:ys) "*" = (x * y):ys  
        newList (x:y:ys) "+" = (x + y):ys
        newList (x:y:ys) "-" = (y - x):ys
        newList (x:y:ys) "/" = (y / x):ys
        newList (x:ys) "neg1" = (x*(-1)):ys
        newList (x:ys) "raiz2" = sqrt x:ys
        newList (x:ys) "condnumero" = condnumero x:ys
        newList xs "sum" = [sum xs]
        newList xs "producto" = [product xs]
        newList xs "promedio" = (sum xs / fromIntegral (length xs)):xs
        newList xs numberString = read numberString:xs --Permite leer la lista

--Función que recibe un Float y retorna otro Float, cuyo valor dependerá del valor recibido
condnumero :: Float -> Float
condnumero num
   | num == 3 = 100
   | num == 5 = 25
   | otherwise = 0
 
--Menú de prueba
main = do
  putStrLn "----------------------------------------------------------"
  putStrLn "            BIENVENIDO A LA CALCULADORA DE RPN            "
  putStrLn "----------------------------------------------------------"
  putStr "RPN '20 6 4 + +' = "
  print (solveRPN "20 6 4 + +")
  putStr "RPN '20 6 4 - -' = "
  print (solveRPN "20 6 4 - -")
  putStr "RPN '20 6 4 * *' = "
  print (solveRPN "20 6 4 * *")
  putStr "RPN '20 6 4 / /' = "
  print (solveRPN "20 6 4 / /")
  putStr "RPN '10 67 neg1' = "
  print (solveRPN "10 67 neg1")
  putStr "RPN '10 67 neg1 +' = "
  print (solveRPN "10 67 neg1 +")
  putStr "RPN '10 67 neg1 *' = "
  print (solveRPN "10 67 neg1 *")
  putStr "RPN '10 16 raiz2' = "
  print (solveRPN "10 16 raiz2")
  putStr "RPN '10 16 raiz2 +' = "
  print (solveRPN "10 16 raiz2 +")
  putStr "RPN '10 5 condnumero' = "
  print (solveRPN "10 5 condnumero")
  putStr "RPN '10 5 condnumero -' = "
  print (solveRPN "10 5 condnumero -")
  putStr "RPN '10 3 condnumero +' = "
  print (solveRPN "10 3 condnumero +")
  putStr "RPN '10 2 condnumero +' = "
  print (solveRPN "10 2 condnumero +")
  putStr "RPN '10 67 15 sum' = "
  print (solveRPN "10 67 15 sum")
  putStr "RPN '10 16 10  producto' = "
  print (solveRPN "10 16 10 producto")
  putStr "RPN '120 20 40 120 promedio' = "
  print (solveRPN "120 20 40 120 promedio")
  putStrLn "----------------------------------------------------------"
  putStrLn " PROGRAMA FINALIZADO, GRACIAS POR USAR LA CALCULADORA RPN "
  putStrLn "----------------------------------------------------------"



