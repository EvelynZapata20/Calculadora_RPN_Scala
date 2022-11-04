object Calculadora extends App { //Crea el objeto que extiende la clase App, la cual define el método principal

  def solveRPN(cadena: String): Double = { //Establece que la función recibirá una cadena y retornará un double
    val words = cadena.split(" ") //Crea una variable con la función split, que separa las palabras de la cadena ingresada
    val list = List[Double]() //Crea una variable inmutable lista que contendrá valores double
    words.foldLeft(list)(newList).head //La función foldLeft es un pliegue que recorrerá la lista de izquierda a derecha
  }
   
  /*La función newList, recibe la pila y la cadena creadas, con las cuales se define una serie de casos a los que se envían,
  stack.tail.head para definir el primer elemento de la pila, stack.tail.tail para definir el resto de los elementos
  y stack para enviar todos los elemtentos de la pila*/
  def newList (stack: List[Double], a: String): List[Double] = { 
    a match {
      case "*" => stack.tail.head * stack.head :: stack.tail.tail
      case "+" => stack.tail.head + stack.head :: stack.tail.tail
      case "-" => stack.tail.head - stack.head :: stack.tail.tail
      case "/" => stack.tail.head / stack.head :: stack.tail.tail
      case "neg1" => (stack.head*(-1)) :: stack.tail
      case "raiz2" => Math.sqrt(stack.head) :: stack.tail
      case "condnumero" => condnumero (stack.head) :: stack.tail   
      case "sum" => stack.sum :: stack.tail
      case "producto" => stack.product :: stack.tail
      case "promedio" => stack.sum / stack.length :: stack.tail
      case _ => a.toDouble :: stack //Convierte a double, como se ha definido en la función
    }
  }

   //Función que recibe un Double y retorna otro Double, cuyo valor dependerá del valor recibido 
  def condnumero (a: Double) = {
    if (a==3) {100}
    else if (a==5) {25}
    else {0}
  }

  //Menú de prueba    
  println ("----------------------------------------------------------")
  println ("            BIENVENIDO A LA CALCULADORA DE RPN            ")
  println ("----------------------------------------------------------")
  print ("RPN '20 6 4 + +' = ")
  print (solveRPN ("20 6 4 + +"))
  print ("\nRPN '20 6 4 - -' = ")
  print (solveRPN ("20 6 4 - -"))
  print ("\nRPN '20 6 4 * *' = ")
  print (solveRPN ("20 6 4 * *"))
  print ("\nRPN '20 6 4 / /' = ")
  print (solveRPN ("20 6 4 / /"))
  print ("\nRPN '10 67 neg1' = ")
  print (solveRPN ("10 67 neg1"))
  print ("\nRPN '10 67 neg1 +' = ")
  print (solveRPN ("10 67 neg1 +"))
  print ("\nRPN '10 67 neg1 *' = ")
  print (solveRPN ("10 67 neg1 *"))
  print ("\nRPN '10 16 raiz2' = ")
  print (solveRPN ("10 16 raiz2"))
  print ("\nRPN '10 16 raiz2 +' = ")
  print (solveRPN ("10 16 raiz2 +"))
  print ("\nRPN '10 5 condnumero' = ")
  print (solveRPN ("10 5 condnumero"))
  print ("\nRPN '10 5 condnumero -' = ")
  print (solveRPN ("10 5 condnumero -"))
  print ("\nRPN '10 3 condnumero +' = ")
  print (solveRPN ("10 3 condnumero +"))
  print ("\nRPN '10 2 condnumero +' = ")
  print (solveRPN ("10 2 condnumero +"))
  print ("\nRPN '10 67 15 sum' = ")
  print (solveRPN ("10 67 15 sum"))
  print ("\nRPN '10 16 10  producto' = ")
  print (solveRPN ("10 16 10 producto"))
  print ("\nRPN '120 20 40 120 promedio' = ")
  print (solveRPN ("120 20 40 120 promedio"))
  println ("\n----------------------------------------------------------")
  println (" PROGRAMA FINALIZADO, GRACIAS POR USAR LA CALCULADORA RPN ")
  println ("----------------------------------------------------------")
}