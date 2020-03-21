//object session {
//  def abs(x: Double) = if (x < 0) -x else x
//
//  def sqrt(x: Double) = {
//    def sqrtIter(guess: Double): Double =
//      if (isGoodEnough(guess)) guess
//      else sqrtIter(improve(guess))
//
//    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.0001
//
//    def improve(guess: Double) = (guess + x / guess) / 2
//
//    sqrtIter(1.0)
//  }
//
//  sqrt(13)
// }
object session {
  def factorial(n: Int): Int = {
    def auxFactorial(n: Int, x: Int): Int = {
      if (n == 0 || n ==1)
        x
      else
        (auxFactorial(n-1, n*x))
    }

    auxFactorial(n, 1)
  }

  factorial(5)
}
