package week2

import math.abs

object fixedPoint {

  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance              //> isCloseEnough: (x: Double, y: Double)Boolean

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  fixedPoint(x => 1 + x / 2)(1)                   //> res0: Double = 1.999755859375

	def sqrt1(x: Double): Double =
  	fixedPoint(y => (y + x / y) / 2)(1)       //> sqrt1: (x: Double)Double
  sqrt1(4)                                        //> res1: Double = 2.000000000000002

  def averageDamp(f: Double => Double)(x: Double) =
  	(x + f(x)) / 2                            //> averageDamp: (f: Double => Double)(x: Double)Double
  val x = 1                                       //> x  : Int = 1
  averageDamp(y => x / y)(4)                      //> res2: Double = 2.125
  
  def sqrt(x: Double): Double =
  	fixedPoint(averageDamp(y => x / y))(1)    //> sqrt: (x: Double)Double
  sqrt(4)                                         //> res3: Double = 2.000000000000002

}