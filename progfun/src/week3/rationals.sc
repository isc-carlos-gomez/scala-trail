package week3

object rationals {

  val x = new Rational(1, 3)                      //> x  : week3.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week3.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week3.Rational = 3/2

  x.numerator                                     //> res0: Int = 1
  x.denominator                                   //> res1: Int = 3
  x                                               //> res2: week3.Rational = 1/3
  x + y                                           //> res3: week3.Rational = 22/21
  -x                                              //> res4: week3.Rational = 1/-3
  x - y                                           //> res5: week3.Rational = 8/-21
  x - y - z                                       //> res6: week3.Rational = -79/42
  x + y * z                                       //> res7: week3.Rational = 59/42
  y + y                                           //> res8: week3.Rational = 10/7
  x < y                                           //> res9: Boolean = true
  x max y                                         //> res10: week3.Rational = 5/7

  new Rational(2)                                 //> res11: week3.Rational = 2/1
}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be positive")

  def this(x: Int) = this(x, 1)

  val numerator = x
  val denominator = y

  def < (that: Rational) =
    numerator * that.denominator < that.numerator * denominator

  def max(that: Rational) =
    if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numerator * that.denominator + denominator * that.numerator,
      denominator * that.denominator)

  def unary_- = new Rational(-numerator, denominator)

  def - (that: Rational) = this + -that

  def * (other: Rational) =
    new Rational(
      numerator * other.numerator,
      denominator * other.denominator)

  override def toString = {
    def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
    val g = gcd(numerator, denominator)
    numerator / g + "/" + denominator / g
  }
}