package week4

object patterns {
  val two = Number(2)                             //> two  : week4.Number = Number(2)
  val sum = Sum(two, two)                         //> sum  : week4.Sum = Sum(Number(2),Number(2))
  val x = Var("x")                                //> x  : week4.Var = Var(x)
  val y = Var("y")                                //> y  : week4.Var = Var(y)

  two.eval                                        //> res0: Int = 2
  sum.eval                                        //> res1: Int = 4

  two.show                                        //> res2: String = 2
  sum.show                                        //> res3: String = 2 + 2

  x.show                                          //> res4: String = x
  y.show                                          //> res5: String = y

  Sum(Prod(two, x), y).show                       //> res6: String = 2 * x + y
  Prod(Sum(two, x), y).show                       //> res7: String = (2 + x) * y
  Sum(Sum(two, x), y).show                        //> res8: String = 2 + x + y
  Prod(Sum(two, Prod(x, y)), two).show            //> res9: String = (2 + x * y) * 2
}

trait Expr {
  def eval: Int = this match {
    case Number(n)   => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

  def show: String = this match {
    case Number(n)    => "" + n
    case Sum(e1, e2)  => e1.show + " + " + e2.show
    case Var(name)    => name
    case Prod(e1, e2) => e1.showNested + " * " + e2.showNested
  }

  def showNested: String = this match {
    case Sum(e1, e2) => "(" + this.show + ")"
    case default => this.show
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr