object naturals {
  val zero = Zero                                 //> zero  : Zero.type = 0
  val one = zero.successor                        //> one  : Nat = 1
  val two = one.successor                         //> two  : Nat = 2
  val three = two.successor                       //> three  : Nat = 3

  zero + zero                                     //> res0: Nat = 0
  zero + one                                      //> res1: Nat = 1
  zero.successor                                  //> res2: Nat = 1

  one.predecessor                                 //> res3: Nat = 0
  two.predecessor                                 //> res4: Nat = 1
  three.predecessor                               //> res5: Nat = 2
  one + zero                                      //> res6: Nat = 1
  one + one                                       //> res7: Nat = 2
  one + two                                       //> res8: Nat = 3
  one + three                                     //> res9: Nat = 4

  one - one                                       //> res10: Nat = 0
  three - one                                     //> res11: Nat = 2
  one - two                                       //> java.lang.Error: negative result
                                                  //| 	at Zero$.$minus(naturals.scala:46)
                                                  //| 	at Succ.$minus(naturals.scala:53)
                                                  //| 	at naturals$$anonfun$main$1.apply$mcV$sp(naturals.scala:21)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at naturals$.main(naturals.scala:1)
                                                  //| 	at naturals.main(naturals.scala)
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
  override def toString = {
    def loop(predecesor: Nat, counter: Int): String =
      if (predecesor.isZero)
        "" + counter
      else
        loop(predecesor.predecessor, counter + 1)
    loop(this, 0)
  }
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("no predecessor for zero")
  def +(that: Nat) = that
  def -(that: Nat) =
    if (that.isZero) this
    else throw new Error("negative result")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat) = new Succ(n + that)
  def -(that: Nat) = if (that.isZero) this else n - that.predecessor
}