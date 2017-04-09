package week4

object booleans {
  False && False                                  //> res0: week4.Boolean = false
  False && True                                   //> res1: week4.Boolean = false
  True && False                                   //> res2: week4.Boolean = false
  True && True                                    //> res3: week4.Boolean = true
  
  False || False                                  //> res4: week4.Boolean = false
  False || True                                   //> res5: week4.Boolean = true
  True || False                                   //> res6: week4.Boolean = true
  True || True                                    //> res7: week4.Boolean = true
  
  !False                                          //> res8: week4.Boolean = true
  !True                                           //> res9: week4.Boolean = false
  
  False == False                                  //> res10: week4.Boolean = true
  False == True                                   //> res11: week4.Boolean = false
  True == False                                   //> res12: week4.Boolean = false
  True == True                                    //> res13: week4.Boolean = true
  
  False != False                                  //> res14: week4.Boolean = false
  False != True                                   //> res15: week4.Boolean = true
  True != False                                   //> res16: week4.Boolean = true
  True != True                                    //> res17: week4.Boolean = false
  
  False < False                                   //> res18: week4.Boolean = false
  False < True                                    //> res19: week4.Boolean = true
  True < False                                    //> res20: week4.Boolean = false
  True < True                                     //> res21: week4.Boolean = false
  
}

abstract class Boolean {
  
  def ifThenElse[T](t: => T, e: => T): T
  
  def && (x: => Boolean): Boolean = ifThenElse(x, False)
  
  def || (x: => Boolean): Boolean = ifThenElse(True, x)
  
  def unary_! : Boolean = ifThenElse(False, True)
  
  def == (x: => Boolean): Boolean = ifThenElse(x, x.unary_!)
  
  def != (x: => Boolean): Boolean = ifThenElse(x.unary_!, x)
  
  def < (x: => Boolean): Boolean = ifThenElse(False, x)
  
}

object True extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = t
  override def toString = "true"
}

object False extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = e
  override def toString = "false"
}