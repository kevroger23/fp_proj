package fp.error

import  fp.error._
object err_test extends App {


  val v1 = Some(10)

  println ( "Test =" + v1);

  println ( "Double test = " + v1.map(_*2.2))

  println ( "Is odd v1 " +  v1.filter2(_%2 != 0))

}
