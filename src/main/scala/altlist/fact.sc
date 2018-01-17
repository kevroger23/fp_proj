

def fac1 ( n:Int):Int = n match {
  case 0 => 1
  case 1 => 1
  case _ => n * fac1( n-1)

}

println ( fac1(4))


def fac ( n:Int):Int = {

 // val acc = 1
  def mul ( acc:Int, n:Int ):Int =
  {
    if ( n <= 1 ) acc
    else mul ( acc * n, n -1)
  }


  def mul2 ( acc:Int, n:Int):Int = {

    n match {

      case 1 => acc
      case _  =>  mul2( acc * n, n - 1)
    }
  }

  mul2 ( 1, n)
}


println ( fac(5))