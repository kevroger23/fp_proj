package altlist

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[A] (head:A, tail: List[A]) extends List[A]

object List {
  def apply[A](xs:A*): List[A] = {
    if (xs.isEmpty) Nil else
      Cons[A] ( xs.head, apply(xs.tail:_*))

  }

  def tail[A] ( xs:List[A]):List[A] =
  {
    xs match {
      case Nil => Nil;
      case Cons (h,t) => t
    }
  }

  def drop[A]( n:Int, xs:List[A]):List[A] = {

    if ( n <= 0 ) xs  else drop ( n-1, tail ( xs))

  }

  def dropWhileA[A](xs:List[A], f:A=> Boolean  ) : List[A] = {
    xs match {
      case Nil => Nil
      case Cons( h,t) => if ( f(h)) dropWhileA(t, f) else Cons ( h, dropWhileA(t, f))
    }
  }

  def setHead[A](x:A, xs:List[A] ):List[A] =
  {
    xs match {
      case Nil => Cons ( x, Nil)
      case Cons( h,t) => Cons ( x,t)
    }
  }

  def setHead2[A](x:A, xs:List[A] ):List[A] = Cons( x, List.tail(xs))


  def init[A](xs:List[A]):List[A] = {

    xs match {
      case Nil => Nil
      case Cons(lastelem, Nil )=> Nil
      case Cons ( h,t) => Cons( h, init (t))
    }
  }


  def foldRight[A,B] ( xs:List[A], init:B) ( f:(A,B)=> B) : B = {
    xs match {
      case Nil => init
      case Cons(h,t ) => f ( h, foldRight(t, init) (f))
    }
  }


  //def foldLeftR[A,B] ( xs:List[A], init:B) ( f:(A,B)=> B) : B = {
  //}

  def foldLeft[A,B] ( xs:List[A], init:B) ( f:(A,B)=> B) : B = {

    xs match {

      case Nil => init
      case Cons( h,t) =>  foldLeft(t, f(h,init))(f)
    }
  }

  def foldRightR[A,B] ( xs:List[A], init:B) ( f:(A,B)=> B) : B = {

    def addfold( acc:B, xs:List[A] ): B = {

      xs match {

        case  Nil =>  acc
        case Cons( h, t) => addfold(f(h,acc), t)

      }

    }
   addfold(init, xs)
  }

  def reverse[A](xs:List[A]):List[A] = foldLeft(xs,Nil:List[A])( (h,b )=> Cons( h,b) )

  def append[A] (xs:List[A], a:A):List[A] = foldRight(xs, Cons(a,Nil)) ( (h,b )=> Cons( h,b) )


  def map[A,B]( xs:List[A]) (f:A=>B ) : List[B] = foldRight(xs, Nil:List[B]) ( (a:A, b:List[B]) => (Cons (f(a), b)) )





  def len[A](xs:List[A]):Int  = foldRightR[A,Int](xs , 0 )( ( _ , b ) => b +1 )

  def len2[A] (xs:List[A]):Int  = foldLeft(xs,0)(( _ , b ) => b +1 )
  def prodInt (xs:List[Int]) = foldRightR[Int,Int](xs, 1)( _*_)


  def sum2(xs:List[Int]) = foldLeft(xs, 0) ( (x:Int, y:Int) => x + y)

  def filter[A] (xs:List[A])( f: A => Boolean):List[A] =  {
    xs match {
      case Nil => Nil
      case Cons( h, t) => if (f(h)) Cons(h, filter (t)(f) ) else filter(t)(f)
    }
  }

  type blist = List[B]
  def flatmap_i[A,blist] (xs:List[A])( f:A=>blist) : List[blist] = {
    map(xs)(f)

  }

  def flatmap(xs:List[blist]):List[B] = {


  }
}