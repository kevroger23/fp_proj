import altlist._

object omain extends App {


  println ("hello")

  val l1 = List(22, 1,3,10,5,11)

  println (l1)
  val l2 = List.tail(l1)

  val l3 = List.setHead(23, l1)

  println(l3)

  val l4 = List.setHead2(100, l1)
  println ( l4)


  val t2 = List.drop (2, l1)

  println( t2)

  def isEven(x:Int) = x%2 == 0
  val noEven = List.dropWhileA[Int](l1, isEven )

  println(noEven)


  val exceptlast = List.init(l1)

  println ( exceptlast)


  val prod = List.prodInt(l1)

  println( prod)


  val cons_list = List.foldRight(l1, Nil:List[Int])(Cons(_,_))

  val cons_list2 = List.foldRight(l1, Cons(1000,Nil))(Cons(_,_))


  println ("Conslist2 = " + cons_list2)

  val list_len = List.len(l1)

  println ( "List Length =" + list_len)

  val list_len2 = List.len2(l1)

  println ( "List Length =" + list_len2)

  println ( "Sum2 = " + List.sum2(l1))


  println ( "Reverse List = " + List.reverse(l1))


  println ( "appended List = "  + List.append(l1, 458))

  def addOne(xs:List[Int]) = List.map(xs)( x => x + 1)

  println ( "Added One List = " + addOne(l1))


  println ( "Filtered list of odds =" + List.filter(l1)(_%2 != 0))

}

