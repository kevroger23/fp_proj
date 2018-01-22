

def areEqual[A](la:List[A], lb:List[A] ) :Boolean =


  (la.length == lb.length) && (
    if (la == Nil) true else (la.head == lb.head) && areEqual(la.tail, lb.tail)
    )


val l1  = List (1,3,4,5,1,2)

val l2 = List (1,2,3)

l1.take(2)


println ( "Are equal = " +  areEqual(l1, l2))



def getSliceList[A] ( xs:List[A], slen:Int):List[List[A]] = {
  if ( slen > xs.length) Nil else
     xs.take(slen)::getSliceList(xs.tail,slen)
}

val sl = getSliceList(l1, 8)


def isSubsequence [A] (la:List[A], lb:List[A]):Boolean = {

  getSliceList(la, lb.length).contains(lb)

}


isSubsequence(l1, l2)