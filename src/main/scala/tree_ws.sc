
sealed  trait Tree[+A]
case class Leaf[A](value:A) extends  Tree[A]

case class Branch[A]( left:Tree[A], right: Tree[A]) extends  Tree[A]


def count_nodes [A] ( root:Tree[A]):Int ={
  root match {
    case Leaf (v) =>  1;
    case Branch(l,r) => 1 + count_nodes(l)   + count_nodes (r)
  }
}


val l1 = Leaf(1)
val l2 = Leaf(33)

val l3 = Leaf(3)
val l4 = Leaf(4)


val t1 = Branch(l1, l2)

val t2 = Branch(l3,l4)

val root1 = Branch[Int]( t1,t2 )

count_nodes( t1)

count_nodes( root1)


def max_element ( root:Tree[Int]):Int ={

  root match {
    case Leaf(v) => v
    case Branch(l, r) => {
      val mx_left = max_element(l)
      val mx_right = max_element(r)
      if (mx_left > mx_right) mx_left else mx_right
    }
  }

}

val res10 = max_element( root1)

def max_depth[A] (root:Tree[A]):Int = {

  root match {
    case Leaf(v) => 1
    case Branch(l, r) => {
      val mx_left =  1 + max_depth(l)
      val mx_right = 1+ max_depth(r)
      if (mx_left >= mx_right) mx_left else mx_right
    }
  }

}

val res11 = max_depth(t1)


def treeMap[A,B]( root:Tree[A]) (f:A=>B):Tree[B] = {
  root match {
    case Leaf(v) =>  Leaf(f(v))
    case Branch(l,r) => Branch[B](treeMap(l)(f), treeMap(r)(f))

  }}

val double:Int=>Int = x => x *2
val tdoub = treeMap(root1)(double)