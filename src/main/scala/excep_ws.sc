
import scala.{ Option => _, Either => _}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]



sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =  {

    this match {
      case None => None
      case Some(a) => Some[B](f(a))
    }
  }



}


val v1 = Some[Int](10)
