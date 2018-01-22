package fp.error

import scala.{Option => _, Either => _}
case class Some[+A](get:A) extends Option[A]
{

}
case object None extends Option[Nothing]



sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =  {

    this match {
      case None => None
      case Some(a) => Some[B](f(a))
    }
  }


  def flatMap[B]( f:A=> Option[B]) = {

    this match {

      case None => None
      case Some(a) => f(a)

    }

  }

  def getOrElse [B >: A] ( default: => B ) : B  ={
    this match {
      case None => { default}
      case Some(a) => a
    }
  }



  def orElse [B >: A] ( op: => B ) : Option[B]  ={
    this match {
      case None => None
      case Some(a) => Some[B](op)
    }
  }


  def filter ( f:A=> Boolean) :Option[A] = {


    this match {
      case None => None
      case Some(a) => if ( f(a) == true) Some(a) else None
    }
  }

  def filter2 ( f:A=> Boolean) :Option[A] = {

    if ( this.map(f) == true) this else None
  }


  def variance ( xs:Seq[Double]):Option[Double] =
  {
    val mean = Some ( xs.foldLeft(0.0)( _ + _)/xs.length)

    val `var` = xs.map( x => math.pow( x - mean, 2))

    val v2 = mean.flatMap ( xs => )


  }

}



