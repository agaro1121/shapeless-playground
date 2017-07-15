package stuff

object KittensMain extends App {

  import cats.derived._, functor._, legacy._
  import cats.Functor

  case class Cat[T](food: Option[T], foods: List[T])

  val f = Functor[Cat]

  val cat = Cat(Some(1), List(2, 3))
  println(f.map(cat)(_ + 1))

}
