package shapelessguideexamples.chapter2

import shapeless.Generic
import shapelessguideexamples.chapter2.ADTAndGenRepr.{Circle, Rectangle, Shape}

object CoProducts extends App {

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
  case class Red()
  case class Amber()
  case class Green()
  type Light = Red :+: Amber :+: Green :+: CNil

  val red: Light = Inl(Red())

  val green: Light = Inr(Inr(Inl(Green())))

  println(red)
  println(green)


  // modeled as CoProduct
  val gen = Generic[Shape]
  // gen: shapeless.Generic[Shape]{type Repr =
  // shapeless.:+:[Rectangle,shapeless.:+:[Circle,shapeless.CNil]]}

println(gen)
  println(gen.to(Rectangle(3.0, 4.0)))
  println(gen.to(Circle(1.0)))

}
