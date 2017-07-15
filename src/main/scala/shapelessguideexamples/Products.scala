package shapelessguideexamples

import shapeless._

object Products extends App {

  /*
  * Products
  * */
  case class Employee(name: String, number: Int, manager: Boolean)

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val e1 = Employee("Dave", 123, false)
  val ic1 = IceCream("Sundae", 1, false)

  val genEmployee = Generic[Employee]
  val genIceCream = Generic[IceCream]

  println(genEmployee.to(e1))
  println(genIceCream.to(ic1))

  def genericCsv(gen: String :: Int :: Boolean :: HNil): String =
    List(gen(0), gen(1).toString, gen(2).toString).mkString(",")


  println(genericCsv(genEmployee.to(e1)))
  println(genericCsv(genIceCream.to(ic1)))

  println(genEmployee.from(genIceCream.to(ic1)))
  println(genIceCream.from(genEmployee.to(e1)))

}
