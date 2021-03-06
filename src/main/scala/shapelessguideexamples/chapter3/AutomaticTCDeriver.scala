package shapelessguideexamples.chapter3

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy, the}
import shapelessguideexamples.chapter2.{Circle, Rectangle, Shape}
import shapelessguideexamples.{Employee, IceCream, Tree}

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {

  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  // "Constructor" method
  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    value => func(value)

  implicit val stringEncoder: CsvEncoder[String] =
    createEncoder(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    createEncoder(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    createEncoder(b => if (b) List("yes") else List("no"))

  implicit val doubleEncoder: CsvEncoder[Double] =
    createEncoder(d => List(d.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    createEncoder(_ => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[CsvEncoder[H]],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] = {
    createEncoder {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }
  }

  /*
  * If you have an encoder for it's parts(gen.Repr = HList),
  * then you have an encoder for the whole thing
  *
  * "Given a type A and an HList type R,
  * an implicit Generic to map A to R,
  * and a CsvEncoder for R, create a CsvEncoder for A."
  * */
  implicit def genericEncoder[A, R](
    implicit
//    gen: Generic[A]{ type Repr = R },
    gen: Generic.Aux[A, R], //same as above commented code
    enc: Lazy[CsvEncoder[R]] // generic repr of A
  ): CsvEncoder[A] = {
    createEncoder { a =>
      enc.value.encode(gen.to(a))
    }
  }

  // ideally this is dead code. If you get here you have a problem
  implicit val cnilEncoder: CsvEncoder[CNil] =
    createEncoder(_ => throw new Exception("Impossibru!"))

  implicit def coproudctEncoder[H, T <: Coproduct](
                                                  implicit
                                                  hEncoder: Lazy[CsvEncoder[H]],
                                                  tEncoder: CsvEncoder[T]
                                                  ): CsvEncoder[H :+: T] = {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

}

object AutomaticTCDeriver extends App {

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  /*println(writeCsv(iceCreams))

  CsvEncoder[IceCream] == implicitly[CsvEncoder[IceCream]]
  the[CsvEncoder[IceCream]]*/

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

//  println(reprEncoder.encode("abc" :: 123 :: true :: HNil))

  // 1 impl for IceCream
  // but this is too specific
  /*implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    CsvEncoder.createEncoder(iceCream => enc.encode(gen.to(iceCream)))
  }*/

  println(writeCsv(iceCreams))

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(1.0)
  )

  println(writeCsv(shapes))

  the[CsvEncoder[Tree[Int]]]

  import scala.reflect.runtime.universe._

  println(reify(CsvEncoder[Boolean]))

}
