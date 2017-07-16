package shapelessguideexamples.chapter5

import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, the}
import shapeless.labelled.FieldType
import shapelessguideexamples.IceCream
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy, Witness}
import shapelessguideexamples.chapter2.{Circle, Rectangle, Shape}

sealed trait JsonValue

case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue

case class JsonArray(items: List[JsonValue]) extends JsonValue

case class JsonString(value: String) extends JsonValue

case class JsonNumber(value: Double) extends JsonValue

case class JsonBoolean(value: Boolean) extends JsonValue

case object JsonNull extends JsonValue

/**
 * Type class for encoding a value of type A as JSON.
 */
trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

object JsonEncoder {

  def apply[T](implicit enc: JsonEncoder[T]): JsonEncoder[T] = enc

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] =
    value => func(value)

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] { //DON'T CONVERT THIS USING SAM TYPES
      def encode(value: A): JsonObject =
        fn(value)
    }

  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => JsonNumber(num))
  implicit val intEncoder: JsonEncoder[Int] =
    createEncoder(num => JsonNumber(num))
  implicit val booleanEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => JsonBoolean(bool))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(enc.encode)))
  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    createObjectEncoder {
      case Inl(h) =>
        JsonObject(List(typeName -> hEncoder.value.encode(h)))
      case Inr(t) =>
        tEncoder.encode(t)
    }
  }

  implicit def genericObjectEncoder[A, H](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }

}

object DeriveProductWithLabelGen extends App {

  def encodeJson[A](value: A)(implicit encoder: JsonEncoder[A]): JsonValue =
    encoder.encode(value)

  val iceCream = IceCream("Sundae", 1, false)
  val gen = LabelledGeneric[IceCream].to(iceCream)

  println(gen)

  the[JsonEncoder[IceCream]]

  println(JsonEncoder[IceCream].encode(iceCream))
  println(encodeJson(iceCream))

  println(LabelledGeneric[Shape].to(Circle(1.0)))

  val shape: Shape = Circle(2.0)

  println(JsonEncoder[Shape].encode(Rectangle(1.0, 2.0)))
  println(encodeJson(shape))

}
