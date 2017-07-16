package shapelessguideexamples.chapter5

import shapeless.{HNil, HList, ::}
import shapeless.syntax.singleton._
import shapeless.labelled.{FieldType, KeyTag}
import shapeless.syntax.SingletonOps

object Types extends App {

  val number: Int = 42
  trait Cherries

  /*
  * Type Tagging
  * Change compile time type
  * but keep runtime behavior the same
  * */
  val numCherries: Int with Cherries =
    number.asInstanceOf[Int with Cherries]


  /*
  * Tags expression on RIGHT
  * with singleton type of literal on the left
  *
  * Here we are tagging number with phantom type:
  * KeyTag["numCherries", Int]
  * */
  val numCherries2 = "numCherries" ->> number
  /*
    @ val number = 42
      number: Int = 42
    @ val numCherries = "numCherries" ->> number
      numCherries: Int with KeyTag[numCherries, Int] = 42
   */

  import shapeless.Witness
  val numCherries3 = "numCherries" ->> 123

  // Get the tag from a tagged value:
  def getFieldName[K, V](value: FieldType[K, V])
                        (implicit witness: Witness.Aux[K]): K =
    witness.value

  println(getFieldName(numCherries3))

  def getFieldValue[K, V](value: FieldType[K, V]): V =
    value

  println(getFieldValue(numCherries3))



  // Record - HList with tagged fields
  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil


}
