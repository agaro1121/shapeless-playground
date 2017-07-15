package shapelessguideexamples

sealed trait Tree[A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]
