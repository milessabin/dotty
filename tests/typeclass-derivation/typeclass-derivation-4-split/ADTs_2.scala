import Mirror.productElement

import Utils._

// ADTs

case class ISB(i: Int, s: String, b: Boolean)
object ISB extends Mirror.Product {
  type MirroredType = ISB
  type MonoType = MirroredType
  type Label = "ISB"
  type ElemTypes = (Int, String, Boolean)
  type ElemLabels = ("i", "s", "b")

  def fromProduct(p: Product): MonoType =
    ISB(productElement[Int](p, 0), productElement[String](p, 1), productElement[Boolean](p, 2))

  inline implicit def mirror: this.type = this
}

case class Box[A](x: A)
object Box extends Mirror.Product {
  type MirroredType = Box
  type MonoType = Box[_]
  type Label = "Box"
  type ElemTypes = [t] => Tuple1[t]
  type ElemLabels = Tuple1["x"]

  def fromProduct(p: Product): MonoType =
    Box[Any](productElement[Any](p, 0))

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T]: Mirror.Product {
    type MirroredType = Box.MirroredType[T]
    type MonoType = MirroredType
    type Label = Box.Label
    type ElemTypes = Box.ElemTypes[T]
    type ElemLabels = Box.ElemLabels
  } = Box.asInstanceOf
}

sealed trait OptionInt
object OptionInt extends Mirror.Sum {
  type MirroredType = OptionInt
  type MonoType = MirroredType
  type Label = "OptionInt"
  type ElemTypes = (SomeInt, NoneInt.type)
  type ElemLabels = ("SomeInt", "NoneInt")

  def ordinal(x: MonoType): Int = x match {
    case _: SomeInt => 0
    case NoneInt => 1
  }

  inline implicit def mirror: this.type = this
}

case class SomeInt(value: Int) extends OptionInt
object SomeInt extends Mirror.Product {
  type MirroredType = SomeInt
  type MonoType = MirroredType
  type Label = "SomeInt"
  type ElemTypes = Tuple1[Int]
  type ElemLabels = Tuple1["value"]

  def fromProduct(p: Product): MonoType =
    SomeInt(productElement[Int](p, 0))

  inline implicit def mirror: this.type = this
}

case object NoneInt extends OptionInt with Mirror.Product {
  type MirroredType = NoneInt.type
  type MonoType = MirroredType
  type Label = "NoneInt"
  type ElemTypes = Unit
  type ElemLabels = Unit

  def fromProduct(p: Product): MonoType =
    NoneInt

  inline implicit def mirror: this.type = this
}

sealed trait Opt[+A]
object Opt extends Mirror.Sum {
  type MirroredType = Opt
  type MonoType = Opt[_]
  type Label = "Opt"
  type ElemTypes = [t] => (Sm[t], Const[Nn.type][t])
  type ElemLabels = ("Sm", "Nn")

  def ordinal(x: MonoType): Int = x match {
    case _: Sm[_] => 0
    case Nn => 1
  }

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T]: Mirror.Sum {
    type MirroredType = Opt.MirroredType[T]
    type MonoType = MirroredType
    type Label = Opt.Label
    type ElemTypes = Opt.ElemTypes[T]
    type ElemLabels = Opt.ElemLabels
  } = Opt.asInstanceOf
}

case class Sm[+A](value: A) extends Opt[A]
object Sm extends Mirror.Product {
  type MirroredType = Sm
  type MonoType = Sm[_]
  type Label = "Sm"
  type ElemTypes = [t] => Tuple1[t]
  type ElemLabels = Tuple1["value"]

  def fromProduct(p: Product): MonoType =
    Sm(productElement[Any](p, 0))

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T]: Mirror.Product {
    type MirroredType = Sm.MirroredType[T]
    type MonoType = MirroredType
    type ElemTypes = Sm.ElemTypes[T]
    type Label = Sm.Label
    type ElemLabels = Sm.ElemLabels
  } = Sm.asInstanceOf
}

case object Nn extends Opt[Nothing] with Mirror.Product {
  type MirroredType = Nn.type
  type MonoType = MirroredType
  type Label = "Nn"
  type ElemTypes = Unit
  type ElemLabels = Unit

  def fromProduct(p: Product): MonoType =
    Nn

  inline implicit def mirror: this.type = this
}

sealed trait CList[+A]
object CList extends Mirror.Sum {
  type MirroredType = CList
  type MonoType = CList[_]
  type Label = "CList"
  type ElemTypes = [t] => (CCons[t], Const[CNil.type][t])
  type ElemLabels = ("CCons", "CNil")

  def ordinal(x: MonoType): Int = x match {
    case _: CCons[_] => 0
    case CNil => 1
  }

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T]: Mirror.Sum {
    type MirroredType = CList.MirroredType[T]
    type MonoType = MirroredType
    type Label = CList.Label
    type ElemTypes = CList.ElemTypes[T]
    type ElemLabels = CList.ElemLabels
  } = CList.asInstanceOf
}

case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
object CCons extends Mirror.Product {
  type MirroredType = CCons
  type MonoType = CCons[_]
  type Label = "CCons"
  type ElemTypes = [t] => (t, CList[t])
  type ElemLabels = ("hd", "tl")

  def fromProduct(p: Product): MonoType =
    CCons(productElement[Any](p, 0), productElement[CList[Any]](p, 1))

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T]: Mirror.Product {
    type MirroredType = CCons.MirroredType[T]
    type MonoType = MirroredType
    type Label = CCons.Label
    type ElemTypes = CCons.ElemTypes[T]
    type ElemLabels = CCons.ElemLabels
  } = CCons.asInstanceOf
}

case object CNil extends CList[Nothing] with Mirror.Product {
  type MirroredType = CNil.type
  type MonoType = MirroredType
  type Label = "CNil"
  type ElemTypes = Unit
  type ElemLabels = Unit

  inline implicit def mirror: this.type = this
  def fromProduct(p: Product): MonoType =
    CNil
}

case class Order[F[_]](
  item: F[String],
  quantity: F[Int]
)
object Order extends Mirror.Product {
  type MirroredType = Order
  type MonoType = Order[[_] => Any]
  type Label = "Order"
  type ElemTypes = [t[_]] => (t[String], t[Int])
  type ElemLabels = ("item", "quantity")

  def fromProduct(p: Product): MonoType =
    Order[[_] => Any](productElement[Any](p, 0), productElement[Any](p, 1))

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[F[_]]: Mirror.Product {
    type MirroredType = Order.MirroredType[F]
    type MonoType = MirroredType
    type Label = Order.Label
    type ElemTypes = Order.ElemTypes[F]
    type ElemLabels = Order.ElemLabels
  } = Order.asInstanceOf
}

sealed trait OptionD[T] {
  def fold: T = this match {
    case Given(t) => t
    case Default(t) => t
  }
}
object OptionD {
  val fold: OptionD ~> Id = [t] -> (ot: OptionD[t]) => ot.fold
}

case class Given[T](value: T) extends OptionD[T]
case class Default[T](value: T) extends OptionD[T]

trait ListF[+A, +R]
object ListF extends Mirror.Sum {
  type List[A] = Fix[ListF, A]

  type MirroredType = ListF
  type MonoType = ListF[_, _]
  type Label = "ListF"
  type Const[c] = [t, u] => c
  type ElemTypes = [t, u] => (ConsF[t, u], Const[NilF.type][t, u])
  type ElemLabels = ("ConsF", "NilF")

  def ordinal(x: MonoType): Int = x match {
    case _: ConsF[_, _] => 0
    case NilF => 1
  }

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T, U]: Mirror.Sum {
    type MirroredType = ListF.MirroredType[T, U]
    type MonoType = MirroredType
    type Label = ListF.Label
    type ElemTypes = ListF.ElemTypes[T, U]
    type ElemLabels = ListF.ElemLabels
  } = ListF.asInstanceOf
}

case class ConsF[+A, +R](hd: A, tl: R) extends ListF[A, R]
object ConsF extends Mirror.Product {
  type MirroredType = ConsF
  type MonoType = ConsF[_, _]
  type Label = "ConsF"
  type ElemTypes = [t, u] => (t, u)
  type ElemLabels = ("hd", "tl")

  def fromProduct(p: Product): MonoType =
    ConsF(productElement[Any](p, 0), productElement[Any](p, 1))

  inline implicit def mirror: this.type = this
  inline implicit def monoMirror[T, U]: Mirror.Product {
    type MirroredType = ConsF.MirroredType[T, U]
    type MonoType = MirroredType
    type Label = ConsF.Label
    type ElemTypes = ConsF.ElemTypes[T, U]
    type ElemLabels = ConsF.ElemLabels
  } = ConsF.asInstanceOf
}

case object NilF extends ListF[Nothing, Nothing] with Mirror.Product {
  type MirroredType = NilF.type
  type MonoType = MirroredType
  type Label = "NilF"
  type ElemTypes = Unit
  type ElemLabels = Unit

  def fromProduct(p: Product): MonoType =
    NilF

  inline implicit def mirror: this.type = this
}

case class BI(b: Boolean, i: Int)
object BI extends Mirror.Product {
  type MirroredType = BI
  type MonoType = MirroredType
  type Label = "BI"
  type ElemTypes = (Boolean, Int)
  type ElemLabels = ("b", "i")

  def fromProduct(p: Product): MonoType =
    BI(productElement[Boolean](p, 0), productElement[Int](p, 1))

  inline implicit def mirror: this.type = this
}
