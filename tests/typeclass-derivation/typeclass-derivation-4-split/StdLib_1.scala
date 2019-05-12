import scala.collection.mutable.WrappedArray
import scala.compiletime._

// Standard library

object Utils {
  type Id[t] = t
  type Const[c] = [t] => c

  type ~>[A[_], B[_]] = [t] -> A[t] => B[t]

  inline def summon[T] = implicit match {
    case t: T => t
  }

  inline def summonAsArray[T]: Array[Any] = inline erasedValue[Id[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[a])
    case _: (a, b) => Array(summon[a], summon[b])
    case _: (a, b, c) => Array(summon[a], summon[b], summon[c])
    case _: (a, b, c, d) => Array(summon[a], summon[b], summon[c], summon[d])
    case _: (a, b, c, d, e) => Array(summon[a], summon[b], summon[c], summon[d], summon[e])
    // Add fallback for larger sizes
  }

  inline def summonValues[T] <: T = inline erasedValue[Id[T]] match {
    case _: Unit => ()
    case _: (a *: b) => constValue[a] *: summonValues[b]
  }

  inline def summonValuesAsArray[T]: Array[Any] = inline erasedValue[Id[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(constValue[a])
    case _: (a, b) => Array(constValue[a], constValue[b])
    case _: (a, b, c) => Array(constValue[a], constValue[b], constValue[c])
    case _: (a, b, c, d) => Array(constValue[a], constValue[b], constValue[c], constValue[d])
    case _: (a, b, c, d, e) => Array(constValue[a], constValue[b], constValue[c], constValue[d], constValue[e])
    // Add fallback for larger sizes
  }

  case class Fix[S[_, _], A](unfix: S[A, Fix[S, A]])
}

case class Labelling[T](label: String, elemLabels: Seq[String])
object Labelling {
  inline implicit def apply[T0](implicit mirror: Mirror { type MirroredType = T0 }): Labelling[T0] =
    Labelling[T0](constValue[mirror.Label & String], WrappedArray.make[String](Utils.summonValuesAsArray[mirror.ElemLabels]))
}

sealed trait Mirror {
  // type MirroredType <: AnyKind // possible, but not necessary
  type MonoType
  type Label
  type ElemLabels
}

object Mirror {
  trait Product extends Mirror {
    def fromProduct(p: scala.Product): MonoType
  }

  trait Sum extends Mirror {
    def ordinal(x: MonoType): Int
  }

  def productToTuple(x: Any): Any =
    x match {
      case p: scala.Product => p.productArity match {
        case 0 => ()
        case 1 => Tuple1(p.productElement(0))
        case 2 => (p.productElement(0), p.productElement(1))
        case 3 => (p.productElement(0), p.productElement(1), p.productElement(2))
        case 4 => (p.productElement(0), p.productElement(1), p.productElement(2), p.productElement(3))
        case 5 => (p.productElement(0), p.productElement(1), p.productElement(2), p.productElement(3), p.productElement(4))
        // Add fallback for larger sizes
      }
    }

  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[scala.Product].productElement(idx).asInstanceOf[T]
}

