package scala
import annotation.showAsInfix
import typelevel._

sealed trait Tuple extends Any {
  import Tuple._

  inline def toArray: Array[Object] =
    /*if (specialize)
      inline constValueOpt[BoundedSize[this.type]] match {
      case Some(0) =>
        $emptyArray
      case Some(1) =>
        val t = asInstanceOf[Tuple1[Object]]
        Array(t._1)
      case Some(2) =>
        val t = asInstanceOf[Tuple2[Object, Object]]
        Array(t._1, t._2)
      case Some(3) =>
        val t = asInstanceOf[Tuple3[Object, Object, Object]]
        Array(t._1, t._2, t._3)
      case Some(4) =>
        val t = asInstanceOf[Tuple4[Object, Object, Object, Object]]
        Array(t._1, t._2, t._3, t._4)
      case Some(n) if n <= $MaxSpecialized =>
        $toArray(this, n)
      case Some(n) =>
        asInstanceOf[TupleXXL].elems
      case None =>
        dynamicToArray(this)
    }
    else*/ dynamicToArray(this)

  inline def *: [H] (x: H): H *: this.type =
    /*if (specialize) {
      type Result = H *: this.type
      inline constValueOpt[BoundedSize[this.type]] match {
        case Some(0) =>
          Tuple1(x).asInstanceOf[Result]
        case Some(1) =>
          Tuple2(x, asInstanceOf[Tuple1[_]]._1).asInstanceOf[Result]
        case Some(2) =>
          val t = asInstanceOf[Tuple2[_, _]]
          Tuple3(x, t._1, t._2).asInstanceOf[Result]
        case Some(3) =>
          val t = asInstanceOf[Tuple3[_, _, _]]
          Tuple4(x, t._1, t._2, t._3).asInstanceOf[Result]
        case Some(4) =>
          val t = asInstanceOf[Tuple4[_, _, _, _]]
          Tuple5(x, t._1, t._2, t._3, t._4).asInstanceOf[Result]
        case Some(n) =>
          fromArray[Result]($consArray(x, toArray))
        case _ =>
          dynamic_*:[this.type, H](this, x)
      }
    }
    else*/ dynamic_*:[this.type, H](this, x)

  inline def ++(that: Tuple): Concat[this.type, that.type] =
    /*if (specialize) {
      type Result = Concat[this.type, that.type]
      inline constValueOpt[BoundedSize[this.type]] match {
        case Some(0) =>
          that.asInstanceOf[Result]
        case Some(1) =>
          if (constValue[BoundedSize[that.type]] == 0) this.asInstanceOf[Result]
          else (asInstanceOf[Tuple1[_]]._1 *: that).asInstanceOf[Result]
        case Some(2) =>
          val t = asInstanceOf[Tuple2[_, _]]
          inline constValue[BoundedSize[that.type]] match {
            case 0 => this.asInstanceOf[Result]
            case 1 =>
              val u = that.asInstanceOf[Tuple1[_]]
              Tuple3(t._1, t._2, u._1).asInstanceOf[Result]
            case 2 =>
              val u = that.asInstanceOf[Tuple2[_, _]]
              Tuple4(t._1, t._2, u._1, u._2).asInstanceOf[Result]
            case _ =>
              genericConcat[Result](this, that).asInstanceOf[Result]
          }
        case Some(3) =>
          val t = asInstanceOf[Tuple3[_, _, _]]
          inline constValue[BoundedSize[that.type]] match {
            case 0 => this.asInstanceOf[Result]
            case 1 =>
              val u = that.asInstanceOf[Tuple1[_]]
              Tuple4(t._1, t._2, t._3, u._1).asInstanceOf[Result]
            case _ =>
              genericConcat[Result](this, that).asInstanceOf[Result]
          }
        case Some(_) =>
          if (constValue[BoundedSize[that.type]] == 0) this.asInstanceOf[Result]
          else genericConcat[Result](this, that).asInstanceOf[Result]
        case None =>
          dynamic_++[this.type, that.type](this, that)
      }
    }
  else*/ dynamic_++[this.type, that.type](this, that)

  inline def genericConcat[T <: Tuple](xs: Tuple, ys: Tuple): Tuple =
    fromArray[T](xs.toArray ++ ys.toArray)

  inline def size: Size[this.type] =
    /*if (specialize) {
      type Result = Size[this.type]
      inline constValueOpt[BoundedSize[this.type]] match {
        case Some(n) => n.asInstanceOf[Result]
        case _ => dynamicSize(this)
      }
    }
    else*/ dynamicSize(this)
}

object Tuple {
  inline val $MaxSpecialized = 22
  inline private val XXL = $MaxSpecialized + 1

  final val specialize = false

  type Head[+X <: NonEmptyTuple] = X match {
    case x *: _ => x
  }

  type Tail[+X <: NonEmptyTuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  type Concat[+X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[+X <: Tuple, +N] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  type Size[+X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Size[xs]]
  }

  private[scala] type BoundedSizeRecur[X, L <: Int] <: Int = X match {
    case Unit => 0
    case x *: xs =>
      L match {
        case 0 => 0
        case S[n] => S[BoundedSizeRecur[xs, n]]
      }
  }

  private[scala] type BoundedSize[X] = BoundedSizeRecur[X, 23]

  val $emptyArray = Array[Object]()

  def $toArray(xs: Tuple, n: Int) = {
    val arr = new Array[Object](n)
    var i = 0
    var it = xs.asInstanceOf[Product].productIterator
    while (i < n) {
      arr(i) = it.next().asInstanceOf[Object]
      i += 1
    }
    arr
  }

  def $consArray[H](x: H, elems: Array[Object]): Array[Object] = {
    val elems1 = new Array[Object](elems.length + 1)
    elems1(0) = x.asInstanceOf[Object]
    Array.copy(elems, 0, elems1, 1, elems.length)
    elems1
  }

  inline def fromArray[T <: Tuple](xs: Array[Object]): T =
    /*if (specialize)
      inline constValue[BoundedSize[T]] match {
        case 0  => ().asInstanceOf[T]
        case 1  => Tuple1(xs(0)).asInstanceOf[T]
        case 2  => Tuple2(xs(0), xs(1)).asInstanceOf[T]
        case 3  => Tuple3(xs(0), xs(1), xs(2)).asInstanceOf[T]
        case 4  => Tuple4(xs(0), xs(1), xs(2), xs(3)).asInstanceOf[T]
        case 5  => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4)).asInstanceOf[T]
        case 6  => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5)).asInstanceOf[T]
        case 7  => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6)).asInstanceOf[T]
        case 8  => Tuple8(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7)).asInstanceOf[T]
        case 9  => Tuple9(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8)).asInstanceOf[T]
        case 10 => Tuple10(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9)).asInstanceOf[T]
        case 11 => Tuple11(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10)).asInstanceOf[T]
        case 12 => Tuple12(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11)).asInstanceOf[T]
        case 13 => Tuple13(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12)).asInstanceOf[T]
        case 14 => Tuple14(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13)).asInstanceOf[T]
        case 15 => Tuple15(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14)).asInstanceOf[T]
        case 16 => Tuple16(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15)).asInstanceOf[T]
        case 17 => Tuple17(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16)).asInstanceOf[T]
        case 18 => Tuple18(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17)).asInstanceOf[T]
        case 19 => Tuple19(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18)).asInstanceOf[T]
        case 20 => Tuple20(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19)).asInstanceOf[T]
        case 21 => Tuple21(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20)).asInstanceOf[T]
        case 22 => Tuple22(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20), xs(21)).asInstanceOf[T]
        case _ => TupleXXL(xs).asInstanceOf[T]
      }
    else */dynamicFromArray[T](xs)

  def dynamicFromArray[T <: Tuple](xs: Array[Object]): T = xs.length match {
    case 0  => ().asInstanceOf[T]
    case 1  => Tuple1(xs(0)).asInstanceOf[T]
    case 2  => Tuple2(xs(0), xs(1)).asInstanceOf[T]
    case 3  => Tuple3(xs(0), xs(1), xs(2)).asInstanceOf[T]
    case 4  => Tuple4(xs(0), xs(1), xs(2), xs(3)).asInstanceOf[T]
    case 5  => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4)).asInstanceOf[T]
    case 6  => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5)).asInstanceOf[T]
    case 7  => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6)).asInstanceOf[T]
    case 8  => Tuple8(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7)).asInstanceOf[T]
    case 9  => Tuple9(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8)).asInstanceOf[T]
    case 10 => Tuple10(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9)).asInstanceOf[T]
    case 11 => Tuple11(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10)).asInstanceOf[T]
    case 12 => Tuple12(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11)).asInstanceOf[T]
    case 13 => Tuple13(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12)).asInstanceOf[T]
    case 14 => Tuple14(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13)).asInstanceOf[T]
    case 15 => Tuple15(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14)).asInstanceOf[T]
    case 16 => Tuple16(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15)).asInstanceOf[T]
    case 17 => Tuple17(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16)).asInstanceOf[T]
    case 18 => Tuple18(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17)).asInstanceOf[T]
    case 19 => Tuple19(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18)).asInstanceOf[T]
    case 20 => Tuple20(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19)).asInstanceOf[T]
    case 21 => Tuple21(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20)).asInstanceOf[T]
    case 22 => Tuple22(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20), xs(21)).asInstanceOf[T]
    case _ => TupleXXL(xs).asInstanceOf[T]
  }

  def dynamicToArray(self: Tuple): Array[Object] = (self: Any) match {
    case self: Unit =>
      $emptyArray
    case self: Tuple1[_] =>
      val t = self.asInstanceOf[Tuple1[Object]]
      Array(t._1)
    case self: Tuple2[_, _] =>
      val t = self.asInstanceOf[Tuple2[Object, Object]]
      Array(t._1, t._2)
    case self: Tuple3[_, _, _] =>
      val t = self.asInstanceOf[Tuple3[Object, Object, Object]]
      Array(t._1, t._2, t._3)
    case self: Tuple4[_, _, _, _] =>
      val t = self.asInstanceOf[Tuple4[Object, Object, Object, Object]]
      Array(t._1, t._2, t._3, t._4)
    case self: TupleXXL =>
      asInstanceOf[TupleXXL].elems
    case self: Product =>
      val arr = new Array[Object](self.productArity)
      for (i <- 0 until arr.length) arr(i) = self.productElement(i).asInstanceOf[Object]
      arr
  }

  def dynamic_*: [This <: Tuple, H] (self: Tuple, x: H): H *: This = {
    type Result = H *: This
    (self: Any) match {
      case Unit =>
        Tuple1(x).asInstanceOf[Result]
      case self: Tuple1[_] =>
        Tuple2(x, self._1).asInstanceOf[Result]
      case self: Tuple2[_, _] =>
        Tuple3(x, self._1, self._2).asInstanceOf[Result]
      case self: Tuple3[_, _, _] =>
        Tuple4(x, self._1, self._2, self._3).asInstanceOf[Result]
      case self: Tuple4[_, _, _, _] =>
        Tuple5(x, self._1, self._2, self._3, self._4).asInstanceOf[Result]
      case _ =>
        dynamicFromArray[Result]($consArray(x, dynamicToArray(self)))
    }
  }

  def dynamic_++[This <: Tuple, That <: Tuple](self: This, that: That): Concat[This, That] = {
    type Result = Concat[This, That]
    (this: Any) match {
      case self: Unit => return self.asInstanceOf[Result]
      case _ =>
    }
    (that: Any) match {
      case that: Unit => return self.asInstanceOf[Result]
      case _ =>
    }
    dynamicFromArray[Result](dynamicToArray(self) ++ dynamicToArray(that))
  }

  def dynamicSize[This <: Tuple](self: This): Size[This] = (self: Any) match {
    case self: Unit => 0.asInstanceOf[Size[This]]
    case self: TupleXXL => self.elems.length.asInstanceOf[Size[This]]
    case self: Product => self.productArity.asInstanceOf[Size[This]]
  }
}

abstract sealed class NonEmptyTuple extends Tuple {
  import Tuple._
  import NonEmptyTuple._

  inline def head: Head[this.type] =
    /*if (specialize) {
      type Result = Head[this.type]
      val resVal = inline constValueOpt[BoundedSize[this.type]] match {
        case Some(1) =>
          val t = asInstanceOf[Tuple1[_]]
          t._1
        case Some(2) =>
          val t = asInstanceOf[Tuple2[_, _]]
          t._1
        case Some(3) =>
          val t = asInstanceOf[Tuple3[_, _, _]]
          t._1
        case Some(4) =>
          val t = asInstanceOf[Tuple4[_, _, _, _]]
          t._1
        case Some(n) if n > 4 && n <= $MaxSpecialized =>
          asInstanceOf[Product].productElement(0)
        case Some(n) if n > $MaxSpecialized =>
          val t = asInstanceOf[TupleXXL]
          t.elems(0)
        case None =>
          dynamicHead[this.type](this)
      }
      resVal.asInstanceOf[Result]
    }
    else*/ dynamicHead[this.type](this)

  inline def tail: Tail[this.type] =
    /*if (specialize) {
      type Result = Tail[this.type]
      inline constValueOpt[BoundedSize[this.type]]  match {
        case Some(1) =>
          ().asInstanceOf[Result]
        case Some(2) =>
          val t = asInstanceOf[Tuple2[_, _]]
          Tuple1(t._2).asInstanceOf[Result]
        case Some(3) =>
          val t = asInstanceOf[Tuple3[_, _, _]]
          Tuple2(t._2, t._3).asInstanceOf[Result]
        case Some(4) =>
          val t = asInstanceOf[Tuple4[_, _, _, _]]
          Tuple3(t._2, t._3, t._4).asInstanceOf[Result]
        case Some(5) =>
          val t = asInstanceOf[Tuple5[_, _, _, _, _]]
          Tuple4(t._2, t._3, t._4, t._5).asInstanceOf[Result]
        case Some(n) if n > 5 =>
          fromArray[Result](toArray.tail)
        case None =>
          dynamicTail[this.type](this)
      }
    }
    else*/ dynamicTail[this.type](this)

  /*
  inline def fallbackApply(n: Int) =
    inline constValueOpt[n.type] match {
      case Some(n: Int) => error("index out of bounds: ", n)
      case None => dynamicApply[this.type](this, n)
    }
  */

  inline def apply(n: Int): Elem[this.type, n.type] =
    /*if (specialize) {
      type Result = Elem[this.type, n.type]
      inline constValueOpt[Size[this.type]] match {
        case Some(1) =>
          val t = asInstanceOf[Tuple1[_]]
          inline constValueOpt[n.type] match {
            case Some(0) => t._1.asInstanceOf[Result]
            case _ => fallbackApply(n).asInstanceOf[Result]
          }
        case Some(2) =>
          val t = asInstanceOf[Tuple2[_, _]]
          inline constValueOpt[n.type] match {
            case Some(0) => t._1.asInstanceOf[Result]
            case Some(1) => t._2.asInstanceOf[Result]
            case _ => fallbackApply(n).asInstanceOf[Result]
          }
        case Some(3) =>
          val t = asInstanceOf[Tuple3[_, _, _]]
          inline constValueOpt[n.type] match {
            case Some(0) => t._1.asInstanceOf[Result]
            case Some(1) => t._2.asInstanceOf[Result]
            case Some(2) => t._3.asInstanceOf[Result]
            case _ => fallbackApply(n).asInstanceOf[Result]
          }
        case Some(4) =>
          val t = asInstanceOf[Tuple4[_, _, _, _]]
          inline constValueOpt[n.type] match {
            case Some(0) => t._1.asInstanceOf[Result]
            case Some(1) => t._2.asInstanceOf[Result]
            case Some(2) => t._3.asInstanceOf[Result]
            case Some(3) => t._4.asInstanceOf[Result]
            case _ => fallbackApply(n).asInstanceOf[Result]
          }
        case Some(s) if s > 4 && s <= $MaxSpecialized =>
          val t = asInstanceOf[Product]
          inline constValueOpt[n.type] match {
            case Some(n) if n >= 0 && n < s => t.productElement(n).asInstanceOf[Result]
            case _ => fallbackApply(n).asInstanceOf[Result]
          }
        case Some(s) if s > $MaxSpecialized =>
          val t = asInstanceOf[TupleXXL]
          inline constValueOpt[n.type] match {
            case Some(n) if n >= 0 && n < s => t.elems(n).asInstanceOf[Result]
            case _ => fallbackApply(n).asInstanceOf[Result]
          }
        case _ => fallbackApply(n).asInstanceOf[Result]
      }
    }
    else*/ dynamicApply[this.type](this, n)
}

object NonEmptyTuple {
  import Tuple._

  def dynamicHead[This <: NonEmptyTuple] (self: This): Head[This] = {
    type Result = Head[This]
    val res = (self: Any) match {
      case self: Tuple1[_] => self._1
      case self: Tuple2[_, _] => self._1
      case self: Tuple3[_, _, _] => self._1
      case self: Tuple4[_, _, _, _] => self._1
      case self: TupleXXL => self.elems(0)
      case self: Product => self.productElement(0)
    }
    res.asInstanceOf[Result]
  }

  def dynamicTail[This <: NonEmptyTuple] (self: This): Tail[This] = {
    type Result = Tail[This]
    val res = (self: Any) match {
      case self: Tuple1[_] => ()
      case self: Tuple2[_, _] => Tuple1(self._2)
      case self: Tuple3[_, _, _] => Tuple2(self._2, self._3)
      case self: Tuple4[_, _, _, _] => Tuple3(self._2, self._3, self._4)
      case _ => dynamicFromArray[Result](self.toArray.tail)
    }
    res.asInstanceOf[Result]
  }

  def dynamicApply[This <: NonEmptyTuple] (self: This, n: Int): Elem[This, n.type] = {
    type Result = Elem[This, n.type]
    val res = (self: Any) match {
      case self: TupleXXL => self.elems(n)
      case self: Product => self.productElement(n)
    }
    res.asInstanceOf[Result]
  }
}

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  def unapply[H, T <: Tuple](x: H *: T) = (x.head, x.tail)
}
