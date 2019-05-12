import scala.annotation.tailrec
import scala.compiletime._

// Third party library

sealed trait CompleteOr[T]
case class Complete[T](t: T) extends CompleteOr[T]
case class Continue[T](t: T) extends CompleteOr[T]

object Complete {
  inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
    if(c) Complete(t)
    else Continue(f)
}

import Utils._

abstract class ErasedInstances[FT] {
  def erasedMap(x: Any)(f: (Any, Any) => Any): Any
}

final class ErasedProductInstances[FT](val mirror: Mirror.Product, is0: => Array[Any]) extends ErasedInstances[FT] {
  lazy val is = is0

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

  class ArrayProduct(val elems: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

  def erasedConstruct(f: Any => Any): Any = {
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
    val n = is.length
    val arr = new Array[Any](n)
    var acc = a
    var i = 0
    while(i < n) {
      val (acc0, e0) = f(acc, is(i))
      e0 match {
        case Some(e) =>
          acc = acc0
          arr(i) = e
        case None =>
          return (acc0, None)
      }
      i = i+1
    }
    (acc, Some(mirror.fromProduct(ArrayProduct(arr))))
  }

  def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
    val x = toProduct(x0)
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i), x.productElement(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = {
    val x = toProduct(x0)
    val y = toProduct(y0)
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i), x.productElement(i), y.productElement(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  def erasedFoldLeft(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    val x = toProduct(x0)
    val n = x.productArity
    @tailrec
    def loop(i: Int, acc: Any): Any =
      if(i >= n) acc
      else
        f(acc, is(i), x.productElement(i)) match {
          case Complete(r) => r
          case Continue(acc) =>
            loop(i+1, acc)
        }

    loop(0, i)
  }

  def erasedFoldLeft2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    val x = toProduct(x0)
    val y = toProduct(y0)
    val n = x.productArity
    @tailrec
    def loop(i: Int, acc: Any): Any =
      if(i >= n) acc
      else
        f(acc, is(i), x.productElement(i), y.productElement(i)) match {
          case Complete(r) => r
          case Continue(acc) =>
            loop(i+1, acc)
        }

    loop(0, i)
  }
}

final class ErasedCoproductInstances[FT](mirror: Mirror.Sum, is0: => Array[Any]) extends ErasedInstances[FT] {
  lazy val is = is0

  def ordinal(x: Any): Any = is(mirror.ordinal(x.asInstanceOf))

  def erasedMap(x: Any)(f: (Any, Any) => Any): Any = {
    val i = ordinal(x)
    f(i, x)
  }

  def erasedProject(p: Int)(i: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) =
    f(i, is(p))

  def erasedFold(x: Any)(f: (Any, Any) => Any): Any = {
    val i = ordinal(x)
    f(i, x)
  }

  def erasedFold2(x: Any, y: Any)(a: => Any)(f: (Any, Any, Any) => Any): Any = {
    val i = mirror.ordinal(x.asInstanceOf)
    val j = mirror.ordinal(y.asInstanceOf)
    if(i == j) f(is(i), x, y)
    else a
  }
}

object K0 {
  type Generic[O] = Mirror { type MirroredType = O ; type ElemTypes }
  type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type ElemTypes }
  type CoproductGeneric[O] = Mirror.Sum { type MirroredType = O ; type ElemTypes }

  def Generic[O](implicit gen: Generic[O]): Generic[O] { type ElemTypes = gen.ElemTypes ; type Label = gen.Label ; type ElemLabels = gen.ElemLabels } = gen
  def ProductGeneric[O](implicit gen: ProductGeneric[O]): ProductGeneric[O] { type ElemTypes = gen.ElemTypes ; type Label = gen.Label ; type ElemLabels = gen.ElemLabels } = gen
  def CoproductGeneric[O](implicit gen: CoproductGeneric[O]): CoproductGeneric[O] { type ElemTypes = gen.ElemTypes ; type Label = gen.Label ; type ElemLabels = gen.ElemLabels } = gen

  type Instances[F[_], T] = ErasedInstances[F[T]]
  type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_], T] = ErasedCoproductInstances[F[T]]

  def Instances[F[_], T](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_], T](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_], T](implicit inst: CoproductInstances[F, T]): inst.type = inst

  type ToPairs[T] = T match {
    case Unit => Unit
    case a *: b => (a, ToPairs[b])
  }

  type ToUnion[T] = T match {
    case Unit => Nothing
    case a *: b => a | ToUnion[b]
  }

  // Workaround for missing literal types for negative Ints
  def narrow(x: Any): Id[x.type] = x
  val m1 = narrow(-1)
  type M1 = m1.type

  type IndexOf[E, X] = IndexOf0[E, X, 0]

  type IndexOf0[E, X, I <: Int] <: Int = X match {
    case Unit => M1
    case x *: xs => x match {
      case E => I
      case _ => IndexOf0[E, xs, S[I]]
    }
  }

  type LiftP[F[_], T] <: Tuple = T match {
    case Unit => Unit
    case a *: b => F[a] *: LiftP[F, b]
  }

  inline def summonFirst[F[_], T, U]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => implicit match {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  implicit object Ops {
    inline def (gen: ProductGeneric[Obj]) toRepr [Obj] (o: Obj): gen.ElemTypes = Mirror.productToTuple(o).asInstanceOf[gen.ElemTypes]
    inline def (gen: ProductGeneric[Obj]) fromRepr [Obj] (r: gen.ElemTypes): Obj = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj]

    inline def (gen: CoproductGeneric[Obj]) toRepr [Obj] (o: Obj): ToUnion[gen.ElemTypes] = o.asInstanceOf
    inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj] (r: ToUnion[gen.ElemTypes]): Obj = r.asInstanceOf

    inline def (inst: Instances[F, T]) map [F[_], T] (x: T)(f: [t] -> (F[t], t) => t): T =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_], T] (f: [t] -> F[t] => t): T =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) unfold [F[_], T, Acc] (i: Acc)(f: [t] -> (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_], T] (x: T, y: T)(f: [t] -> (F[t], t, t) => t): T =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_], T, Acc] (x: T)(i: Acc)(f: [t] -> (Acc, F[t], t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_], T, Acc] (x: T, y: T)(i: Acc)(f: [t] -> (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) project [F[_], T, Acc] (p: Int)(i: Acc)(f: [t] -> (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold [F[_], T, R] (x: T)(f: [t] -> (F[t], t) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_], T, R] (x: T, y: T)(a: => R)(f: [t] -> (F[t], t, t) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  type ProductGenericR[O, R] = Mirror.Product { type MirroredType = O ; type ElemTypes = R }
  type CoproductGenericR[O, R] = Mirror.Sum { type MirroredType = O ; type ElemTypes = R }

  inline implicit def mkInstances[F[_], T](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: Mirror.Product   => mkProductInstances[F, T](gen.asInstanceOf[ProductGeneric[T] { type ElemTypes = gen.ElemTypes }])
      case c: Mirror.Sum => mkCoproductInstances[F, T](gen.asInstanceOf[CoproductGeneric[T] { type ElemTypes = gen.ElemTypes }])
    }

  inline implicit def mkProductInstances[F[_], T](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_], T](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf
}

object K1 {
  type Generic[O[_]] = Mirror { type MirroredType = O ; type ElemTypes[_] }
  type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type ElemTypes[_] }
  type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type ElemTypes[_] }

  def Generic[O[_]](implicit gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_]](implicit gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_]](implicit gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_]], T[_]](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_]], T[_]](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_]], T[_]](implicit inst: CoproductInstances[F, T]): inst.type = inst

  class Da
  class Db
  type Dummy = Da & Db
  type Apply[F[_]] = F[Dummy]

  type LiftP[F[_[_]], T[_]] = LiftP0[F, K0.ToPairs[Apply[T]]]

  type LiftP0[F[_[_]], T] <: Tuple = T match {
    case Unit => Unit
    case (Apply[a],  b) => F[a] *: LiftP0[F, b]
    case (Dummy, b) => F[Id] *: LiftP0[F, b]
    case (a, b) => F[Const[a]] *: LiftP0[F, b]
  }

  inline def summonFirst[F[_[_]], T[_], U[_]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => implicit match {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  implicit object Ops {
    inline def (gen: ProductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): gen.ElemTypes[A] = Mirror.productToTuple(o).asInstanceOf[gen.ElemTypes[A]]
    inline def (gen: ProductGeneric[Obj]) fromRepr [Obj[_], A] (r: gen.ElemTypes[A]): Obj[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj[A]]

    inline def (gen: CoproductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): K0.ToUnion[gen.ElemTypes[A]] = o.asInstanceOf
    inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj[_], A] (r: K0.ToUnion[gen.ElemTypes[A]]): Obj[A] = r.asInstanceOf

    inline def (inst: Instances[F, T]) map[F[_[_]], T[_], A, R](x: T[A])(f: [t[_]] -> (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_]], T[_], R] (f: [t[_]] -> F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(f: [t[_]] -> (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_]], T[_], A, Acc] (x: T[A])(i: Acc)(f: [t[_]] -> (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_]], T[_], A, B, Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_]] -> (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_]], T[_], A, R] (x: T[A])(f: [t[_]] -> (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(a: => R)(f: [t[_]] -> (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline implicit def mkInstances[F[_[_]], T[_]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: Mirror.Product   => mkProductInstances[F, T](gen.asInstanceOf[ProductGeneric[T] { type ElemTypes = gen.ElemTypes }])
      case c: Mirror.Sum => mkCoproductInstances[F, T](gen.asInstanceOf[CoproductGeneric[T] { type ElemTypes = gen.ElemTypes }])
    }

  inline implicit def mkProductInstances[F[_[_]], T[_]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_[_]], T[_]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf

  implicit def mkK1_0[O](implicit k0: K0.ProductGeneric[O]): ProductGeneric[Const[O]] { type ElemTypes = Const[k0.ElemTypes] } = k0.asInstanceOf
}

object K11 {
  type Generic[O[_[_]]] = Mirror { type MirroredType = O ; type ElemTypes[_[_]] }
  type ProductGeneric[O[_[_]]] = Mirror.Product { type MirroredType = O ; type ElemTypes[_[_]] }
  type CoproductGeneric[O[_[_]]] = Mirror.Sum { type MirroredType = O ; type ElemTypes[_[_]] }

  def Generic[O[_[_]]](implicit gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_[_]]](implicit gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_[_]]](implicit gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_[_]]], T[_[_]]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_[_]]], T[_[_]]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_[_]]], T[_[_]]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_[_]]], T[_[_]]](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_[_]]], T[_[_]]](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_[_]]], T[_[_]]](implicit inst: CoproductInstances[F, T]): inst.type = inst

  type Id[t] = [f[_]] => f[t]
  type Const[c] = [f[_]] => c

  type Dummy[_]
  type Apply[F[_[_]]] = F[Dummy]

  type LiftP[F[_[_[_]]], T[_[_]]] = LiftP0[F, K0.ToPairs[Apply[T]]]

  type LiftP0[F[_[_[_]]], T] <: Tuple = T match {
    case Unit => Unit
    case (Apply[a], b) => F[a] *: LiftP0[F, b]
    case (Dummy[a], b) => F[Id[a]] *: LiftP0[F, b]
    case (a, b) => F[Const[a]] *: LiftP0[F, b]
  }

  implicit object Ops {
    inline def (inst: Instances[F, T]) map[F[_[_[_]]], T[_[_]], A[_], R[_]](x: T[A])(f: [t[_[_]]] -> (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_[_]]], T[_[_]], R[_]] (f: [t[_[_]]] -> F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_[_]]], T[_[_]], A[_], B[_], R[_]] (x: T[A], y: T[B])(f: [t[_[_]]] -> (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_[_]]], T[_[_]], A[_], Acc] (x: T[A])(i: Acc)(f: [t[_[_]]] -> (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_[_]]], T[_[_]], A[_], B[_], Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_[_]]] -> (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_[_]]], T[_[_]], A[_], R] (x: T[A])(f: [t[_[_]]] -> (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_[_]]], T[_[_]], A[_], B[_], R] (x: T[A], y: T[B])(a: => R)(f: [t[_[_]]] -> (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline implicit def mkInstances[F[_[_[_]]], T[_[_]]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: Mirror.Product   => mkProductInstances[F, T](gen.asInstanceOf[ProductGeneric[T] { type ElemTypes = gen.ElemTypes }])
      case c: Mirror.Sum => mkCoproductInstances[F, T](gen.asInstanceOf[CoproductGeneric[T] { type ElemTypes = gen.ElemTypes }])
    }

  inline implicit def mkProductInstances[F[_[_[_]]], T[_[_]]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_[_[_]]], T[_[_]]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf
}

object K2 {
  type Generic[O[_, _]] = Mirror { type MirroredType = O ; type ElemTypes[_, _] }
  type ProductGeneric[O[_, _]] = Mirror.Product { type MirroredType = O ; type ElemTypes[_, _] }
  type CoproductGeneric[O[_, _]] = Mirror.Sum { type MirroredType = O ; type ElemTypes[_, _] }

  def Generic[O[_, _]](implicit gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_, _]](implicit gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_, _]](implicit gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_, _]], T[_, _]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_, _]], T[_, _]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_, _]], T[_, _]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_, _]], T[_, _]](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_, _]], T[_, _]](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_, _]], T[_, _]](implicit inst: CoproductInstances[F, T]): inst.type = inst

  type Id1[t, u] = t
  type Id2[t, u] = u
  type Const[c] = [t, u] => c

  class D1a
  class D1b
  class D2a
  class D2b

  type Dummy1 = D1a & D1b
  type Dummy2 = D1a & D2b
  type Apply[F[_, _]] = F[Dummy1, Dummy2]

  type LiftP[F[_[_, _]], T[_, _]] = LiftP0[F, K0.ToPairs[Apply[T]]]

  type LiftP0[F[_[_, _]], T] <: Tuple = T match {
    case Unit => Unit
    case (Apply[a], b) => F[a] *: LiftP0[F, b]
    case (Dummy1, b) => F[Id1] *: LiftP0[F, b]
    case (Dummy2, b) => F[Id2] *: LiftP0[F, b]
    case (a, b) => F[Const[a]] *: LiftP0[F, b]
  }

  implicit object Ops {
    inline def (inst: Instances[F, T]) map[F[_[_, _]], T[_, _], A, B, R, S](x: T[A, B])(f: [t[_, _]] -> (F[t], t[A, B]) => t[R, S]): T[R, S] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_, _]], T[_, _], R, S] (f: [t[_, _]] -> F[t] => t[R, S]): T[R, S] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_, _]], T[_, _], A, B, C, D, R, S] (x: T[A, B], y: T[C, D])(f: [t[_, _]] -> (F[t], t[A, B], t[C, D]) => t[R, S]): T[R, S] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_, _]], T[_, _], A, B, Acc] (x: T[A, B])(i: Acc)(f: [t[_, _]] -> (Acc, F[t], t[A, B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_, _]], T[_, _], A, B, C, D, Acc] (x: T[A, B], y: T[C, D])(i: Acc)(f: [t[_, _]] -> (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_, _]], T[_, _], A, B, R] (x: T[A, B])(f: [t[_, _]] -> (F[t], t[A, B]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_, _]], T[_, _], A, B, C, D, R] (x: T[A, B], y: T[C, D])(a: => R)(f: [t[_, _]] -> (F[t], t[A, B], t[C, D]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline implicit def mkInstances[F[_[_, _]], T[_, _]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: Mirror.Product   => mkProductInstances[F, T](gen.asInstanceOf[ProductGeneric[T] { type ElemTypes = gen.ElemTypes }])
      case c: Mirror.Sum => mkCoproductInstances[F, T](gen.asInstanceOf[CoproductGeneric[T] { type ElemTypes = gen.ElemTypes }])
    }

  inline implicit def mkProductInstances[F[_[_, _]], T[_, _]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_[_, _]], T[_, _]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[LiftP[F, gen.ElemTypes]]).asInstanceOf
}
