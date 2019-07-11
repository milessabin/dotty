package playground

import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec
import scala.compiletime._

object Shapeless3 {
  type Id[t] = t
  type Const[c] = [t] =>> c
  case class Wrap[T](t: T)


  type ~>[A[_], B[_]] = [t] => A[t] => B[t]

  inline def summon[T] = implicit match {
    case t: T => t
  }

  inline def summonAsArray[T <: Tuple]: Array[Any] =
    summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

  inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
    case _: Unit => arr
    case _: (a *: b) =>
      arr(i) = summon[a]
      summonAsArray0[b](i+1, arr)
  }

  sealed trait CompleteOr[T]
  case class Complete[T](t: T) extends CompleteOr[T]
  case class Continue[T](t: T) extends CompleteOr[T]

  object Complete {
    inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
      if(c) Complete(t)
      else Continue(f)
  }

  abstract class ErasedInstances[FT] {
    def erasedMap(x: Any)(f: (Any, Any) => Any): Any
  }

  abstract class ErasedProductInstances[FT] extends ErasedInstances[FT] {
    def erasedConstruct(f: Any => Any): Any
    def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any])
    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any
    def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any
    def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any
    def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any
  }

  final class ErasedProductInstances0[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
    def erasedConstruct(f: Any => Any): Any = mirror.fromProduct(None)
    def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = (a, Some(mirror.fromProduct(None)))
    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = x0
    def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = x0
    def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = a
    def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = a
  }

  abstract class ErasedProductInstances1[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
    def mkI: Any
    lazy val i = mkI

    inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

    final def erasedConstruct(f: Any => Any): Any =
      mirror.fromProduct(Tuple1(f(i)))

    final def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
      val (acc0, e0) = f(a, i)
      e0 match {
        case Some(_) => (acc0, Some(mirror.fromProduct(e0)))
        case None => (acc0, None)
      }
    }

    final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any =
      mirror.fromProduct(Tuple1(f(i, toProduct(x0).productElement(0))))

    final def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any =
      mirror.fromProduct(Tuple1(f(i, toProduct(x0).productElement(0), toProduct(y0).productElement(0))))

    final def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
      f(a, i, toProduct(x0).productElement(0)) match {
        case Complete(r) => r
        case Continue(acc) => acc
      }
    }

    final def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
      f(a, i, toProduct(x0).productElement(0), toProduct(y0).productElement(0)) match {
        case Complete(r) => r
        case Continue(acc) => acc
      }
    }
  }

  abstract class ErasedProductInstancesN[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
    import ErasedProductInstances.ArrayProduct

    def mkIs: Array[Any]
    lazy val is = mkIs

    inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

    final def erasedConstruct(f: Any => Any): Any = {
      val n = is.length
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i))
        i = i+1
      }
      mirror.fromProduct(ArrayProduct(arr))
    }

    final def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
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

    final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
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

    final def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = {
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

    final def erasedFoldLeft(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
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

    final def erasedFoldLeft2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
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

  object ErasedProductInstances {
    class ArrayProduct(val elems: Array[Any]) extends Product {
      def canEqual(that: Any): Boolean = true
      def productElement(n: Int) = elems(n)
      def productArity = elems.length
      override def productIterator: Iterator[Any] = elems.iterator
    }

    inline def summonOne[T] = inline erasedValue[T] match {
      case _: Tuple1[a] => summon[a]
    }

    inline def apply[FT, E <: Tuple](mirror: Mirror.Product) : ErasedProductInstances[FT] =
      inline erasedValue[Tuple.Size[E]] match {
        case 0 => new ErasedProductInstances0[FT](mirror)
        case 1 => new ErasedProductInstances1[FT](mirror) { def mkI = summonOne[E] }
        case _ => new ErasedProductInstancesN[FT](mirror) { def mkIs = summonAsArray[E] }
      }
  }

  abstract class ErasedCoproductInstances[FT](mirror: Mirror.Sum) extends ErasedInstances[FT] {
    def mkIs: Array[Any]
    lazy val is = mkIs

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

  object ErasedCoproductInstances {
    inline def apply[FT, E <: Tuple](mirror: Mirror.Sum) : ErasedCoproductInstances[FT] =
      new ErasedCoproductInstances[FT](mirror) {
        def mkIs = summonAsArray[E]
      }
  }

  object K0 {
    type Generic[O] = Mirror { type MirroredType = O ; type MirroredElemTypes }
    type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes }
    type CoproductGeneric[O] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes }

    def Generic[O] given (gen: Generic[O]): Generic[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
    def ProductGeneric[O] given (gen: ProductGeneric[O]): ProductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
    def CoproductGeneric[O] given (gen: CoproductGeneric[O]): CoproductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen

    type Instances[F[_], T] = ErasedInstances[F[T]]
    type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]
    type CoproductInstances[F[_], T] = ErasedCoproductInstances[F[T]]

    def Instances[F[_], T] given (inst: Instances[F, T]): inst.type = inst
    def ProductInstances[F[_], T] given (inst: ProductInstances[F, T]): inst.type = inst
    def CoproductInstances[F[_], T] given (inst: CoproductInstances[F, T]): inst.type = inst

    type ToUnion[T] = T match {
      case Unit => Nothing
      case a *: b => a | ToUnion[b]
    }

    type IndexOf[E, X] = IndexOf0[E, X, 0]

    type IndexOf0[E, X, I <: Int] <: Int = X match {
      case Unit => -1
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

    given Ops {
      inline def (gen: ProductGeneric[Obj]) toRepr [Obj] (o: Obj): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
      inline def (gen: ProductGeneric[Obj]) fromRepr [Obj] (r: gen.MirroredElemTypes): Obj = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj]

      inline def (gen: CoproductGeneric[Obj]) toRepr [Obj] (o: Obj): ToUnion[gen.MirroredElemTypes] = o.asInstanceOf
      inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj] (r: ToUnion[gen.MirroredElemTypes]): Obj = r.asInstanceOf

      inline def (inst: Instances[F, T]) map [F[_], T] (x: T)(f: [t] => (F[t], t) => t): T =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

      inline def (inst: ProductInstances[F, T]) construct [F[_], T] (f: [t] => F[t] => t): T =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) unfold [F[_], T, Acc] (i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) map2 [F[_], T] (x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft [F[_], T, Acc] (x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_], T, Acc] (x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

      inline def (inst: CoproductInstances[F, T]) project [F[_], T, Acc] (p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: CoproductInstances[F, T]) fold [F[_], T, R] (x: T)(f: [t] => (F[t], t) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def (inst: CoproductInstances[F, T]) fold2 [F[_], T, R] (x: T, y: T)(a: => R)(f: [t] => (F[t], t, t) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    }

    type ProductGenericR[O, R] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes = R }
    type CoproductGenericR[O, R] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes = R }

    inline given mkInstances[F[_], T] as ErasedInstances[F[T]] given (gen: Generic[T]) =
      inline gen match {
        case p: ProductGeneric[T]   => mkProductInstances[F, T] given p
        case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
      }

    inline given mkProductInstances[F[_], T] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
      ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

    inline given mkCoproductInstances[F[_], T] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
      ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)
  }

  object K1 {
    type Generic[O[_]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_] }
    type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
    type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

    def Generic[O[_]] given (gen: Generic[O]): gen.type = gen
    def ProductGeneric[O[_]] given (gen: ProductGeneric[O]): gen.type = gen
    def CoproductGeneric[O[_]] given (gen: CoproductGeneric[O]): gen.type = gen

    type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
    type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
    type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

    def Instances[F[_[_]], T[_]] given (inst: Instances[F, T]): inst.type = inst
    def ProductInstances[F[_[_]], T[_]] given (inst: ProductInstances[F, T]): inst.type = inst
    def CoproductInstances[F[_[_]], T[_]] given (inst: CoproductInstances[F, T]): inst.type = inst

    class Dummy
    type Apply[T[_]] = T[Dummy]
    type Unapply[F[_[_]], T] = T match {
      case Wrap[Apply[a]] => F[a]
      case Wrap[Dummy] => F[Id]
      case Wrap[c] => F[Const[c]]
    }

    type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

    type LiftP0[F[_[_]], T] <: Tuple = T match {
      case Unit => Unit
      case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
    }

    inline def summonFirst[F[_[_]], T[_], U[_]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

    inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
      case _: (a *: b) => implicit match {
        case aa: `a` => aa
        case _ => summonFirst0[b]
      }
    }

    given Ops {
      inline def (gen: ProductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
      inline def (gen: ProductGeneric[Obj]) fromRepr [Obj[_], A] (r: gen.MirroredElemTypes[A]): Obj[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj[A]]

      inline def (gen: CoproductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
      inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj[_], A] (r: K0.ToUnion[gen.MirroredElemTypes[A]]): Obj[A] = r.asInstanceOf

      inline def (inst: Instances[F, T]) map[F[_[_]], T[_], A, R](x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

      inline def (inst: ProductInstances[F, T]) construct [F[_[_]], T[_], R] (f: [t[_]] => F[t] => t[R]): T[R] =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) map2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(f: [t[_]] => (F[t], t[A], t[B]) => t[R]): T[R] =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_]], T[_], A, Acc] (x: T[A])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_]], T[_], A, B, Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

      inline def (inst: CoproductInstances[F, T]) fold [F[_[_]], T[_], A, R] (x: T[A])(f: [t[_]] => (F[t], t[A]) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(a: => R)(f: [t[_]] => (F[t], t[A], t[B]) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    }

    inline given mkInstances[F[_[_]], T[_]] as ErasedInstances[F[T]] given (gen: Generic[T]) =
      inline gen match {
        case p: ProductGeneric[T] => mkProductInstances[F, T] given p
        case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
      }

    inline given mkProductInstances[F[_[_]], T[_]] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
      ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

    inline given mkCoproductInstances[F[_[_]], T[_]] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
      ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

    type LiftProductGeneric[O, E] = ProductGeneric[Const[O]] { type MirroredElemTypes = Const[E] }

    given mkK1_0[O] as LiftProductGeneric[O, k0.MirroredElemTypes] given (k0: K0.ProductGeneric[O]) = k0.asInstanceOf
  }

  trait Eq[A] {
    def eqv(x: A, y: A): Boolean
  }

  object Eq {
    inline def apply[A] given (ea: Eq[A]): Eq[A] = ea

    given as Eq[Unit] {
      def eqv(x: Unit, y: Unit): Boolean = true
    }
    given as Eq[Boolean] {
      def eqv(x: Boolean, y: Boolean): Boolean = x == y
    }
    given as Eq[Int] {
      def eqv(x: Int, y: Int): Boolean = x == y
    }
    given as Eq[String] {
      def eqv(x: String, y: String): Boolean = x == y
    }

    given eqGen[A] as Eq[A] given (inst: K0.ProductInstances[Eq, A]) {
      def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
        [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
      )
    }

    given eqGenC[A] as Eq[A] given (inst: => K0.CoproductInstances[Eq, A]) {
      def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
        [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
      )
    }

    inline def derived[A] given (gen: K0.Generic[A]): Eq[A] = inline gen match {
      case p: K0.ProductGeneric[A]   => given as p.type = p ; eqGen
      case c: K0.CoproductGeneric[A] => given as c.type = c ; eqGenC
    }
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    inline def apply[F[_]] given (ff: Functor[F]): Functor[F] = ff

    given as Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

    given [F[_], G[_]] as Functor[[t] =>> F[G[t]]] given (ff: Functor[F], fg: Functor[G]) {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = ff.map(fga)(ga => fg.map(ga)(f))
    }

    given functorGen[F[_]] as Functor[F] given (inst: => K1.Instances[Functor, F]) {
      def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))
    }

    given [T] as Functor[Const[T]] {
      def map[A, B](t: T)(f: A => B): T = t
    }

    inline def derived[F[_]] given (gen: K1.Generic[F]): Functor[F] = functorGen
  }
}

object Inlined {
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    import scala.compiletime.erasedValue
    import compiletime._
    import scala.deriving._

    inline def tryEql[TT](x: TT, y: TT): Boolean = delegate match {
      case eq: Eq[TT] => eq.eqv(x, y)
    }

    inline def eqlElemv[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
          eqlElemv[elems1](n + 1)(x, y)
        case _: Unit =>
          true
      }

    inline def eqlProducv[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
      eqlElemv[m.MirroredElemTypes](0)(x, y)

    inline def eqlCasev[Alts](n: Int)(x: Any, y: Any, ord: Int): Boolean =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt`] => eqlElemv[m.MirroredElemTypes](0)(x, y)
            }
          else eqlCasev[alts1](n + 1)(x, y, ord)
        case _: Unit =>
          false
      }

    inline def derived[T](implicit ev: Mirror.Of[T]): Eq[T] = new Eq[T] {
      def eqv(x: T, y: T): Boolean =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            ord == m.ordinal(y) && eqlCasev[m.MirroredElemTypes](0)(x, y, ord)
          case m: Mirror.ProductOf[T] =>
            eqlElemv[m.MirroredElemTypes](0)(x, y)
        }
    }

    implicit object IntEq extends Eq[Int] {
      def eqv(x: Int, y: Int) = x == y
    }

    implicit object BooleanEq extends Eq[Boolean] {
      def eqv(x: Boolean, y: Boolean) = x == y
    }
  }
}

type RE[X] = given QuoteContext => E[X]

implicit class ListOps[A](l: List[A]) {
  def safeZip[B](o: List[B]): List[(A, B)] = {
    assert(l.size == o.size)
    l zip o
  }
}

// Third party library

class K0SummonInstances[F[_], T](val instances: List[Any])
object K0SummonInstances {
  implicit def caseNil[F[_]]: K0SummonInstances[F, Unit] =
    new K0SummonInstances[F, Unit](Nil)

  implicit def caseCons[F[_], H, T <: Tuple](implicit h: F[H], t: K0SummonInstances[F, T]): K0SummonInstances[F, H *: T] =
    new K0SummonInstances[F, H *: T](h :: t.instances)
}

trait K0StagedInstances[F[_], T]
// { def map(x: T)(f: [t] => (F[t], t) => t): T }

trait K0StagedProductInstances[F[_], T] extends K0StagedInstances[F, T] {
  def instances: List[Any]
  def accessorsE(value: E[T])(implicit r: QuoteContext): List[E[Any]]
  def constructorE(fields: List[E[Any]])(implicit r: QuoteContext): E[T]

  def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] =
    accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
      case (acc, ((xn, yn), in)) => f(acc, in.asInstanceOf, xn, yn)
    }
}

object K0StagedProductInstances {
  implicit def apply[F[_], T]
    (implicit
      m: Mirror.ProductOf[T],
      t: Type[T],
      s: K0SummonInstances[F, m.MirroredElemTypes]
    ): K0StagedProductInstances[F, T] = new K0StagedProductInstances[F, T] {
      def instances: List[Any] = s.instances

      def accessorsE(value: E[T])(implicit r: QuoteContext): List[E[Any]] = {
        import r.tasty._
        t.unseal.symbol match {
          case IsClassDefSymbol(self) => // case class
            self.caseFields.map { field =>
              Select.unique(value.unseal, field.name).seal
            }
          case _ => Nil // case object
        }
      }

      def constructorE(fields: List[E[Any]])(implicit r: QuoteContext): E[T] = {
        import r.tasty._
        val companion = t.unseal.tpe match {
          case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
          case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
        }
        Select.overloaded(Ident(companion), "apply", Nil, fields.map(_.unseal)).seal.cast[T]
      }
    }
}

trait K0StagedCoproductInstances[F[_], T] extends K0StagedInstances[F, T] {
  def instances: List[Any]
  def typetestsE(value: E[T])(implicit r: QuoteContext): List[E[Boolean]]
  def castsE(value: E[T])(implicit r: QuoteContext): List[E[Any]]

  def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): RE[R] =
    typetestsE(x).safeZip(castsE(x)).safeZip(typetestsE(y).safeZip(castsE(y))).safeZip(instances).foldLeft(i) {
      case (acc, (((tx, cx), (ty, cy)), in)) =>
        '{ if ($tx && $ty) ${ f(in.asInstanceOf, cx, cy) } else $acc }
    }
}

object K0StagedCoproductInstances {
  implicit def apply[F[_], T]
    (implicit
      m: Mirror.SumOf[T],
      t: Type[T],
      s: K0SummonInstances[F, m.MirroredElemTypes],
      x: K0SummonInstances[Type, m.MirroredElemTypes],
    ): K0StagedCoproductInstances[F, T] = new K0StagedCoproductInstances[F, T] {
      def instances: List[Any] = s.instances

      def typetestsE(value: E[T])(implicit r: QuoteContext): List[E[Boolean]] =
        x.instances.map { case tpe: Type[_] => '{ $value.isInstanceOf[$tpe] } }

      def castsE(value: E[T])(implicit r: QuoteContext): List[E[Any]] = {
        x.instances.map { case tpe: Type[_] => '{ $value.asInstanceOf[$tpe] } }
      }
    }
}

type Id[t] = t
type Const[c] = [t] =>> c

case class Wrap[T](t: T)
class Dummy
type Apply[T[_]] = T[Dummy]
type Unapply[F[_[_]], T] = T match {
  case Wrap[Apply[a]] => F[a]
  case Wrap[Dummy] => F[Id]
  case Wrap[c] => F[Const[c]]
}

type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

class K1SummonInstances[F[_[_]], G[_]](val instances: List[Any])
object K1SummonInstances {
  implicit def applied[F[_[_]], T[_]](implicit s: K1SummonInstances0[F, Apply[T]]): K1SummonInstances[F, T] = {
    // assert(s.instances.nonEmpty)
    new K1SummonInstances[F, T](s.instances)
  }
}

class K1SummonInstances0[F[_[_]], T](val instances: List[Any])
object K1SummonInstances0 {
  implicit def caseNil[F[_[_]]]: K1SummonInstances0[F, Unit] =
    new K1SummonInstances0[F, Unit](Nil)

  implicit def caseCons[F[_[_]], H, T <: Tuple]
    (implicit
      h: Unapply[F, Wrap[H]],
      t: K1SummonInstances0[F, T]
    ): K1SummonInstances0[F, H *: T] =
      new K1SummonInstances0[F, H *: T](h :: t.instances)
}

trait K1StagedInstances[F[_[_]], T[_]] {
  def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]]
}

object K1StagedInstances {
  implicit def fromStagedProductInstances[F[_[_]], T[_]]
    (implicit spi: K1StagedProductInstances[F, T]): K1StagedInstances[F, T] = spi

  implicit def fromStagedCoproductInstances[F[_[_]], T[_]]
    (implicit spi: K1StagedCoproductInstances[F, T]): K1StagedInstances[F, T] = spi
}

trait K1StagedProductInstances[F[_[_]], T[_]] extends K1StagedInstances[F, T] {
  def instances: List[Any]
  def accessorsE[A](value: E[T[A]])(implicit r: QuoteContext): List[E[Any]]
  def constructorE[A](fields: List[E[Any]])(implicit r: QuoteContext): E[T[A]]

  def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] = {
    val args = instances.safeZip(accessorsE(x)).map((in, a) => f(in.asInstanceOf, a.asInstanceOf))
    constructorE(args)
  }
}

object K1StagedProductInstances {
  implicit def apply[F[_[_]], T[_]]
    (implicit
      m: ProductGeneric[T],
      t: Type[T],
      s: K1SummonInstances[F, m.MirroredElemTypes]
    ): K1StagedProductInstances[F, T] = new K1StagedProductInstances[F, T] {
      def instances: List[Any] = s.instances

      def accessorsE[A](value: E[T[A]])(implicit r: QuoteContext): List[E[Any]] = {
        import r.tasty._
        t.unseal.symbol match {
          case IsClassDefSymbol(self) =>
            self.caseFields.map { field =>
              Select.unique(value.unseal, field.name).seal
            }
          case NoSymbol() =>
            t.unseal.tpe match {
              case Type.TypeLambda(_, _, Type.AppliedType(tcons, _)) =>
                tcons.classSymbol match {
                  case Some(IsClassDefSymbol(self)) =>
                    self.caseFields.map { field =>
                      Select.unique(value.unseal, field.name).seal
                    }
                  case _ => Nil
                }
              case _ => Nil
            }
        }
      }

      def constructorE[A](fields: List[E[Any]])(implicit r: QuoteContext): E[T[A]] = {
        import r.tasty._
        def companion(tpe: Type): TermRef = tpe match {
          case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
          case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
          case Type.TypeLambda(_, _, Type.AppliedType(tcons, _)) => companion(tcons)
        }
        def defaultTargs(tpe: Type): List[Type] = tpe match {
          case Type.SymRef(_, _)   => Nil
          case Type.TypeRef(_, _) => Nil
          case Type.TypeLambda(_, bounds, _) =>
            // Do we need to call .low for covariant cases? I don't think we do...
            bounds.map(_.hi)
        }
        val t0 = t.unseal.tpe
        Select.overloaded(Ident(companion(t0)), "apply", defaultTargs(t0), fields.map(_.unseal))
          .seal.asInstanceOf[E[T[A]]]
      }
    }
}

trait K1StagedCoproductInstances[F[_[_]], T[_]] extends K1StagedInstances[F, T] {
  def instances: List[Any]
  def typetestsE[A](value: E[T[A]])(implicit r: QuoteContext): List[E[Boolean]]
  def castsE[A](value: E[T[A]])(implicit r: QuoteContext): List[E[Any]]

  def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] =
    instances.safeZip(typetestsE(x).safeZip(castsE(x))).foldLeft('{ null }) {
      case (acc, (i, (t, c))) => '{ if ($t) ${ f(i.asInstanceOf, c.asInstanceOf) } else $acc }
    }.asInstanceOf
}

object K1StagedCoproductInstances {
  implicit def apply[F[_[_]], T[_]]
    (implicit
      m: CoproductGeneric[T],
      t: Type[T],
      s: K1SummonInstances[F, m.MirroredElemTypes],
      x: K1SummonInstances[Type, m.MirroredElemTypes],
    ): K1StagedCoproductInstances[F, T] = new K1StagedCoproductInstances[F, T] {
      def instances: List[Any] = s.instances

      def typetestsE[A](value: E[T[A]])(implicit r: QuoteContext): List[E[Boolean]] =
        x.instances.map { case tpe: Type[_] =>
          trait DummyK[X]
          val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
          '{ ${ value.asInstanceOf[E[T[Any]]] }.isInstanceOf[$tpe0[Any]] }
        }

      def castsE[A](value: E[T[A]])(implicit r: QuoteContext): List[E[Any]] =
        x.instances.map { case tpe: Type[_] =>
          import r.tasty._
          tpe.unseal.tpe match {
            case Type.TypeLambda(_, bound :: Nil, _) =>
              trait DummyK[X]
              val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
              '{ ${ value.asInstanceOf[E[T[Any]]] }.asInstanceOf[$tpe0[Any]] }
          }
        }
    }
}

// Type class definitions

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

object Eq {
  inline def derived[T](implicit inline e: => Eq0[T]): Eq[T] =
    new Eq[T] {
      def eqv(a: T, b: T): Boolean = eqStaged(a, b)
    }

  inline def eqStaged[T](f1: T, f2: T)(implicit inline e: => Eq0[T]): Boolean =
    ${ eqStagedImpl('f1, 'f2)(e) }

  def eqStagedImpl[T](f1: E[T], f2: E[T])(e: Eq0[T])(implicit r: QuoteContext): E[Boolean] =
    e.eqv(f1, f2)
}

trait Eq0[A] {
  def eqv(x: E[A], y: E[A]): RE[Boolean]
}

object Eq0 {
  implicit def eqUnit: Eq0[Unit] = new Eq0[Unit] {
    def eqv(x: E[Unit], y: E[Unit]): RE[Boolean] =
      '{ true }
  }

  implicit def eqBoolean: Eq0[Boolean] = new Eq0[Boolean] {
    def eqv(x: E[Boolean], y: E[Boolean]): RE[Boolean] =
      '{ $x == $y }
  }

  implicit def eqInt: Eq0[Int] = new Eq0[Int] {
    def eqv(x: E[Int], y: E[Int]): RE[Boolean] =
      '{ $x == $y }
  }

  implicit def eqString: Eq0[String] = new Eq0[String] {
    def eqv(x: E[String], y: E[String]): RE[Boolean] =
      '{ $x == $y }
  }

  implicit def eqGenP[A](implicit inst: K0StagedProductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): RE[Boolean] =
        inst.foldLeft2E(x, y)('{ true })(
          [t] => (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
            '{ $acc && ${ eqt.eqv(t0, t1) }}
        )
    }

  implicit def eqGenC[A](implicit inst: K0StagedCoproductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): RE[Boolean] = inst.fold2E(x, y)('{ false })(
        [t] => (eqt: Eq0[t], t0: E[t], t1: E[t]) => eqt.eqv(t0, t1)
      )
    }
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  inline def derived[F[_]](implicit inline e: => Functor0[F]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = mapStaged(fa, f)
    }

  inline def mapStaged[F[_], A, B](fa: F[A], f: A => B)(implicit inline e: => Functor0[F]): F[B] =
    ${ mapStagedImpl('fa, 'f)(e) }

  def mapStagedImpl[F[_], A: Type, B: Type](fa: E[F[A]], f: E[A => B])(e: Functor0[F])(implicit r: QuoteContext): E[F[B]] =
    e.map(fa)(f)
}

trait Functor0[F[_]] {
  def map[A: Type, B: Type](fa: E[F[A]])(f: E[A => B]): RE[F[B]]
}

object Functor0 {
  implicit def functorId: Functor0[Id] = new Functor0[Id] {
    def map[A: Type, B: Type](a: E[A])(f: E[A => B]): RE[B] = '{ $f($a) }
  }

  implicit def functorConst[T]: Functor0[Const[T]] = new Functor0[Const[T]] {
    def map[A: Type, B: Type](t: E[T])(f: E[A => B]): RE[T] = t
  }

  implicit def functorNested[F[_]: Type, G[_]: Type](implicit ff: Functor0[F], fg: Functor0[G]): Functor0[[t] =>> F[G[t]]] =
    new Functor0[[t] =>> F[G[t]]] {
      def map[A: Type, B: Type](fga: E[F[G[A]]])(f: E[A => B]): RE[F[G[B]]] =
        ff.map(fga)('{ ga => ${ fg.map('{ ga })(f) } })
        // ff.map[G[A], G[B]](fga)('{ (ga: G[A]) => ${ fg.map[A, B]('{ ga })(f) } })
    }

  implicit def derived[F[_]](implicit inst: K1StagedInstances[Functor0, F]): Functor0[F] =
    new Functor0[F] {
      def map[A: Type, B: Type](fa: E[F[A]])(f: E[A => B]): RE[F[B]] =
        inst.mapE(fa)([t[_]] => (ft: Functor0[t], ta: E[t[A]]) => ft.map(ta)(f))
    }
}

// ADTs

case class ISB(i: Int, s: String, b: Boolean) // derives Eq

sealed trait OptionInt // derives Eq
case class SomeInt(value: Int) extends OptionInt
case object NoneInt extends OptionInt

object OptionInt

sealed trait IList
case class ICons(hd: Int, tl: IList) extends IList
case object INil extends IList

object IList

case class Box[A](x: A) // derives Functor

sealed trait Opt[+A] // derives Functor
case class Sm[+A](value: A) extends Opt[A]
case object Nn extends Opt[Nothing]

object Opt

sealed trait CList[+A] // derives Functor
case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
case object CNil extends CList[Nothing]

object CList

// Benchmarks

type I = Int
type B = Boolean

case class P0()
case class P10(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B)
case class P20(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B)
case class P30(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B
)
case class P40(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B
)
case class P50(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B
)
case class P60(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B
)
case class P70(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B
)
case class P80(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B,
  a7: I, b7: B, c7: I, d7: B, e7: I, f7: B, g7: I, h7: B, i7: I, j7: B
)
case class P90(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B,
  a7: I, b7: B, c7: I, d7: B, e7: I, f7: B, g7: I, h7: B, i7: I, j7: B,
  a8: I, b8: B, c8: I, d8: B, e8: I, f8: B, g8: I, h8: B, i8: I, j8: B
)
case class P100(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B,
  a7: I, b7: B, c7: I, d7: B, e7: I, f7: B, g7: I, h7: B, i7: I, j7: B,
  a8: I, b8: B, c8: I, d8: B, e8: I, f8: B, g8: I, h8: B, i8: I, j8: B,
  a9: I, b9: B, c9: I, d9: B, e9: I, f9: B, g9: I, h9: B, i9: I, j9: B
)

sealed trait C10
object C10

case class C10_A0(i: I) extends C10
case class C10_B0(b: B) extends C10
case class C10_C0(i: I) extends C10
case class C10_D0(b: B) extends C10
case class C10_E0(i: I) extends C10
case class C10_F0(b: B) extends C10
case class C10_G0(i: I) extends C10
case class C10_H0(b: B) extends C10
case class C10_I0(i: I) extends C10
case class C10_J0(b: B) extends C10

sealed trait C20; object C20
  case class C20_A0(i: I) extends C20; case class C20_B0(b: B) extends C20; case class C20_C0(i: I) extends C20; case class C20_D0(b: B) extends C20; case class C20_E0(i: I) extends C20; case class C20_F0(b: B) extends C20; case class C20_G0(i: I) extends C20; case class C20_H0(b: B) extends C20; case class C20_I0(i: I) extends C20; case class C20_J0(b: B) extends C20;
  case class C20_A1(i: I) extends C20; case class C20_B1(b: B) extends C20; case class C20_C1(i: I) extends C20; case class C20_D1(b: B) extends C20; case class C20_E1(i: I) extends C20; case class C20_F1(b: B) extends C20; case class C20_G1(i: I) extends C20; case class C20_H1(b: B) extends C20; case class C20_I1(i: I) extends C20; case class C20_J1(b: B) extends C20

sealed trait C30; object C30
  case class C30_A0(i: I) extends C30; case class C30_B0(b: B) extends C30; case class C30_C0(i: I) extends C30; case class C30_D0(b: B) extends C30; case class C30_E0(i: I) extends C30; case class C30_F0(b: B) extends C30; case class C30_G0(i: I) extends C30; case class C30_H0(b: B) extends C30; case class C30_I0(i: I) extends C30; case class C30_J0(b: B) extends C30;
  case class C30_A1(i: I) extends C30; case class C30_B1(b: B) extends C30; case class C30_C1(i: I) extends C30; case class C30_D1(b: B) extends C30; case class C30_E1(i: I) extends C30; case class C30_F1(b: B) extends C30; case class C30_G1(i: I) extends C30; case class C30_H1(b: B) extends C30; case class C30_I1(i: I) extends C30; case class C30_J1(b: B) extends C30;
  case class C30_A2(i: I) extends C30; case class C30_B2(b: B) extends C30; case class C30_C2(i: I) extends C30; case class C30_D2(b: B) extends C30; case class C30_E2(i: I) extends C30; case class C30_F2(b: B) extends C30; case class C30_G2(i: I) extends C30; case class C30_H2(b: B) extends C30; case class C30_I2(i: I) extends C30; case class C30_J2(b: B) extends C30

sealed trait C40; object C40
  case class C40_A0(i: I) extends C40; case class C40_B0(b: B) extends C40; case class C40_C0(i: I) extends C40; case class C40_D0(b: B) extends C40; case class C40_E0(i: I) extends C40; case class C40_F0(b: B) extends C40; case class C40_G0(i: I) extends C40; case class C40_H0(b: B) extends C40; case class C40_I0(i: I) extends C40; case class C40_J0(b: B) extends C40;
  case class C40_A1(i: I) extends C40; case class C40_B1(b: B) extends C40; case class C40_C1(i: I) extends C40; case class C40_D1(b: B) extends C40; case class C40_E1(i: I) extends C40; case class C40_F1(b: B) extends C40; case class C40_G1(i: I) extends C40; case class C40_H1(b: B) extends C40; case class C40_I1(i: I) extends C40; case class C40_J1(b: B) extends C40;
  case class C40_A2(i: I) extends C40; case class C40_B2(b: B) extends C40; case class C40_C2(i: I) extends C40; case class C40_D2(b: B) extends C40; case class C40_E2(i: I) extends C40; case class C40_F2(b: B) extends C40; case class C40_G2(i: I) extends C40; case class C40_H2(b: B) extends C40; case class C40_I2(i: I) extends C40; case class C40_J2(b: B) extends C40;
  case class C40_A3(i: I) extends C40; case class C40_B3(b: B) extends C40; case class C40_C3(i: I) extends C40; case class C40_D3(b: B) extends C40; case class C40_E3(i: I) extends C40; case class C40_F3(b: B) extends C40; case class C40_G3(i: I) extends C40; case class C40_H3(b: B) extends C40; case class C40_I3(i: I) extends C40; case class C40_J3(b: B) extends C40

sealed trait C50; object C50
  case class C50_A0(i: I) extends C50; case class C50_B0(b: B) extends C50; case class C50_C0(i: I) extends C50; case class C50_D0(b: B) extends C50; case class C50_E0(i: I) extends C50; case class C50_F0(b: B) extends C50; case class C50_G0(i: I) extends C50; case class C50_H0(b: B) extends C50; case class C50_I0(i: I) extends C50; case class C50_J0(b: B) extends C50;
  case class C50_A1(i: I) extends C50; case class C50_B1(b: B) extends C50; case class C50_C1(i: I) extends C50; case class C50_D1(b: B) extends C50; case class C50_E1(i: I) extends C50; case class C50_F1(b: B) extends C50; case class C50_G1(i: I) extends C50; case class C50_H1(b: B) extends C50; case class C50_I1(i: I) extends C50; case class C50_J1(b: B) extends C50;
  case class C50_A2(i: I) extends C50; case class C50_B2(b: B) extends C50; case class C50_C2(i: I) extends C50; case class C50_D2(b: B) extends C50; case class C50_E2(i: I) extends C50; case class C50_F2(b: B) extends C50; case class C50_G2(i: I) extends C50; case class C50_H2(b: B) extends C50; case class C50_I2(i: I) extends C50; case class C50_J2(b: B) extends C50;
  case class C50_A3(i: I) extends C50; case class C50_B3(b: B) extends C50; case class C50_C3(i: I) extends C50; case class C50_D3(b: B) extends C50; case class C50_E3(i: I) extends C50; case class C50_F3(b: B) extends C50; case class C50_G3(i: I) extends C50; case class C50_H3(b: B) extends C50; case class C50_I3(i: I) extends C50; case class C50_J3(b: B) extends C50;
  case class C50_A4(i: I) extends C50; case class C50_B4(b: B) extends C50; case class C50_C4(i: I) extends C50; case class C50_D4(b: B) extends C50; case class C50_E4(i: I) extends C50; case class C50_F4(b: B) extends C50; case class C50_G4(i: I) extends C50; case class C50_H4(b: B) extends C50; case class C50_I4(i: I) extends C50; case class C50_J4(b: B) extends C50

sealed trait C60; object C60
  case class C60_A0(i: I) extends C60; case class C60_B0(b: B) extends C60; case class C60_C0(i: I) extends C60; case class C60_D0(b: B) extends C60; case class C60_E0(i: I) extends C60; case class C60_F0(b: B) extends C60; case class C60_G0(i: I) extends C60; case class C60_H0(b: B) extends C60; case class C60_I0(i: I) extends C60; case class C60_J0(b: B) extends C60;
  case class C60_A1(i: I) extends C60; case class C60_B1(b: B) extends C60; case class C60_C1(i: I) extends C60; case class C60_D1(b: B) extends C60; case class C60_E1(i: I) extends C60; case class C60_F1(b: B) extends C60; case class C60_G1(i: I) extends C60; case class C60_H1(b: B) extends C60; case class C60_I1(i: I) extends C60; case class C60_J1(b: B) extends C60;
  case class C60_A2(i: I) extends C60; case class C60_B2(b: B) extends C60; case class C60_C2(i: I) extends C60; case class C60_D2(b: B) extends C60; case class C60_E2(i: I) extends C60; case class C60_F2(b: B) extends C60; case class C60_G2(i: I) extends C60; case class C60_H2(b: B) extends C60; case class C60_I2(i: I) extends C60; case class C60_J2(b: B) extends C60;
  case class C60_A3(i: I) extends C60; case class C60_B3(b: B) extends C60; case class C60_C3(i: I) extends C60; case class C60_D3(b: B) extends C60; case class C60_E3(i: I) extends C60; case class C60_F3(b: B) extends C60; case class C60_G3(i: I) extends C60; case class C60_H3(b: B) extends C60; case class C60_I3(i: I) extends C60; case class C60_J3(b: B) extends C60;
  case class C60_A4(i: I) extends C60; case class C60_B4(b: B) extends C60; case class C60_C4(i: I) extends C60; case class C60_D4(b: B) extends C60; case class C60_E4(i: I) extends C60; case class C60_F4(b: B) extends C60; case class C60_G4(i: I) extends C60; case class C60_H4(b: B) extends C60; case class C60_I4(i: I) extends C60; case class C60_J4(b: B) extends C60;
  case class C60_A5(i: I) extends C60; case class C60_B5(b: B) extends C60; case class C60_C5(i: I) extends C60; case class C60_D5(b: B) extends C60; case class C60_E5(i: I) extends C60; case class C60_F5(b: B) extends C60; case class C60_G5(i: I) extends C60; case class C60_H5(b: B) extends C60; case class C60_I5(i: I) extends C60; case class C60_J5(b: B) extends C60

sealed trait C70; object C70
  case class C70_A0(i: I) extends C70; case class C70_B0(b: B) extends C70; case class C70_C0(i: I) extends C70; case class C70_D0(b: B) extends C70; case class C70_E0(i: I) extends C70; case class C70_F0(b: B) extends C70; case class C70_G0(i: I) extends C70; case class C70_H0(b: B) extends C70; case class C70_I0(i: I) extends C70; case class C70_J0(b: B) extends C70;
  case class C70_A1(i: I) extends C70; case class C70_B1(b: B) extends C70; case class C70_C1(i: I) extends C70; case class C70_D1(b: B) extends C70; case class C70_E1(i: I) extends C70; case class C70_F1(b: B) extends C70; case class C70_G1(i: I) extends C70; case class C70_H1(b: B) extends C70; case class C70_I1(i: I) extends C70; case class C70_J1(b: B) extends C70;
  case class C70_A2(i: I) extends C70; case class C70_B2(b: B) extends C70; case class C70_C2(i: I) extends C70; case class C70_D2(b: B) extends C70; case class C70_E2(i: I) extends C70; case class C70_F2(b: B) extends C70; case class C70_G2(i: I) extends C70; case class C70_H2(b: B) extends C70; case class C70_I2(i: I) extends C70; case class C70_J2(b: B) extends C70;
  case class C70_A3(i: I) extends C70; case class C70_B3(b: B) extends C70; case class C70_C3(i: I) extends C70; case class C70_D3(b: B) extends C70; case class C70_E3(i: I) extends C70; case class C70_F3(b: B) extends C70; case class C70_G3(i: I) extends C70; case class C70_H3(b: B) extends C70; case class C70_I3(i: I) extends C70; case class C70_J3(b: B) extends C70;
  case class C70_A4(i: I) extends C70; case class C70_B4(b: B) extends C70; case class C70_C4(i: I) extends C70; case class C70_D4(b: B) extends C70; case class C70_E4(i: I) extends C70; case class C70_F4(b: B) extends C70; case class C70_G4(i: I) extends C70; case class C70_H4(b: B) extends C70; case class C70_I4(i: I) extends C70; case class C70_J4(b: B) extends C70;
  case class C70_A5(i: I) extends C70; case class C70_B5(b: B) extends C70; case class C70_C5(i: I) extends C70; case class C70_D5(b: B) extends C70; case class C70_E5(i: I) extends C70; case class C70_F5(b: B) extends C70; case class C70_G5(i: I) extends C70; case class C70_H5(b: B) extends C70; case class C70_I5(i: I) extends C70; case class C70_J5(b: B) extends C70;
  case class C70_A6(i: I) extends C70; case class C70_B6(b: B) extends C70; case class C70_C6(i: I) extends C70; case class C70_D6(b: B) extends C70; case class C70_E6(i: I) extends C70; case class C70_F6(b: B) extends C70; case class C70_G6(i: I) extends C70; case class C70_H6(b: B) extends C70; case class C70_I6(i: I) extends C70; case class C70_J6(b: B) extends C70

sealed trait C80; object C80
  case class C80_A0(i: I) extends C80; case class C80_B0(b: B) extends C80; case class C80_C0(i: I) extends C80; case class C80_D0(b: B) extends C80; case class C80_E0(i: I) extends C80; case class C80_F0(b: B) extends C80; case class C80_G0(i: I) extends C80; case class C80_H0(b: B) extends C80; case class C80_I0(i: I) extends C80; case class C80_J0(b: B) extends C80;
  case class C80_A1(i: I) extends C80; case class C80_B1(b: B) extends C80; case class C80_C1(i: I) extends C80; case class C80_D1(b: B) extends C80; case class C80_E1(i: I) extends C80; case class C80_F1(b: B) extends C80; case class C80_G1(i: I) extends C80; case class C80_H1(b: B) extends C80; case class C80_I1(i: I) extends C80; case class C80_J1(b: B) extends C80;
  case class C80_A2(i: I) extends C80; case class C80_B2(b: B) extends C80; case class C80_C2(i: I) extends C80; case class C80_D2(b: B) extends C80; case class C80_E2(i: I) extends C80; case class C80_F2(b: B) extends C80; case class C80_G2(i: I) extends C80; case class C80_H2(b: B) extends C80; case class C80_I2(i: I) extends C80; case class C80_J2(b: B) extends C80;
  case class C80_A3(i: I) extends C80; case class C80_B3(b: B) extends C80; case class C80_C3(i: I) extends C80; case class C80_D3(b: B) extends C80; case class C80_E3(i: I) extends C80; case class C80_F3(b: B) extends C80; case class C80_G3(i: I) extends C80; case class C80_H3(b: B) extends C80; case class C80_I3(i: I) extends C80; case class C80_J3(b: B) extends C80;
  case class C80_A4(i: I) extends C80; case class C80_B4(b: B) extends C80; case class C80_C4(i: I) extends C80; case class C80_D4(b: B) extends C80; case class C80_E4(i: I) extends C80; case class C80_F4(b: B) extends C80; case class C80_G4(i: I) extends C80; case class C80_H4(b: B) extends C80; case class C80_I4(i: I) extends C80; case class C80_J4(b: B) extends C80;
  case class C80_A5(i: I) extends C80; case class C80_B5(b: B) extends C80; case class C80_C5(i: I) extends C80; case class C80_D5(b: B) extends C80; case class C80_E5(i: I) extends C80; case class C80_F5(b: B) extends C80; case class C80_G5(i: I) extends C80; case class C80_H5(b: B) extends C80; case class C80_I5(i: I) extends C80; case class C80_J5(b: B) extends C80;
  case class C80_A6(i: I) extends C80; case class C80_B6(b: B) extends C80; case class C80_C6(i: I) extends C80; case class C80_D6(b: B) extends C80; case class C80_E6(i: I) extends C80; case class C80_F6(b: B) extends C80; case class C80_G6(i: I) extends C80; case class C80_H6(b: B) extends C80; case class C80_I6(i: I) extends C80; case class C80_J6(b: B) extends C80;
  case class C80_A7(i: I) extends C80; case class C80_B7(b: B) extends C80; case class C80_C7(i: I) extends C80; case class C80_D7(b: B) extends C80; case class C80_E7(i: I) extends C80; case class C80_F7(b: B) extends C80; case class C80_G7(i: I) extends C80; case class C80_H7(b: B) extends C80; case class C80_I7(i: I) extends C80; case class C80_J7(b: B) extends C80

sealed trait C90; object C90
  case class C90_A0(i: I) extends C90; case class C90_B0(b: B) extends C90; case class C90_C0(i: I) extends C90; case class C90_D0(b: B) extends C90; case class C90_E0(i: I) extends C90; case class C90_F0(b: B) extends C90; case class C90_G0(i: I) extends C90; case class C90_H0(b: B) extends C90; case class C90_I0(i: I) extends C90; case class C90_J0(b: B) extends C90;
  case class C90_A1(i: I) extends C90; case class C90_B1(b: B) extends C90; case class C90_C1(i: I) extends C90; case class C90_D1(b: B) extends C90; case class C90_E1(i: I) extends C90; case class C90_F1(b: B) extends C90; case class C90_G1(i: I) extends C90; case class C90_H1(b: B) extends C90; case class C90_I1(i: I) extends C90; case class C90_J1(b: B) extends C90;
  case class C90_A2(i: I) extends C90; case class C90_B2(b: B) extends C90; case class C90_C2(i: I) extends C90; case class C90_D2(b: B) extends C90; case class C90_E2(i: I) extends C90; case class C90_F2(b: B) extends C90; case class C90_G2(i: I) extends C90; case class C90_H2(b: B) extends C90; case class C90_I2(i: I) extends C90; case class C90_J2(b: B) extends C90;
  case class C90_A3(i: I) extends C90; case class C90_B3(b: B) extends C90; case class C90_C3(i: I) extends C90; case class C90_D3(b: B) extends C90; case class C90_E3(i: I) extends C90; case class C90_F3(b: B) extends C90; case class C90_G3(i: I) extends C90; case class C90_H3(b: B) extends C90; case class C90_I3(i: I) extends C90; case class C90_J3(b: B) extends C90;
  case class C90_A4(i: I) extends C90; case class C90_B4(b: B) extends C90; case class C90_C4(i: I) extends C90; case class C90_D4(b: B) extends C90; case class C90_E4(i: I) extends C90; case class C90_F4(b: B) extends C90; case class C90_G4(i: I) extends C90; case class C90_H4(b: B) extends C90; case class C90_I4(i: I) extends C90; case class C90_J4(b: B) extends C90;
  case class C90_A5(i: I) extends C90; case class C90_B5(b: B) extends C90; case class C90_C5(i: I) extends C90; case class C90_D5(b: B) extends C90; case class C90_E5(i: I) extends C90; case class C90_F5(b: B) extends C90; case class C90_G5(i: I) extends C90; case class C90_H5(b: B) extends C90; case class C90_I5(i: I) extends C90; case class C90_J5(b: B) extends C90;
  case class C90_A6(i: I) extends C90; case class C90_B6(b: B) extends C90; case class C90_C6(i: I) extends C90; case class C90_D6(b: B) extends C90; case class C90_E6(i: I) extends C90; case class C90_F6(b: B) extends C90; case class C90_G6(i: I) extends C90; case class C90_H6(b: B) extends C90; case class C90_I6(i: I) extends C90; case class C90_J6(b: B) extends C90;
  case class C90_A7(i: I) extends C90; case class C90_B7(b: B) extends C90; case class C90_C7(i: I) extends C90; case class C90_D7(b: B) extends C90; case class C90_E7(i: I) extends C90; case class C90_F7(b: B) extends C90; case class C90_G7(i: I) extends C90; case class C90_H7(b: B) extends C90; case class C90_I7(i: I) extends C90; case class C90_J7(b: B) extends C90;
  case class C90_A8(i: I) extends C90; case class C90_B8(b: B) extends C90; case class C90_C8(i: I) extends C90; case class C90_D8(b: B) extends C90; case class C90_E8(i: I) extends C90; case class C90_F8(b: B) extends C90; case class C90_G8(i: I) extends C90; case class C90_H8(b: B) extends C90; case class C90_I8(i: I) extends C90; case class C90_J8(b: B) extends C90

sealed trait C100; object C100
  case class C100_A0(i: I) extends C100; case class C100_B0(b: B) extends C100; case class C100_C0(i: I) extends C100; case class C100_D0(b: B) extends C100; case class C100_E0(i: I) extends C100; case class C100_F0(b: B) extends C100; case class C100_G0(i: I) extends C100; case class C100_H0(b: B) extends C100; case class C100_I0(i: I) extends C100; case class C100_J0(b: B) extends C100;
  case class C100_A1(i: I) extends C100; case class C100_B1(b: B) extends C100; case class C100_C1(i: I) extends C100; case class C100_D1(b: B) extends C100; case class C100_E1(i: I) extends C100; case class C100_F1(b: B) extends C100; case class C100_G1(i: I) extends C100; case class C100_H1(b: B) extends C100; case class C100_I1(i: I) extends C100; case class C100_J1(b: B) extends C100;
  case class C100_A2(i: I) extends C100; case class C100_B2(b: B) extends C100; case class C100_C2(i: I) extends C100; case class C100_D2(b: B) extends C100; case class C100_E2(i: I) extends C100; case class C100_F2(b: B) extends C100; case class C100_G2(i: I) extends C100; case class C100_H2(b: B) extends C100; case class C100_I2(i: I) extends C100; case class C100_J2(b: B) extends C100;
  case class C100_A3(i: I) extends C100; case class C100_B3(b: B) extends C100; case class C100_C3(i: I) extends C100; case class C100_D3(b: B) extends C100; case class C100_E3(i: I) extends C100; case class C100_F3(b: B) extends C100; case class C100_G3(i: I) extends C100; case class C100_H3(b: B) extends C100; case class C100_I3(i: I) extends C100; case class C100_J3(b: B) extends C100;
  case class C100_A4(i: I) extends C100; case class C100_B4(b: B) extends C100; case class C100_C4(i: I) extends C100; case class C100_D4(b: B) extends C100; case class C100_E4(i: I) extends C100; case class C100_F4(b: B) extends C100; case class C100_G4(i: I) extends C100; case class C100_H4(b: B) extends C100; case class C100_I4(i: I) extends C100; case class C100_J4(b: B) extends C100;
  case class C100_A5(i: I) extends C100; case class C100_B5(b: B) extends C100; case class C100_C5(i: I) extends C100; case class C100_D5(b: B) extends C100; case class C100_E5(i: I) extends C100; case class C100_F5(b: B) extends C100; case class C100_G5(i: I) extends C100; case class C100_H5(b: B) extends C100; case class C100_I5(i: I) extends C100; case class C100_J5(b: B) extends C100;
  case class C100_A6(i: I) extends C100; case class C100_B6(b: B) extends C100; case class C100_C6(i: I) extends C100; case class C100_D6(b: B) extends C100; case class C100_E6(i: I) extends C100; case class C100_F6(b: B) extends C100; case class C100_G6(i: I) extends C100; case class C100_H6(b: B) extends C100; case class C100_I6(i: I) extends C100; case class C100_J6(b: B) extends C100;
  case class C100_A7(i: I) extends C100; case class C100_B7(b: B) extends C100; case class C100_C7(i: I) extends C100; case class C100_D7(b: B) extends C100; case class C100_E7(i: I) extends C100; case class C100_F7(b: B) extends C100; case class C100_G7(i: I) extends C100; case class C100_H7(b: B) extends C100; case class C100_I7(i: I) extends C100; case class C100_J7(b: B) extends C100;
  case class C100_A8(i: I) extends C100; case class C100_B8(b: B) extends C100; case class C100_C8(i: I) extends C100; case class C100_D8(b: B) extends C100; case class C100_E8(i: I) extends C100; case class C100_F8(b: B) extends C100; case class C100_G8(i: I) extends C100; case class C100_H8(b: B) extends C100; case class C100_I8(i: I) extends C100; case class C100_J8(b: B) extends C100;
  case class C100_A9(i: I) extends C100; case class C100_B9(b: B) extends C100; case class C100_C9(i: I) extends C100; case class C100_D9(b: B) extends C100; case class C100_E9(i: I) extends C100; case class C100_F9(b: B) extends C100; case class C100_G9(i: I) extends C100; case class C100_H9(b: B) extends C100; case class C100_I9(i: I) extends C100; case class C100_J9(b: B) extends C100

case class PK0[A]()
case class PK10[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A
)
case class PK20[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A
)
case class PK30[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A
)
case class PK40[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A
)
case class PK50[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A
)
case class PK60[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A
)
case class PK70[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A
)
case class PK80[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A,
  a7: I, b7: A, c7: I, d7: A, e7: I, f7: A, g7: I, h7: A, i7: I, j7: A
)
case class PK90[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A,
  a7: I, b7: A, c7: I, d7: A, e7: I, f7: A, g7: I, h7: A, i7: I, j7: A,
  a8: I, b8: A, c8: I, d8: A, e8: I, f8: A, g8: I, h8: A, i8: I, j8: A
)
case class PK100[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A,
  a7: I, b7: A, c7: I, d7: A, e7: I, f7: A, g7: I, h7: A, i7: I, j7: A,
  a8: I, b8: A, c8: I, d8: A, e8: I, f8: A, g8: I, h8: A, i8: I, j8: A,
  a9: I, b9: A, c9: I, d9: A, e9: I, f9: A, g9: I, h9: A, i9: I, j9: A
)

sealed trait CK0[+A]
object CK0
sealed trait CK10[+A]
object CK10
  case class CK10_A0[A](a: A) extends CK10[A]; case class CK10_B0(b: B) extends CK10[Nothing]; case class CK10_C0[A](a: A) extends CK10[A]; case class CK10_D0(b: B) extends CK10[Nothing]; case class CK10_E0[A](a: A) extends CK10[A]; case class CK10_F0(b: B) extends CK10[Nothing]; case class CK10_G0[A](a: A) extends CK10[A]; case class CK10_H0(b: B) extends CK10[Nothing]; case class CK10_I0[A](a: A) extends CK10[A]; case class CK10_J0(b: B) extends CK10[Nothing];

sealed trait CK20[+A]
object CK20
  case class CK20_A0[A](a: A) extends CK20[A]; case class CK20_B0(b: B) extends CK20[Nothing]; case class CK20_C0[A](a: A) extends CK20[A]; case class CK20_D0(b: B) extends CK20[Nothing]; case class CK20_E0[A](a: A) extends CK20[A]; case class CK20_F0(b: B) extends CK20[Nothing]; case class CK20_G0[A](a: A) extends CK20[A]; case class CK20_H0(b: B) extends CK20[Nothing]; case class CK20_I0[A](a: A) extends CK20[A]; case class CK20_J0(b: B) extends CK20[Nothing];
  case class CK20_A1[A](a: A) extends CK20[A]; case class CK20_B1(b: B) extends CK20[Nothing]; case class CK20_C1[A](a: A) extends CK20[A]; case class CK20_D1(b: B) extends CK20[Nothing]; case class CK20_E1[A](a: A) extends CK20[A]; case class CK20_F1(b: B) extends CK20[Nothing]; case class CK20_G1[A](a: A) extends CK20[A]; case class CK20_H1(b: B) extends CK20[Nothing]; case class CK20_I1[A](a: A) extends CK20[A]; case class CK20_J1(b: B) extends CK20[Nothing]

sealed trait CK30[+A]
object CK30
  case class CK30_A0[A](a: A) extends CK30[A]; case class CK30_B0(b: B) extends CK30[Nothing]; case class CK30_C0[A](a: A) extends CK30[A]; case class CK30_D0(b: B) extends CK30[Nothing]; case class CK30_E0[A](a: A) extends CK30[A]; case class CK30_F0(b: B) extends CK30[Nothing]; case class CK30_G0[A](a: A) extends CK30[A]; case class CK30_H0(b: B) extends CK30[Nothing]; case class CK30_I0[A](a: A) extends CK30[A]; case class CK30_J0(b: B) extends CK30[Nothing];
  case class CK30_A1[A](a: A) extends CK30[A]; case class CK30_B1(b: B) extends CK30[Nothing]; case class CK30_C1[A](a: A) extends CK30[A]; case class CK30_D1(b: B) extends CK30[Nothing]; case class CK30_E1[A](a: A) extends CK30[A]; case class CK30_F1(b: B) extends CK30[Nothing]; case class CK30_G1[A](a: A) extends CK30[A]; case class CK30_H1(b: B) extends CK30[Nothing]; case class CK30_I1[A](a: A) extends CK30[A]; case class CK30_J1(b: B) extends CK30[Nothing];
  case class CK30_A2[A](a: A) extends CK30[A]; case class CK30_B2(b: B) extends CK30[Nothing]; case class CK30_C2[A](a: A) extends CK30[A]; case class CK30_D2(b: B) extends CK30[Nothing]; case class CK30_E2[A](a: A) extends CK30[A]; case class CK30_F2(b: B) extends CK30[Nothing]; case class CK30_G2[A](a: A) extends CK30[A]; case class CK30_H2(b: B) extends CK30[Nothing]; case class CK30_I2[A](a: A) extends CK30[A]; case class CK30_J2(b: B) extends CK30[Nothing]

sealed trait CK40[+A]
object CK40
  case class CK40_A0[A](a: A) extends CK40[A]; case class CK40_B0(b: B) extends CK40[Nothing]; case class CK40_C0[A](a: A) extends CK40[A]; case class CK40_D0(b: B) extends CK40[Nothing]; case class CK40_E0[A](a: A) extends CK40[A]; case class CK40_F0(b: B) extends CK40[Nothing]; case class CK40_G0[A](a: A) extends CK40[A]; case class CK40_H0(b: B) extends CK40[Nothing]; case class CK40_I0[A](a: A) extends CK40[A]; case class CK40_J0(b: B) extends CK40[Nothing];
  case class CK40_A1[A](a: A) extends CK40[A]; case class CK40_B1(b: B) extends CK40[Nothing]; case class CK40_C1[A](a: A) extends CK40[A]; case class CK40_D1(b: B) extends CK40[Nothing]; case class CK40_E1[A](a: A) extends CK40[A]; case class CK40_F1(b: B) extends CK40[Nothing]; case class CK40_G1[A](a: A) extends CK40[A]; case class CK40_H1(b: B) extends CK40[Nothing]; case class CK40_I1[A](a: A) extends CK40[A]; case class CK40_J1(b: B) extends CK40[Nothing];
  case class CK40_A2[A](a: A) extends CK40[A]; case class CK40_B2(b: B) extends CK40[Nothing]; case class CK40_C2[A](a: A) extends CK40[A]; case class CK40_D2(b: B) extends CK40[Nothing]; case class CK40_E2[A](a: A) extends CK40[A]; case class CK40_F2(b: B) extends CK40[Nothing]; case class CK40_G2[A](a: A) extends CK40[A]; case class CK40_H2(b: B) extends CK40[Nothing]; case class CK40_I2[A](a: A) extends CK40[A]; case class CK40_J2(b: B) extends CK40[Nothing];
  case class CK40_A3[A](a: A) extends CK40[A]; case class CK40_B3(b: B) extends CK40[Nothing]; case class CK40_C3[A](a: A) extends CK40[A]; case class CK40_D3(b: B) extends CK40[Nothing]; case class CK40_E3[A](a: A) extends CK40[A]; case class CK40_F3(b: B) extends CK40[Nothing]; case class CK40_G3[A](a: A) extends CK40[A]; case class CK40_H3(b: B) extends CK40[Nothing]; case class CK40_I3[A](a: A) extends CK40[A]; case class CK40_J3(b: B) extends CK40[Nothing]

sealed trait CK50[+A]
object CK50
  case class CK50_A0[A](a: A) extends CK50[A]; case class CK50_B0(b: B) extends CK50[Nothing]; case class CK50_C0[A](a: A) extends CK50[A]; case class CK50_D0(b: B) extends CK50[Nothing]; case class CK50_E0[A](a: A) extends CK50[A]; case class CK50_F0(b: B) extends CK50[Nothing]; case class CK50_G0[A](a: A) extends CK50[A]; case class CK50_H0(b: B) extends CK50[Nothing]; case class CK50_I0[A](a: A) extends CK50[A]; case class CK50_J0(b: B) extends CK50[Nothing];
  case class CK50_A1[A](a: A) extends CK50[A]; case class CK50_B1(b: B) extends CK50[Nothing]; case class CK50_C1[A](a: A) extends CK50[A]; case class CK50_D1(b: B) extends CK50[Nothing]; case class CK50_E1[A](a: A) extends CK50[A]; case class CK50_F1(b: B) extends CK50[Nothing]; case class CK50_G1[A](a: A) extends CK50[A]; case class CK50_H1(b: B) extends CK50[Nothing]; case class CK50_I1[A](a: A) extends CK50[A]; case class CK50_J1(b: B) extends CK50[Nothing];
  case class CK50_A2[A](a: A) extends CK50[A]; case class CK50_B2(b: B) extends CK50[Nothing]; case class CK50_C2[A](a: A) extends CK50[A]; case class CK50_D2(b: B) extends CK50[Nothing]; case class CK50_E2[A](a: A) extends CK50[A]; case class CK50_F2(b: B) extends CK50[Nothing]; case class CK50_G2[A](a: A) extends CK50[A]; case class CK50_H2(b: B) extends CK50[Nothing]; case class CK50_I2[A](a: A) extends CK50[A]; case class CK50_J2(b: B) extends CK50[Nothing];
  case class CK50_A3[A](a: A) extends CK50[A]; case class CK50_B3(b: B) extends CK50[Nothing]; case class CK50_C3[A](a: A) extends CK50[A]; case class CK50_D3(b: B) extends CK50[Nothing]; case class CK50_E3[A](a: A) extends CK50[A]; case class CK50_F3(b: B) extends CK50[Nothing]; case class CK50_G3[A](a: A) extends CK50[A]; case class CK50_H3(b: B) extends CK50[Nothing]; case class CK50_I3[A](a: A) extends CK50[A]; case class CK50_J3(b: B) extends CK50[Nothing];
  case class CK50_A4[A](a: A) extends CK50[A]; case class CK50_B4(b: B) extends CK50[Nothing]; case class CK50_C4[A](a: A) extends CK50[A]; case class CK50_D4(b: B) extends CK50[Nothing]; case class CK50_E4[A](a: A) extends CK50[A]; case class CK50_F4(b: B) extends CK50[Nothing]; case class CK50_G4[A](a: A) extends CK50[A]; case class CK50_H4(b: B) extends CK50[Nothing]; case class CK50_I4[A](a: A) extends CK50[A]; case class CK50_J4(b: B) extends CK50[Nothing]

sealed trait CK60[+A]
object CK60
  case class CK60_A0[A](a: A) extends CK60[A]; case class CK60_B0(b: B) extends CK60[Nothing]; case class CK60_C0[A](a: A) extends CK60[A]; case class CK60_D0(b: B) extends CK60[Nothing]; case class CK60_E0[A](a: A) extends CK60[A]; case class CK60_F0(b: B) extends CK60[Nothing]; case class CK60_G0[A](a: A) extends CK60[A]; case class CK60_H0(b: B) extends CK60[Nothing]; case class CK60_I0[A](a: A) extends CK60[A]; case class CK60_J0(b: B) extends CK60[Nothing];
  case class CK60_A1[A](a: A) extends CK60[A]; case class CK60_B1(b: B) extends CK60[Nothing]; case class CK60_C1[A](a: A) extends CK60[A]; case class CK60_D1(b: B) extends CK60[Nothing]; case class CK60_E1[A](a: A) extends CK60[A]; case class CK60_F1(b: B) extends CK60[Nothing]; case class CK60_G1[A](a: A) extends CK60[A]; case class CK60_H1(b: B) extends CK60[Nothing]; case class CK60_I1[A](a: A) extends CK60[A]; case class CK60_J1(b: B) extends CK60[Nothing];
  case class CK60_A2[A](a: A) extends CK60[A]; case class CK60_B2(b: B) extends CK60[Nothing]; case class CK60_C2[A](a: A) extends CK60[A]; case class CK60_D2(b: B) extends CK60[Nothing]; case class CK60_E2[A](a: A) extends CK60[A]; case class CK60_F2(b: B) extends CK60[Nothing]; case class CK60_G2[A](a: A) extends CK60[A]; case class CK60_H2(b: B) extends CK60[Nothing]; case class CK60_I2[A](a: A) extends CK60[A]; case class CK60_J2(b: B) extends CK60[Nothing];
  case class CK60_A3[A](a: A) extends CK60[A]; case class CK60_B3(b: B) extends CK60[Nothing]; case class CK60_C3[A](a: A) extends CK60[A]; case class CK60_D3(b: B) extends CK60[Nothing]; case class CK60_E3[A](a: A) extends CK60[A]; case class CK60_F3(b: B) extends CK60[Nothing]; case class CK60_G3[A](a: A) extends CK60[A]; case class CK60_H3(b: B) extends CK60[Nothing]; case class CK60_I3[A](a: A) extends CK60[A]; case class CK60_J3(b: B) extends CK60[Nothing];
  case class CK60_A4[A](a: A) extends CK60[A]; case class CK60_B4(b: B) extends CK60[Nothing]; case class CK60_C4[A](a: A) extends CK60[A]; case class CK60_D4(b: B) extends CK60[Nothing]; case class CK60_E4[A](a: A) extends CK60[A]; case class CK60_F4(b: B) extends CK60[Nothing]; case class CK60_G4[A](a: A) extends CK60[A]; case class CK60_H4(b: B) extends CK60[Nothing]; case class CK60_I4[A](a: A) extends CK60[A]; case class CK60_J4(b: B) extends CK60[Nothing];
  case class CK60_A5[A](a: A) extends CK60[A]; case class CK60_B5(b: B) extends CK60[Nothing]; case class CK60_C5[A](a: A) extends CK60[A]; case class CK60_D5(b: B) extends CK60[Nothing]; case class CK60_E5[A](a: A) extends CK60[A]; case class CK60_F5(b: B) extends CK60[Nothing]; case class CK60_G5[A](a: A) extends CK60[A]; case class CK60_H5(b: B) extends CK60[Nothing]; case class CK60_I5[A](a: A) extends CK60[A]; case class CK60_J5(b: B) extends CK60[Nothing]

sealed trait CK70[+A]
object CK70
  case class CK70_A0[A](a: A) extends CK70[A]; case class CK70_B0(b: B) extends CK70[Nothing]; case class CK70_C0[A](a: A) extends CK70[A]; case class CK70_D0(b: B) extends CK70[Nothing]; case class CK70_E0[A](a: A) extends CK70[A]; case class CK70_F0(b: B) extends CK70[Nothing]; case class CK70_G0[A](a: A) extends CK70[A]; case class CK70_H0(b: B) extends CK70[Nothing]; case class CK70_I0[A](a: A) extends CK70[A]; case class CK70_J0(b: B) extends CK70[Nothing];
  case class CK70_A1[A](a: A) extends CK70[A]; case class CK70_B1(b: B) extends CK70[Nothing]; case class CK70_C1[A](a: A) extends CK70[A]; case class CK70_D1(b: B) extends CK70[Nothing]; case class CK70_E1[A](a: A) extends CK70[A]; case class CK70_F1(b: B) extends CK70[Nothing]; case class CK70_G1[A](a: A) extends CK70[A]; case class CK70_H1(b: B) extends CK70[Nothing]; case class CK70_I1[A](a: A) extends CK70[A]; case class CK70_J1(b: B) extends CK70[Nothing];
  case class CK70_A2[A](a: A) extends CK70[A]; case class CK70_B2(b: B) extends CK70[Nothing]; case class CK70_C2[A](a: A) extends CK70[A]; case class CK70_D2(b: B) extends CK70[Nothing]; case class CK70_E2[A](a: A) extends CK70[A]; case class CK70_F2(b: B) extends CK70[Nothing]; case class CK70_G2[A](a: A) extends CK70[A]; case class CK70_H2(b: B) extends CK70[Nothing]; case class CK70_I2[A](a: A) extends CK70[A]; case class CK70_J2(b: B) extends CK70[Nothing];
  case class CK70_A3[A](a: A) extends CK70[A]; case class CK70_B3(b: B) extends CK70[Nothing]; case class CK70_C3[A](a: A) extends CK70[A]; case class CK70_D3(b: B) extends CK70[Nothing]; case class CK70_E3[A](a: A) extends CK70[A]; case class CK70_F3(b: B) extends CK70[Nothing]; case class CK70_G3[A](a: A) extends CK70[A]; case class CK70_H3(b: B) extends CK70[Nothing]; case class CK70_I3[A](a: A) extends CK70[A]; case class CK70_J3(b: B) extends CK70[Nothing];
  case class CK70_A4[A](a: A) extends CK70[A]; case class CK70_B4(b: B) extends CK70[Nothing]; case class CK70_C4[A](a: A) extends CK70[A]; case class CK70_D4(b: B) extends CK70[Nothing]; case class CK70_E4[A](a: A) extends CK70[A]; case class CK70_F4(b: B) extends CK70[Nothing]; case class CK70_G4[A](a: A) extends CK70[A]; case class CK70_H4(b: B) extends CK70[Nothing]; case class CK70_I4[A](a: A) extends CK70[A]; case class CK70_J4(b: B) extends CK70[Nothing];
  case class CK70_A5[A](a: A) extends CK70[A]; case class CK70_B5(b: B) extends CK70[Nothing]; case class CK70_C5[A](a: A) extends CK70[A]; case class CK70_D5(b: B) extends CK70[Nothing]; case class CK70_E5[A](a: A) extends CK70[A]; case class CK70_F5(b: B) extends CK70[Nothing]; case class CK70_G5[A](a: A) extends CK70[A]; case class CK70_H5(b: B) extends CK70[Nothing]; case class CK70_I5[A](a: A) extends CK70[A]; case class CK70_J5(b: B) extends CK70[Nothing];
  case class CK70_A6[A](a: A) extends CK70[A]; case class CK70_B6(b: B) extends CK70[Nothing]; case class CK70_C6[A](a: A) extends CK70[A]; case class CK70_D6(b: B) extends CK70[Nothing]; case class CK70_E6[A](a: A) extends CK70[A]; case class CK70_F6(b: B) extends CK70[Nothing]; case class CK70_G6[A](a: A) extends CK70[A]; case class CK70_H6(b: B) extends CK70[Nothing]; case class CK70_I6[A](a: A) extends CK70[A]; case class CK70_J6(b: B) extends CK70[Nothing]

sealed trait CK80[+A]
object CK80
  case class CK80_A0[A](a: A) extends CK80[A]; case class CK80_B0(b: B) extends CK80[Nothing]; case class CK80_C0[A](a: A) extends CK80[A]; case class CK80_D0(b: B) extends CK80[Nothing]; case class CK80_E0[A](a: A) extends CK80[A]; case class CK80_F0(b: B) extends CK80[Nothing]; case class CK80_G0[A](a: A) extends CK80[A]; case class CK80_H0(b: B) extends CK80[Nothing]; case class CK80_I0[A](a: A) extends CK80[A]; case class CK80_J0(b: B) extends CK80[Nothing];
  case class CK80_A1[A](a: A) extends CK80[A]; case class CK80_B1(b: B) extends CK80[Nothing]; case class CK80_C1[A](a: A) extends CK80[A]; case class CK80_D1(b: B) extends CK80[Nothing]; case class CK80_E1[A](a: A) extends CK80[A]; case class CK80_F1(b: B) extends CK80[Nothing]; case class CK80_G1[A](a: A) extends CK80[A]; case class CK80_H1(b: B) extends CK80[Nothing]; case class CK80_I1[A](a: A) extends CK80[A]; case class CK80_J1(b: B) extends CK80[Nothing];
  case class CK80_A2[A](a: A) extends CK80[A]; case class CK80_B2(b: B) extends CK80[Nothing]; case class CK80_C2[A](a: A) extends CK80[A]; case class CK80_D2(b: B) extends CK80[Nothing]; case class CK80_E2[A](a: A) extends CK80[A]; case class CK80_F2(b: B) extends CK80[Nothing]; case class CK80_G2[A](a: A) extends CK80[A]; case class CK80_H2(b: B) extends CK80[Nothing]; case class CK80_I2[A](a: A) extends CK80[A]; case class CK80_J2(b: B) extends CK80[Nothing];
  case class CK80_A3[A](a: A) extends CK80[A]; case class CK80_B3(b: B) extends CK80[Nothing]; case class CK80_C3[A](a: A) extends CK80[A]; case class CK80_D3(b: B) extends CK80[Nothing]; case class CK80_E3[A](a: A) extends CK80[A]; case class CK80_F3(b: B) extends CK80[Nothing]; case class CK80_G3[A](a: A) extends CK80[A]; case class CK80_H3(b: B) extends CK80[Nothing]; case class CK80_I3[A](a: A) extends CK80[A]; case class CK80_J3(b: B) extends CK80[Nothing];
  case class CK80_A4[A](a: A) extends CK80[A]; case class CK80_B4(b: B) extends CK80[Nothing]; case class CK80_C4[A](a: A) extends CK80[A]; case class CK80_D4(b: B) extends CK80[Nothing]; case class CK80_E4[A](a: A) extends CK80[A]; case class CK80_F4(b: B) extends CK80[Nothing]; case class CK80_G4[A](a: A) extends CK80[A]; case class CK80_H4(b: B) extends CK80[Nothing]; case class CK80_I4[A](a: A) extends CK80[A]; case class CK80_J4(b: B) extends CK80[Nothing];
  case class CK80_A5[A](a: A) extends CK80[A]; case class CK80_B5(b: B) extends CK80[Nothing]; case class CK80_C5[A](a: A) extends CK80[A]; case class CK80_D5(b: B) extends CK80[Nothing]; case class CK80_E5[A](a: A) extends CK80[A]; case class CK80_F5(b: B) extends CK80[Nothing]; case class CK80_G5[A](a: A) extends CK80[A]; case class CK80_H5(b: B) extends CK80[Nothing]; case class CK80_I5[A](a: A) extends CK80[A]; case class CK80_J5(b: B) extends CK80[Nothing];
  case class CK80_A6[A](a: A) extends CK80[A]; case class CK80_B6(b: B) extends CK80[Nothing]; case class CK80_C6[A](a: A) extends CK80[A]; case class CK80_D6(b: B) extends CK80[Nothing]; case class CK80_E6[A](a: A) extends CK80[A]; case class CK80_F6(b: B) extends CK80[Nothing]; case class CK80_G6[A](a: A) extends CK80[A]; case class CK80_H6(b: B) extends CK80[Nothing]; case class CK80_I6[A](a: A) extends CK80[A]; case class CK80_J6(b: B) extends CK80[Nothing];
  case class CK80_A7[A](a: A) extends CK80[A]; case class CK80_B7(b: B) extends CK80[Nothing]; case class CK80_C7[A](a: A) extends CK80[A]; case class CK80_D7(b: B) extends CK80[Nothing]; case class CK80_E7[A](a: A) extends CK80[A]; case class CK80_F7(b: B) extends CK80[Nothing]; case class CK80_G7[A](a: A) extends CK80[A]; case class CK80_H7(b: B) extends CK80[Nothing]; case class CK80_I7[A](a: A) extends CK80[A]; case class CK80_J7(b: B) extends CK80[Nothing]

sealed trait CK90[+A]
object CK90
  case class CK90_A0[A](a: A) extends CK90[A]; case class CK90_B0(b: B) extends CK90[Nothing]; case class CK90_C0[A](a: A) extends CK90[A]; case class CK90_D0(b: B) extends CK90[Nothing]; case class CK90_E0[A](a: A) extends CK90[A]; case class CK90_F0(b: B) extends CK90[Nothing]; case class CK90_G0[A](a: A) extends CK90[A]; case class CK90_H0(b: B) extends CK90[Nothing]; case class CK90_I0[A](a: A) extends CK90[A]; case class CK90_J0(b: B) extends CK90[Nothing];
  case class CK90_A1[A](a: A) extends CK90[A]; case class CK90_B1(b: B) extends CK90[Nothing]; case class CK90_C1[A](a: A) extends CK90[A]; case class CK90_D1(b: B) extends CK90[Nothing]; case class CK90_E1[A](a: A) extends CK90[A]; case class CK90_F1(b: B) extends CK90[Nothing]; case class CK90_G1[A](a: A) extends CK90[A]; case class CK90_H1(b: B) extends CK90[Nothing]; case class CK90_I1[A](a: A) extends CK90[A]; case class CK90_J1(b: B) extends CK90[Nothing];
  case class CK90_A2[A](a: A) extends CK90[A]; case class CK90_B2(b: B) extends CK90[Nothing]; case class CK90_C2[A](a: A) extends CK90[A]; case class CK90_D2(b: B) extends CK90[Nothing]; case class CK90_E2[A](a: A) extends CK90[A]; case class CK90_F2(b: B) extends CK90[Nothing]; case class CK90_G2[A](a: A) extends CK90[A]; case class CK90_H2(b: B) extends CK90[Nothing]; case class CK90_I2[A](a: A) extends CK90[A]; case class CK90_J2(b: B) extends CK90[Nothing];
  case class CK90_A3[A](a: A) extends CK90[A]; case class CK90_B3(b: B) extends CK90[Nothing]; case class CK90_C3[A](a: A) extends CK90[A]; case class CK90_D3(b: B) extends CK90[Nothing]; case class CK90_E3[A](a: A) extends CK90[A]; case class CK90_F3(b: B) extends CK90[Nothing]; case class CK90_G3[A](a: A) extends CK90[A]; case class CK90_H3(b: B) extends CK90[Nothing]; case class CK90_I3[A](a: A) extends CK90[A]; case class CK90_J3(b: B) extends CK90[Nothing];
  case class CK90_A4[A](a: A) extends CK90[A]; case class CK90_B4(b: B) extends CK90[Nothing]; case class CK90_C4[A](a: A) extends CK90[A]; case class CK90_D4(b: B) extends CK90[Nothing]; case class CK90_E4[A](a: A) extends CK90[A]; case class CK90_F4(b: B) extends CK90[Nothing]; case class CK90_G4[A](a: A) extends CK90[A]; case class CK90_H4(b: B) extends CK90[Nothing]; case class CK90_I4[A](a: A) extends CK90[A]; case class CK90_J4(b: B) extends CK90[Nothing];
  case class CK90_A5[A](a: A) extends CK90[A]; case class CK90_B5(b: B) extends CK90[Nothing]; case class CK90_C5[A](a: A) extends CK90[A]; case class CK90_D5(b: B) extends CK90[Nothing]; case class CK90_E5[A](a: A) extends CK90[A]; case class CK90_F5(b: B) extends CK90[Nothing]; case class CK90_G5[A](a: A) extends CK90[A]; case class CK90_H5(b: B) extends CK90[Nothing]; case class CK90_I5[A](a: A) extends CK90[A]; case class CK90_J5(b: B) extends CK90[Nothing];
  case class CK90_A6[A](a: A) extends CK90[A]; case class CK90_B6(b: B) extends CK90[Nothing]; case class CK90_C6[A](a: A) extends CK90[A]; case class CK90_D6(b: B) extends CK90[Nothing]; case class CK90_E6[A](a: A) extends CK90[A]; case class CK90_F6(b: B) extends CK90[Nothing]; case class CK90_G6[A](a: A) extends CK90[A]; case class CK90_H6(b: B) extends CK90[Nothing]; case class CK90_I6[A](a: A) extends CK90[A]; case class CK90_J6(b: B) extends CK90[Nothing];
  case class CK90_A7[A](a: A) extends CK90[A]; case class CK90_B7(b: B) extends CK90[Nothing]; case class CK90_C7[A](a: A) extends CK90[A]; case class CK90_D7(b: B) extends CK90[Nothing]; case class CK90_E7[A](a: A) extends CK90[A]; case class CK90_F7(b: B) extends CK90[Nothing]; case class CK90_G7[A](a: A) extends CK90[A]; case class CK90_H7(b: B) extends CK90[Nothing]; case class CK90_I7[A](a: A) extends CK90[A]; case class CK90_J7(b: B) extends CK90[Nothing];
  case class CK90_A8[A](a: A) extends CK90[A]; case class CK90_B8(b: B) extends CK90[Nothing]; case class CK90_C8[A](a: A) extends CK90[A]; case class CK90_D8(b: B) extends CK90[Nothing]; case class CK90_E8[A](a: A) extends CK90[A]; case class CK90_F8(b: B) extends CK90[Nothing]; case class CK90_G8[A](a: A) extends CK90[A]; case class CK90_H8(b: B) extends CK90[Nothing]; case class CK90_I8[A](a: A) extends CK90[A]; case class CK90_J8(b: B) extends CK90[Nothing]

sealed trait CK100[+A]
object CK100
  case class CK100_A0[A](a: A) extends CK100[A]; case class CK100_B0(b: B) extends CK100[Nothing]; case class CK100_C0[A](a: A) extends CK100[A]; case class CK100_D0(b: B) extends CK100[Nothing]; case class CK100_E0[A](a: A) extends CK100[A]; case class CK100_F0(b: B) extends CK100[Nothing]; case class CK100_G0[A](a: A) extends CK100[A]; case class CK100_H0(b: B) extends CK100[Nothing]; case class CK100_I0[A](a: A) extends CK100[A]; case class CK100_J0(b: B) extends CK100[Nothing];
  case class CK100_A1[A](a: A) extends CK100[A]; case class CK100_B1(b: B) extends CK100[Nothing]; case class CK100_C1[A](a: A) extends CK100[A]; case class CK100_D1(b: B) extends CK100[Nothing]; case class CK100_E1[A](a: A) extends CK100[A]; case class CK100_F1(b: B) extends CK100[Nothing]; case class CK100_G1[A](a: A) extends CK100[A]; case class CK100_H1(b: B) extends CK100[Nothing]; case class CK100_I1[A](a: A) extends CK100[A]; case class CK100_J1(b: B) extends CK100[Nothing];
  case class CK100_A2[A](a: A) extends CK100[A]; case class CK100_B2(b: B) extends CK100[Nothing]; case class CK100_C2[A](a: A) extends CK100[A]; case class CK100_D2(b: B) extends CK100[Nothing]; case class CK100_E2[A](a: A) extends CK100[A]; case class CK100_F2(b: B) extends CK100[Nothing]; case class CK100_G2[A](a: A) extends CK100[A]; case class CK100_H2(b: B) extends CK100[Nothing]; case class CK100_I2[A](a: A) extends CK100[A]; case class CK100_J2(b: B) extends CK100[Nothing];
  case class CK100_A3[A](a: A) extends CK100[A]; case class CK100_B3(b: B) extends CK100[Nothing]; case class CK100_C3[A](a: A) extends CK100[A]; case class CK100_D3(b: B) extends CK100[Nothing]; case class CK100_E3[A](a: A) extends CK100[A]; case class CK100_F3(b: B) extends CK100[Nothing]; case class CK100_G3[A](a: A) extends CK100[A]; case class CK100_H3(b: B) extends CK100[Nothing]; case class CK100_I3[A](a: A) extends CK100[A]; case class CK100_J3(b: B) extends CK100[Nothing];
  case class CK100_A4[A](a: A) extends CK100[A]; case class CK100_B4(b: B) extends CK100[Nothing]; case class CK100_C4[A](a: A) extends CK100[A]; case class CK100_D4(b: B) extends CK100[Nothing]; case class CK100_E4[A](a: A) extends CK100[A]; case class CK100_F4(b: B) extends CK100[Nothing]; case class CK100_G4[A](a: A) extends CK100[A]; case class CK100_H4(b: B) extends CK100[Nothing]; case class CK100_I4[A](a: A) extends CK100[A]; case class CK100_J4(b: B) extends CK100[Nothing];
  case class CK100_A5[A](a: A) extends CK100[A]; case class CK100_B5(b: B) extends CK100[Nothing]; case class CK100_C5[A](a: A) extends CK100[A]; case class CK100_D5(b: B) extends CK100[Nothing]; case class CK100_E5[A](a: A) extends CK100[A]; case class CK100_F5(b: B) extends CK100[Nothing]; case class CK100_G5[A](a: A) extends CK100[A]; case class CK100_H5(b: B) extends CK100[Nothing]; case class CK100_I5[A](a: A) extends CK100[A]; case class CK100_J5(b: B) extends CK100[Nothing];
  case class CK100_A6[A](a: A) extends CK100[A]; case class CK100_B6(b: B) extends CK100[Nothing]; case class CK100_C6[A](a: A) extends CK100[A]; case class CK100_D6(b: B) extends CK100[Nothing]; case class CK100_E6[A](a: A) extends CK100[A]; case class CK100_F6(b: B) extends CK100[Nothing]; case class CK100_G6[A](a: A) extends CK100[A]; case class CK100_H6(b: B) extends CK100[Nothing]; case class CK100_I6[A](a: A) extends CK100[A]; case class CK100_J6(b: B) extends CK100[Nothing];
  case class CK100_A7[A](a: A) extends CK100[A]; case class CK100_B7(b: B) extends CK100[Nothing]; case class CK100_C7[A](a: A) extends CK100[A]; case class CK100_D7(b: B) extends CK100[Nothing]; case class CK100_E7[A](a: A) extends CK100[A]; case class CK100_F7(b: B) extends CK100[Nothing]; case class CK100_G7[A](a: A) extends CK100[A]; case class CK100_H7(b: B) extends CK100[Nothing]; case class CK100_I7[A](a: A) extends CK100[A]; case class CK100_J7(b: B) extends CK100[Nothing];
  case class CK100_A8[A](a: A) extends CK100[A]; case class CK100_B8(b: B) extends CK100[Nothing]; case class CK100_C8[A](a: A) extends CK100[A]; case class CK100_D8(b: B) extends CK100[Nothing]; case class CK100_E8[A](a: A) extends CK100[A]; case class CK100_F8(b: B) extends CK100[Nothing]; case class CK100_G8[A](a: A) extends CK100[A]; case class CK100_H8(b: B) extends CK100[Nothing]; case class CK100_I8[A](a: A) extends CK100[A]; case class CK100_J8(b: B) extends CK100[Nothing];
  case class CK100_A9[A](a: A) extends CK100[A]; case class CK100_B9(b: B) extends CK100[Nothing]; case class CK100_C9[A](a: A) extends CK100[A]; case class CK100_D9(b: B) extends CK100[Nothing]; case class CK100_E9[A](a: A) extends CK100[A]; case class CK100_F9(b: B) extends CK100[Nothing]; case class CK100_G9[A](a: A) extends CK100[A]; case class CK100_H9(b: B) extends CK100[Nothing]; case class CK100_I9[A](a: A) extends CK100[A]; case class CK100_J9(b: B) extends CK100[Nothing]

