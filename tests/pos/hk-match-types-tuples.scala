import scala.annotation.tailrec
import scala.compiletime._
import scala.runtime.DynamicTuple

object Test0 {
  type ToPairs[T] = T match {
    case Unit => Unit
    case a *: b => (a, ToPairs[b])
  }

  {
    implicitly[ToPairs[Unit] =:= Unit]
    implicitly[ToPairs[Tuple1[Int]] =:= (Int, Unit)]
    implicitly[ToPairs[(Int, String)] =:= (Int, (String, Unit))]
    implicitly[ToPairs[(Int, String, Boolean)] =:= (Int, (String, (Boolean, Unit)))]
  }

  def toPairs[T](t: T): ToPairs[T] = {
    val p = t.asInstanceOf[Product]
    @tailrec
    def loop(i: Int, acc: Tuple): Tuple =
      if(i < 0) acc
      else loop(i-1, (p.productElement(i), acc))
    loop(p.productArity-1, ()).asInstanceOf
  }

  type FromPairs[T] <: Tuple = T match {
    case Unit => Unit
    case (a, b) => a *: FromPairs[b]
  }

  {
    implicitly[FromPairs[Unit] =:= Unit]
    implicitly[FromPairs[(Int, Unit)] =:= Tuple1[Int]]
    implicitly[FromPairs[(Int, (String, Unit))] =:= (Int, String)]
    implicitly[FromPairs[(Int, (String, (Boolean, Unit)))] =:= (Int, String, Boolean)]
  }

  def fromPairs[T <: Tuple](t: T): FromPairs[T] = {
    val arity = size(t)
    val arr = new Array[Object](arity)
    @tailrec
    def loop(t: Tuple, i: Int): Unit = t match {
      case () =>
      case (hd, tl) =>
        arr(i) = hd.asInstanceOf[Object]
        loop(tl.asInstanceOf[Tuple], i+1)
    }
    loop(t.asInstanceOf[Tuple], 0)
    DynamicTuple.dynamicFromArray(arr).asInstanceOf
  }

  def size(t: Tuple): Int = {
    @tailrec
    def loop(t: Tuple, acc: Int): Int = t match {
      case () => acc
      case (_, tl) => loop(tl.asInstanceOf[Tuple], acc+1)
    }
    loop(t, 0)
  }

  type LiftP[F[_], T] <: Tuple = T match {
    case Unit => Unit
    case a *: b => F[a] *: LiftP[F, b]
  }

  trait Monoid[T]

  {
    implicitly[LiftP[Monoid, Unit] =:= Unit]
    implicitly[LiftP[Monoid, Tuple1[Int]] =:= Tuple1[Monoid[Int]]]
    implicitly[LiftP[Monoid, (Int, String)] =:= (Monoid[Int], Monoid[String])]
    implicitly[LiftP[Monoid, (Int, String, Boolean)] =:= (Monoid[Int], Monoid[String], Monoid[Boolean])]
  }

  {
    type Repr = (Int, String, Boolean)
    type FR = (Monoid[Int], Monoid[String], Monoid[Boolean])
    implicitly[LiftP[Monoid, (Int, String, Boolean)] =:= FR]
  }

  type ToUnion[T] = T match {
    case Unit => Nothing
    case a *: b => a | ToUnion[b]
  }

  {
    implicitly[ToUnion[Unit] =:= Nothing]
    implicitly[ToUnion[Tuple1[Int]] =:= Int]
    implicitly[ToUnion[(Int, String)] =:= (Int | String)]
    implicitly[ToUnion[(Int, String, Boolean)] =:= (Int | String | Boolean)]

    class Foo
    class Bar extends Foo
    class Baz extends Foo

    //implicitly[ToUnion[(Bar, Baz)] =:= Foo] // should fail
    implicitly[ToUnion[(Bar, Baz)] =:= (Bar | Baz)]
  }

  type Zip[T, U] <: Tuple = (T, U) match {
    case (Unit, Unit) => Unit
    case ((t *: ts), (u *: us)) => (t, u) *: Zip[ts, us]
  }

  {
    implicitly[Zip[Unit, Unit] =:= Unit]
    implicitly[Zip[Tuple1["i"], Tuple1[Int]] =:= Tuple1[("i", Int)]]
    implicitly[Zip[("i", "s", "b"), (Int, String, Boolean)] =:= (("i", Int), ("s", String), ("b", Boolean))]
  }

  {
    val isb = (1, "foo", 3)
    val i = isb(0)
    val s = isb(1)
    val b = isb(2)
  }

  type Eql[A, B] = A match {
    case B => true
    case _ => false
  }

  {
    val e0: Eql[Int, Int] = true
    val e1: Eql[Int, Double] = false
  }

  type IndexOf[E, X] = IndexOf0[E, X, 0]

  type Id[t] = t
  def narrow(x: Any): Id[x.type] = x
  val m1 = narrow(-1)
  type M1 = m1.type

  type IndexOf0[E, X, I <: Int] <: Int = X match {
    case Unit => M1
    case x *: xs => x match {
      case E => I
      case _ => IndexOf0[E, xs, S[I]]
    }
  }

  type Ks = ("i", "s", "b")
  type Vs = (Int, String, Boolean)

  {
    type T0 = IndexOf["i", Ks]
    val v0: T0 = 0

    type T1 = IndexOf["s", Ks]
    val v1: T1 = 1

    type T2 = IndexOf["b", Ks]
    val v2: T2 = 2

    val v0a = constValue[IndexOf["i", Ks]]
    val v1a = constValue[IndexOf["s", Ks]]
    val v2a = constValue[IndexOf["b", Ks]]
  }

  type ElemAt[K, KS, VS <: Tuple] = Tuple.Elem[VS, IndexOf[K, KS]]

  {
    implicitly[ElemAt["i", Ks, Vs] =:= Int]
    implicitly[ElemAt["s", Ks, Vs] =:= String]
    implicitly[ElemAt["b", Ks, Vs] =:= Boolean]
  }

  inline def select[K, Ks, Vs <: NonEmptyTuple](vs: Vs): ElemAt[K, Ks, Vs] =
    vs(constValue[IndexOf[K, Ks]]).asInstanceOf

  {
    val t = (23, "foo", true)
    val v0 = select[K = "i", Ks = Ks](t)
    val v1 = select[K = "s", Ks = Ks](t)
    val v2 = select[K = "b", Ks = Ks](t)
  }

  type ElemAtOrElse[K, KS, VS <: Tuple, O] = IndexOf[K, KS] match {
    case 0 => Tuple.Elem[VS, 0]
    case S[i] => Tuple.Elem[VS, S[i]]
    case M1 => O
  }

  {
    implicitly[ElemAtOrElse["i", Ks, Vs, Double] =:= Int]
    implicitly[ElemAtOrElse["s", Ks, Vs, Double] =:= String]
    implicitly[ElemAtOrElse["b", Ks, Vs, Double] =:= Boolean]
    implicitly[ElemAtOrElse["d", Ks, Vs, Double] =:= Double]
  }

  inline def selectOrElse[K, Ks, Vs <: NonEmptyTuple, O](vs: Vs, o: => O): ElemAtOrElse[K, Ks, Vs, O] =
    inline constValue[IndexOf[K, Ks]] match {
      case -1 => o.asInstanceOf
      case i => vs(i).asInstanceOf
    }

  {
    val t = (23, "foo", true)
    val v0 = selectOrElse[K = "i", Ks = Ks](t, 1.0)
    val v1 = selectOrElse[K = "s", Ks = Ks](t, 1.0)
    val v2 = selectOrElse[K = "b", Ks = Ks](t, 1.0)
    val v3 = selectOrElse[K = "d", Ks = Ks](t, 1.0)
  }
}

/*
object Test1 {
  type Id[t] = t
  type Const[c] = [t] => c

  class Da
  class Db
  type Dummy = Da & Db
  type Apply[F[_]] = F[Dummy]

  type LiftP[F[_[_]], T[_]] = LiftP0[F, Test0.ToPairs[Apply[T]]]

  type LiftP0[F[_[_]], T] <: Tuple = T match {
    case Unit => Unit
    case (Apply[a],  b) => F[a] *: LiftP0[F, b]
    case (Dummy, b) => F[Id] *: LiftP0[F, b]
    case (a, b) => F[Const[a]] *: LiftP0[F, b]
  }

  trait Functor[F[_]]

  {
    type Repr = [t] => Tuple1[Set[t]]
    type FR = Tuple1[Functor[Set]]
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (Set[t], List[t])
    type FR = (Functor[Set], Functor[List])
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => Tuple1[t]
    type FR = Tuple1[Functor[Id]]
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (Set[t], t)
    type FR = (Functor[Set], Functor[Id])
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (Set[t], List[t], t)
    type FR = (Functor[Set], Functor[List], Functor[Id])
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => Tuple1[Int]
    type FR = Tuple1[Functor[Const[Int]]]
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => Tuple1[String]
    type FR = Tuple1[Functor[Const[String]]]
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  type Coproduct[F[_[_]], T[_]] = Coproduct0[F, Test0.ToPairs[Apply[T]]]

  type Coproduct0[F[_[_]], T] = T match {
    case Unit => Nothing
    case (Apply[a], b) => F[a] | Coproduct0[F, b]
    case (Dummy, b) => F[Id] | Coproduct0[F, b]
    case (a, b) => F[Const[a]] | Coproduct0[F, b]
  }

  {
    type Repr = [t] => Tuple1[Set[t]]
    type CP = Functor[Set]
    implicitly[Coproduct[Functor, Repr] =:= CP]
  }

  {
    // Covariance force explicit Const wrapping in Repr
    trait CList[+t]
    class CCons[+t] extends CList[t]
    object CNil extends CList[Nothing]

    type Repr = [t] => (CCons[t], Const[CNil.type][t])
    type CP = Functor[CCons] | Functor[Const[CNil.type]]
    implicitly[Coproduct[Functor, Repr] =:= CP]
  }

  {
    trait CList[t]
    class CCons[t] extends CList[t]
    object CNil extends CList[Nothing]

    type Repr = [t] => (CCons[t], CNil.type)
    type CP = Functor[CCons] | Functor[Const[CNil.type]]
    implicitly[Coproduct[Functor, Repr] =:= CP]
  }

  /*
  class Unique1[T[_], F[_[_]], G[_[_]]]
  object Unique1 {
    inline implicit def apply[T[_], F[_[_]], G[_[_]]]: Unique1[T, F, G] =
      unique[Apply[T], F, G, Unique1[T, F, G]](null)

    inline def unique[T, F[_[_]], G[_[_]], R](r: R): R = inline erasedValue[T] match {
      case _: (Apply[a], b) => contU[a, b, F, G, R](r)
      case _: (Dummy, b) => contU[Id, b, F, G, R](r)
      case _: (a, b) => contU[Const[a], b, F, G, R](r)
    }

    inline def contU[H[_], T, F[_[_]], G[_[_]], R](r: R): R =
      implicit match {
        case fa: F[H] => all[T, G, R](r)
        case ga: G[H] => unique[T, F, G, R](r)
      }

    inline def all[T, F[_[_]], R](r: R): R = inline erasedValue[T] match {
      case _: Unit => r
      case _: (Apply[a], b) => contA[a, b, F, R](r)
      case _: (Dummy, b) => contA[Id, b, F, R](r)
      case _: (a, b) => contA[Const[a], b, F, R](r)
    }

    inline def contA[H[_], T, F[_[_]], R](r: R): R =
      implicit match {
        case fa: F[H] => all[T, F, R](r)
      }
  }

  trait F[T[_]]
  trait G[T[_]]
  class A[T]
  class B[T]
  implicit val fa: F[A] = null
  implicit val gb: F[B] = null

  {
    type f = [t] => (A[t], Unit)
    implicitly[Unique1[f, F, G]]
  }
  */
}

object Test11 {
  type Id[t] = [f[_]] => f[t]
  type Const[c] = [f[_]] => c

  type Dummy[_]
  type Apply[F[_[_]]] = F[Dummy]

  type LiftP[F[_[_[_]]], T[_[_]]] = LiftP0[F, Test0.ToPairs[Apply[T]]]

  type LiftP0[F[_[_[_]]], T] <: Tuple = T match {
    case Unit => Unit
    case (Apply[a], b) => F[a] *: LiftP0[F, b]
    case (Dummy[a], b) => F[Id[a]] *: LiftP0[F, b]
    case (a, b) => F[Const[a]] *: LiftP0[F, b]
  }

  trait Functor[F[_]]
  trait FunctorK[F[_[_]]]

  {
    type Repr = [t[_]] => Tuple1[t[Int]]
    type FR = Tuple1[FunctorK[Id[Int]]]
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  {
    type Repr = [t[_]] => (t[Int], t[String])
    type FR = (FunctorK[Id[Int]], FunctorK[Id[String]])
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  {
    type Repr = [t[_]] => (t[Int], String)
    type FR = (FunctorK[Id[Int]], FunctorK[Const[String]])
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  {
    type Repr = [t[_]] => (t[Int], Functor[t])
    type FR = (FunctorK[Id[Int]], FunctorK[Functor])
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  type Coproduct[F[_[_[_]]], T[_[_]]] = Coproduct0[F, Test0.ToPairs[Apply[T]]]

  type Coproduct0[F[_[_[_]]], T] = T match {
    case Unit => Nothing
    case (Apply[a], b) => F[a] | Coproduct0[F, b]
    case (Dummy[a], b) => F[Id[a]] | Coproduct0[F, b]
    case (a, b) => F[Const[a]] | Coproduct0[F, b]
  }

  {
    type Repr = [t[_]] => Tuple1[t[Int]]
    type CP = FunctorK[Id[Int]]
    implicitly[Coproduct[FunctorK, Repr] =:= CP]
  }

  {
    trait Order[t[_]]
    class Accepted[t[_]] extends Order[t]
    class Declined[t[_]] extends Order[t]
    object Invalid extends Order[Nothing]

    type Repr = [t[_]] => (Accepted[t], Declined[t], Invalid.type)
    type CP = FunctorK[Accepted] | FunctorK[Declined] | FunctorK[Const[Invalid.type]]
    //implicitly[Coproduct[FunctorK, Repr] =:= CP] // probably works with a bigger stack
    val v0: Coproduct[FunctorK, Repr] = null.asInstanceOf[FunctorK[Accepted]]
    val v1: Coproduct[FunctorK, Repr] = null.asInstanceOf[FunctorK[Declined]]
    val v2: Coproduct[FunctorK, Repr] = null.asInstanceOf[FunctorK[Const[Invalid.type]]]
  }
}

object Test2 {
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

  type LiftP[F[_[_, _]], T[_, _]] = LiftP0[F, Test0.ToPairs[Apply[T]]]

  type LiftP0[F[_[_, _]], T] <: Tuple = T match {
    case Unit => Unit
    case (Apply[a], b) => F[a] *: LiftP0[F, b]
    case (Dummy1, b) => F[Id1] *: LiftP0[F, b]
    case (Dummy2, b) => F[Id2] *: LiftP0[F, b]
    case (a, b) => F[Const[a]] *: LiftP0[F, b]
  }

  trait Bifunctor[F[_, _]]

  {
    type Repr = [t, u] => (t, u)
    type FR = (Bifunctor[Id1], Bifunctor[Id2])
    implicitly[LiftP[Bifunctor, Repr] =:= FR]
  }

  type Coproduct[F[_[_, _]], T[_, _]] = Coproduct0[F, Test0.ToPairs[Apply[T]]]

  type Coproduct0[F[_[_, _]], T] = T match {
    case Unit => Nothing
    case (Apply[a], b) => F[a] | Coproduct0[F, b]
    case (Dummy1, b) => F[Id1] | Coproduct0[F, b]
    case (Dummy2, b) => F[Id2] | Coproduct0[F, b]
    case (a, b) => F[Const[a]] | Coproduct0[F, b]
  }

  {
    type Repr = [t, u] => Tuple1[Map[t, u]]
    type CP = Bifunctor[Map]
    implicitly[Coproduct[Bifunctor, Repr] =:= CP]
  }

  {
    trait ListF[T, U]
    trait ConsF[T, U] extends ListF[T, U]
    object NilF extends ListF[Nothing, Nothing]

    type Repr = [t, u] => (ConsF[t, u], NilF.type)
    type CP = Bifunctor[ConsF] | Bifunctor[Const[NilF.type]]
    implicitly[Coproduct[Bifunctor, Repr] =:= CP]
  }
}
*/
