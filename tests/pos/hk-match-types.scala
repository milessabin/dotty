object Test1 {
  type Id[t] = t
  type Const[c] = [t] => c

  class Da
  class Db
  type Dummy = Da & Db
  type Apply[F[_]] = F[Dummy]

  type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_]], T] = T match {
    case Unit => Unit
    case (Apply[a], b) => (F[a], LiftP0[F, b])
    case (Dummy, b) => (F[Id], LiftP0[F, b])
    case (a, b) => (F[Const[a]], LiftP0[F, b])
  }

  trait Functor[F[_]]

  {
    type Repr = [t] => (Set[t], Unit)
    type FR = (Functor[Set], Unit)
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (t, Unit)
    type FR = (Functor[Id], Unit)
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (Set[t], (t, Unit))
    type FR = (Functor[Set], (Functor[Id], Unit))
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (Set[t], (List[t], (t, Unit)))
    type FR = (Functor[Set], (Functor[List], (Functor[Id], Unit)))
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (Int, Unit)
    type FR = (Functor[Const[Int]], Unit)
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  {
    type Repr = [t] => (String, Unit)
    type FR = (Functor[Const[String]], Unit)
    implicitly[LiftP[Functor, Repr] =:= FR]
  }

  type Coproduct[F[_[_]], T[_]] = Coproduct0[F, Apply[T]]

  type Coproduct0[F[_[_]], T] = T match {
    case Unit => Nothing
    case (Apply[a], b) => F[a] | Coproduct0[F, b]
    case (Dummy, b) => F[Id] | Coproduct0[F, b]
    case (a, b) => F[Const[a]] | Coproduct0[F, b]
  }

  {
    type Repr = [t] => (Set[t], Unit)
    type CP = Functor[Set]
    implicitly[Coproduct[Functor, Repr] =:= CP]
  }

  {
    trait CList[t]
    class CCons[t] extends CList[t]
    object CNil extends CList[Nothing]

    type Repr = [t] => (CCons[t], (CNil.type, Unit))
    type CP = Functor[CCons] | Functor[Const[CNil.type]]
    implicitly[Coproduct[Functor, Repr] =:= CP]
  }
}

object Test11 {
  type Id[t] = [f[_]] => f[t]
  type Const[c] = [f[_]] => c

  type Dummy[_]
  type Apply[F[_[_]]] = F[Dummy]

  type LiftP[F[_[_[_]]], T[_[_]]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_[_]]], T] = T match {
    case Unit => Unit
    case (Apply[a], b) => (F[a], LiftP0[F, b])
    case (Dummy[a], b) => (F[Id[a]], LiftP0[F, b])
    case (a, b) => (F[Const[a]], LiftP0[F, b])
  }

  trait Functor[F[_]]
  trait FunctorK[F[_[_]]]

  {
    type Repr = [t[_]] => (t[Int], Unit)
    type FR = (FunctorK[Id[Int]], Unit)
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  {
    type Repr = [t[_]] => (t[Int], (t[String], Unit))
    type FR = (FunctorK[Id[Int]], (FunctorK[Id[String]], Unit))
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  {
    type Repr = [t[_]] => (t[Int], (String, Unit))
    type FR = (FunctorK[Id[Int]], (FunctorK[Const[String]], Unit))
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  {
    type Repr = [t[_]] => (t[Int], (Functor[t], Unit))
    type FR = (FunctorK[Id[Int]], (FunctorK[Functor], Unit))
    implicitly[LiftP[FunctorK, Repr] =:= FR]
  }

  type Coproduct[F[_[_[_]]], T[_[_]]] = Coproduct0[F, Apply[T]]

  type Coproduct0[F[_[_[_]]], T] = T match {
    case Unit => Nothing
    case (Apply[a], b) => F[a] | Coproduct0[F, b]
    case (Dummy[a], b) => F[Id[a]] | Coproduct0[F, b]
    case (a, b) => F[Const[a]] | Coproduct0[F, b]
  }

  {
    type Repr = [t[_]] => (t[Int], Unit)
    type CP = FunctorK[Id[Int]]
    implicitly[Coproduct[FunctorK, Repr] =:= CP]
  }

  {
    trait Order[t[_]]
    class Accepted[t[_]] extends Order[t]
    class Declined[t[_]] extends Order[t]
    object Invalid extends Order[Nothing]

    type Repr = [t[_]] => (Accepted[t], (Declined[t], (Invalid.type, Unit)))
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

  type LiftP[F[_[_, _]], T[_, _]] = LiftP0[F, Apply[T]]

  type LiftP0[F[_[_, _]], T] = T match {
    case Unit => Unit
    case (Apply[a], b) => (F[a], LiftP0[F, b])
    case (Dummy1, b) => (F[Id1], LiftP0[F, b])
    case (Dummy2, b) => (F[Id2], LiftP0[F, b])
    case (a, b) => (F[Const[a]], LiftP0[F, b])
  }

  trait Bifunctor[F[_, _]]

  {
    type Repr = [t, u] => (t, (u, Unit))
    type FR = (Bifunctor[Id1], (Bifunctor[Id2], Unit))
    implicitly[LiftP[Bifunctor, Repr] =:= FR]
  }

  type Coproduct[F[_[_, _]], T[_, _]] = Coproduct0[F, Apply[T]]

  type Coproduct0[F[_[_, _]], T] = T match {
    case Unit => Nothing
    case (Apply[a], b) => F[a] | Coproduct0[F, b]
    case (Dummy1, b) => F[Id1] | Coproduct0[F, b]
    case (Dummy2, b) => F[Id2] | Coproduct0[F, b]
    case (a, b) => F[Const[a]] | Coproduct0[F, b]
  }

  {
    type Repr = [t, u] => (Map[t, u], Unit)
    type CP = Bifunctor[Map]
    implicitly[Coproduct[Bifunctor, Repr] =:= CP]
  }

  {
    trait ListF[T, U]
    trait ConsF[T, U] extends ListF[T, U]
    object NilF extends ListF[Nothing, Nothing]

    type Repr = [t, u] => (ConsF[t, u], (NilF.type, Unit))
    type CP = Bifunctor[ConsF] | Bifunctor[Const[NilF.type]]
    implicitly[Coproduct[Bifunctor, Repr] =:= CP]
  }
}
