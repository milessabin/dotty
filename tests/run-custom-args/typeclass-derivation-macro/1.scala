import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec

type RE[X] = given Reflection => E[X]

// Third party library
object K0 {
  class SummonInstances[F[_], T](val instances: List[Any])
  object SummonInstances {
    implicit def caseNil[F[_]]: SummonInstances[F, Unit] =
      new SummonInstances[F, Unit](Nil)

    implicit def caseCons[F[_], H, T <: Tuple](implicit h: F[H], t: SummonInstances[F, T]): SummonInstances[F, H *: T] =
      new SummonInstances[F, H *: T](h :: t.instances)
  }

  trait StagedProductInstances[F[_], T] {
    def instances: List[Any]
    def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]]
    def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T]

    def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] =
      accessorsE(x).zip(accessorsE(y)).zip(instances).foldLeft(i) {
        case (acc, ((xn, yn), in)) => f(acc, in.asInstanceOf, xn, yn)
      }
  }

  object StagedProductInstances {
    implicit def apply[F[_], T]
      (implicit
        m: Mirror.ProductOf[T],
        t: Type[T],
        s: SummonInstances[F, m.MirroredElemTypes]
      ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
        def instances: List[Any] = s.instances

        def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]] = {
          import r._
          t.unseal.symbol match {
            case IsClassDefSymbol(self) => // case class
              self.caseFields.map { field =>
                Select.unique(value.unseal, field.name).seal
              }
            case _ => Nil // case object
          }
        }

        def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T] = {
          import r._
          val companion = t.unseal.tpe match {
            case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
            case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
          }
          Select.overloaded(Ident(companion), "apply", Nil, fields.map(_.unseal)).seal.cast[T]
        }
      }
  }

  trait StagedCoproductInstances[F[_], T] {
    def instances: List[Any]
    def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]]
    def castsE(value: E[T])(implicit r: Reflection): List[E[Any]]

    def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): RE[R] =
      typetestsE(x).zip(castsE(x)).zip(typetestsE(y).zip(castsE(y))).zip(instances).foldLeft(i) {
        case (acc, (((tx, cx), (ty, cy)), in)) =>
          '{ if ($tx && $ty) ${ f(in.asInstanceOf, cx, cy) } else $acc }
      }
  }

  object StagedCoproductInstances {
    implicit def apply[F[_], T]
      (implicit
        m: Mirror.SumOf[T],
        t: Type[T],
        s: SummonInstances[F, m.MirroredElemTypes],
        x: SummonInstances[Type, m.MirroredElemTypes],
      ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
        def instances: List[Any] = s.instances

        def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]] =
          x.instances.map { case tpe: Type[_] => '{ $value.isInstanceOf[$tpe] } }

        def castsE(value: E[T])(implicit r: Reflection): List[E[Any]] =
          x.instances.map { case tpe: Type[_] => '{ $value.asInstanceOf[$tpe] } }
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

  def eqStagedImpl[T](f1: E[T], f2: E[T])(e: Eq0[T])(implicit r: Reflection): E[Boolean] =
    e.eqv(f1, f2)
}

trait Eq0[A] {
  def eqv(x: E[A], y: E[A]): RE[Boolean]
}

object Eq0 {
  import K0._

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

  implicit def eqGenP[A](implicit inst: StagedProductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): RE[Boolean] =
        inst.foldLeft2E(x, y)('{ true })(
          [t] => (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
            '{ $acc && ${ eqt.eqv(t0, t1) }}
        )
    }

  implicit def eqGenC[A](implicit inst: StagedCoproductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): RE[Boolean] = inst.fold2E(x, y)('{ false })(
        [t] => (eqt: Eq0[t], t0: E[t], t1: E[t]) => eqt.eqv(t0, t1)
      )
    }
}

// ADTs

case class ISB(i: Int, s: String, b: Boolean) // derives Eq

sealed trait OptionInt // derives Eq
case class SomeInt(value: Int) extends OptionInt
case object NoneInt extends OptionInt

object OptionInt
