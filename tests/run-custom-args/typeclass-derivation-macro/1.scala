import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec
import scala.compiletime._

object Shapeless3 {
  type Id[t] = t
  type Const[c] = [t] =>> c
  case class Wrap[T](t: T)


  inline def summon[T] = implicit match {
    case t: T => t
  }

  inline def summonValues[T] <: Tuple = inline erasedValue[T] match {
    case _: Unit => ()
    case _: (a *: b) => constValue[a] *: summonValues[b]
  }

  // Third party library

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
    type Generic[O] = Mirror { type MirroredType = O ; type MirroredElemTypes }
    type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes }
    type CoproductGeneric[O] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes }

    def Generic[O](implicit gen: Generic[O]): Generic[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
    def ProductGeneric[O](implicit gen: ProductGeneric[O]): ProductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
    def CoproductGeneric[O](implicit gen: CoproductGeneric[O]): CoproductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen

    type Instances[F[_], T] = ErasedInstances[F[T]]
    type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]
    type CoproductInstances[F[_], T] = ErasedCoproductInstances[F[T]]

    def Instances[F[_], T](implicit inst: Instances[F, T]): inst.type = inst
    def ProductInstances[F[_], T](implicit inst: ProductInstances[F, T]): inst.type = inst
    def CoproductInstances[F[_], T](implicit inst: CoproductInstances[F, T]): inst.type = inst

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

    inline def summonAsArray[F[_], T]: Array[Any] = inline erasedValue[T] match {
      case _: Unit => Array()
      case _: Tuple1[a] => Array(summon[F[a]])
      case _: (a, b) => Array(summon[F[a]], summon[F[b]])
      case _: (a, b, c) => Array(summon[F[a]], summon[F[b]], summon[F[c]])
      case _: (a, b, c, d) => Array(summon[F[a]], summon[F[b]], summon[F[c]], summon[F[d]])
      case _: (a, b, c, d, e) => Array(summon[F[a]], summon[F[b]], summon[F[c]], summon[F[d]], summon[F[e]])
      // (0 to 20 by 10) foreach { i => print("case _: ("); print((1 to i).map("v".+).mkString(", ")); print(") => Array("); print((1 to i).map(x => s"summon[F[v$x]]").mkString(", ")); println(")") }
      // (30 to 100 by 10) foreach { i => print("case _: ("); print((1 to i).map("v".+).mkString(" *: ")); print(" *: Unit) => Array[Any]("); print((1 to i).map(x => s"summon[F[v$x]]").mkString(", ")); println(")") }
      case _: (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => Array(summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]])
      case _: (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) => Array(summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]], summon[F[v41]], summon[F[v42]], summon[F[v43]], summon[F[v44]], summon[F[v45]], summon[F[v46]], summon[F[v47]], summon[F[v48]], summon[F[v49]], summon[F[v50]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]], summon[F[v41]], summon[F[v42]], summon[F[v43]], summon[F[v44]], summon[F[v45]], summon[F[v46]], summon[F[v47]], summon[F[v48]], summon[F[v49]], summon[F[v50]], summon[F[v51]], summon[F[v52]], summon[F[v53]], summon[F[v54]], summon[F[v55]], summon[F[v56]], summon[F[v57]], summon[F[v58]], summon[F[v59]], summon[F[v60]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]], summon[F[v41]], summon[F[v42]], summon[F[v43]], summon[F[v44]], summon[F[v45]], summon[F[v46]], summon[F[v47]], summon[F[v48]], summon[F[v49]], summon[F[v50]], summon[F[v51]], summon[F[v52]], summon[F[v53]], summon[F[v54]], summon[F[v55]], summon[F[v56]], summon[F[v57]], summon[F[v58]], summon[F[v59]], summon[F[v60]], summon[F[v61]], summon[F[v62]], summon[F[v63]], summon[F[v64]], summon[F[v65]], summon[F[v66]], summon[F[v67]], summon[F[v68]], summon[F[v69]], summon[F[v70]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: v71 *: v72 *: v73 *: v74 *: v75 *: v76 *: v77 *: v78 *: v79 *: v80 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]], summon[F[v41]], summon[F[v42]], summon[F[v43]], summon[F[v44]], summon[F[v45]], summon[F[v46]], summon[F[v47]], summon[F[v48]], summon[F[v49]], summon[F[v50]], summon[F[v51]], summon[F[v52]], summon[F[v53]], summon[F[v54]], summon[F[v55]], summon[F[v56]], summon[F[v57]], summon[F[v58]], summon[F[v59]], summon[F[v60]], summon[F[v61]], summon[F[v62]], summon[F[v63]], summon[F[v64]], summon[F[v65]], summon[F[v66]], summon[F[v67]], summon[F[v68]], summon[F[v69]], summon[F[v70]], summon[F[v71]], summon[F[v72]], summon[F[v73]], summon[F[v74]], summon[F[v75]], summon[F[v76]], summon[F[v77]], summon[F[v78]], summon[F[v79]], summon[F[v80]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: v71 *: v72 *: v73 *: v74 *: v75 *: v76 *: v77 *: v78 *: v79 *: v80 *: v81 *: v82 *: v83 *: v84 *: v85 *: v86 *: v87 *: v88 *: v89 *: v90 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]], summon[F[v41]], summon[F[v42]], summon[F[v43]], summon[F[v44]], summon[F[v45]], summon[F[v46]], summon[F[v47]], summon[F[v48]], summon[F[v49]], summon[F[v50]], summon[F[v51]], summon[F[v52]], summon[F[v53]], summon[F[v54]], summon[F[v55]], summon[F[v56]], summon[F[v57]], summon[F[v58]], summon[F[v59]], summon[F[v60]], summon[F[v61]], summon[F[v62]], summon[F[v63]], summon[F[v64]], summon[F[v65]], summon[F[v66]], summon[F[v67]], summon[F[v68]], summon[F[v69]], summon[F[v70]], summon[F[v71]], summon[F[v72]], summon[F[v73]], summon[F[v74]], summon[F[v75]], summon[F[v76]], summon[F[v77]], summon[F[v78]], summon[F[v79]], summon[F[v80]], summon[F[v81]], summon[F[v82]], summon[F[v83]], summon[F[v84]], summon[F[v85]], summon[F[v86]], summon[F[v87]], summon[F[v88]], summon[F[v89]], summon[F[v90]])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: v71 *: v72 *: v73 *: v74 *: v75 *: v76 *: v77 *: v78 *: v79 *: v80 *: v81 *: v82 *: v83 *: v84 *: v85 *: v86 *: v87 *: v88 *: v89 *: v90 *: v91 *: v92 *: v93 *: v94 *: v95 *: v96 *: v97 *: v98 *: v99 *: v100 *: Unit) => Array[Any](summon[F[v1]], summon[F[v2]], summon[F[v3]], summon[F[v4]], summon[F[v5]], summon[F[v6]], summon[F[v7]], summon[F[v8]], summon[F[v9]], summon[F[v10]], summon[F[v11]], summon[F[v12]], summon[F[v13]], summon[F[v14]], summon[F[v15]], summon[F[v16]], summon[F[v17]], summon[F[v18]], summon[F[v19]], summon[F[v20]], summon[F[v21]], summon[F[v22]], summon[F[v23]], summon[F[v24]], summon[F[v25]], summon[F[v26]], summon[F[v27]], summon[F[v28]], summon[F[v29]], summon[F[v30]], summon[F[v31]], summon[F[v32]], summon[F[v33]], summon[F[v34]], summon[F[v35]], summon[F[v36]], summon[F[v37]], summon[F[v38]], summon[F[v39]], summon[F[v40]], summon[F[v41]], summon[F[v42]], summon[F[v43]], summon[F[v44]], summon[F[v45]], summon[F[v46]], summon[F[v47]], summon[F[v48]], summon[F[v49]], summon[F[v50]], summon[F[v51]], summon[F[v52]], summon[F[v53]], summon[F[v54]], summon[F[v55]], summon[F[v56]], summon[F[v57]], summon[F[v58]], summon[F[v59]], summon[F[v60]], summon[F[v61]], summon[F[v62]], summon[F[v63]], summon[F[v64]], summon[F[v65]], summon[F[v66]], summon[F[v67]], summon[F[v68]], summon[F[v69]], summon[F[v70]], summon[F[v71]], summon[F[v72]], summon[F[v73]], summon[F[v74]], summon[F[v75]], summon[F[v76]], summon[F[v77]], summon[F[v78]], summon[F[v79]], summon[F[v80]], summon[F[v81]], summon[F[v82]], summon[F[v83]], summon[F[v84]], summon[F[v85]], summon[F[v86]], summon[F[v87]], summon[F[v88]], summon[F[v89]], summon[F[v90]], summon[F[v91]], summon[F[v92]], summon[F[v93]], summon[F[v94]], summon[F[v95]], summon[F[v96]], summon[F[v97]], summon[F[v98]], summon[F[v99]], summon[F[v100]])
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

    inline implicit def mkInstances[F[_], T](implicit gen: Generic[T]): ErasedInstances[F[T]] =
      inline gen match {
        case p: ProductGeneric[T]   => mkProductInstances[F, T](p)
        case c: CoproductGeneric[T] => mkCoproductInstances[F, T](c)
      }

    inline implicit def mkProductInstances[F[_], T](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
      new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

    inline implicit def mkCoproductInstances[F[_], T](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
      new ErasedCoproductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

    inline def derive[F[_], T](gen: Generic[T], pg: ProductInstances[F, T] => F[T], cg: CoproductInstances[F, T] => F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => pg(mkProductInstances[F, T](p))
        case c: CoproductGeneric[T] => cg(mkCoproductInstances[F, T](c))
      }
  }

  object K1 {
    type Generic[O[_]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_] }
    type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
    type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

    def Generic[O[_]](implicit gen: Generic[O]): gen.type = gen
    def ProductGeneric[O[_]](implicit gen: ProductGeneric[O]): gen.type = gen
    def CoproductGeneric[O[_]](implicit gen: CoproductGeneric[O]): gen.type = gen

    type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
    type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
    type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

    def Instances[F[_[_]], T[_]](implicit inst: Instances[F, T]): inst.type = inst
    def ProductInstances[F[_[_]], T[_]](implicit inst: ProductInstances[F, T]): inst.type = inst
    def CoproductInstances[F[_[_]], T[_]](implicit inst: CoproductInstances[F, T]): inst.type = inst

    class Dummy
    type Apply[T[_]] = T[Dummy]
    type Unapply[F[_[_]], T] = T match {
      case Wrap[Apply[a]] => F[a]
      case Wrap[Dummy] => F[Id]
      case Wrap[c] => F[Const[c]]
    }

    inline def summon[F[_[_]], T] = implicit match {
      case ft: Unapply[F, Wrap[T]] => ft
    }

    inline def summonAsArray[F[_[_]], T[_]]: Array[Any] = inline erasedValue[Apply[T]] match {
      case _: Unit => Array()
      case _: Tuple1[a] => Array(summon[F, a])
      case _: (a, b) => Array(summon[F, a], summon[F, b])
      case _: (a, b, c) => Array(summon[F, a], summon[F, b], summon[F, c])
      case _: (a, b, c, d) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d])
      case _: (a, b, c, d, e) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d], summon[F, e])
      // (0 to 20 by 10) foreach { i => print("case _: ("); print((1 to i).map("v".+).mkString(", ")); print(") => Array("); print((1 to i).map(x => s"summon[F, v$x]").mkString(", ")); println(")") }
      // (30 to 100 by 10) foreach { i => print("case _: ("); print((1 to i).map("v".+).mkString(" *: ")); print(" *: Unit) => Array[Any]("); print((1 to i).map(x => s"summon[F, v$x]").mkString(", ")); println(")") }
      case _: (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10])
      case _: (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40], summon[F, v41], summon[F, v42], summon[F, v43], summon[F, v44], summon[F, v45], summon[F, v46], summon[F, v47], summon[F, v48], summon[F, v49], summon[F, v50])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40], summon[F, v41], summon[F, v42], summon[F, v43], summon[F, v44], summon[F, v45], summon[F, v46], summon[F, v47], summon[F, v48], summon[F, v49], summon[F, v50], summon[F, v51], summon[F, v52], summon[F, v53], summon[F, v54], summon[F, v55], summon[F, v56], summon[F, v57], summon[F, v58], summon[F, v59], summon[F, v60])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40], summon[F, v41], summon[F, v42], summon[F, v43], summon[F, v44], summon[F, v45], summon[F, v46], summon[F, v47], summon[F, v48], summon[F, v49], summon[F, v50], summon[F, v51], summon[F, v52], summon[F, v53], summon[F, v54], summon[F, v55], summon[F, v56], summon[F, v57], summon[F, v58], summon[F, v59], summon[F, v60], summon[F, v61], summon[F, v62], summon[F, v63], summon[F, v64], summon[F, v65], summon[F, v66], summon[F, v67], summon[F, v68], summon[F, v69], summon[F, v70])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: v71 *: v72 *: v73 *: v74 *: v75 *: v76 *: v77 *: v78 *: v79 *: v80 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40], summon[F, v41], summon[F, v42], summon[F, v43], summon[F, v44], summon[F, v45], summon[F, v46], summon[F, v47], summon[F, v48], summon[F, v49], summon[F, v50], summon[F, v51], summon[F, v52], summon[F, v53], summon[F, v54], summon[F, v55], summon[F, v56], summon[F, v57], summon[F, v58], summon[F, v59], summon[F, v60], summon[F, v61], summon[F, v62], summon[F, v63], summon[F, v64], summon[F, v65], summon[F, v66], summon[F, v67], summon[F, v68], summon[F, v69], summon[F, v70], summon[F, v71], summon[F, v72], summon[F, v73], summon[F, v74], summon[F, v75], summon[F, v76], summon[F, v77], summon[F, v78], summon[F, v79], summon[F, v80])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: v71 *: v72 *: v73 *: v74 *: v75 *: v76 *: v77 *: v78 *: v79 *: v80 *: v81 *: v82 *: v83 *: v84 *: v85 *: v86 *: v87 *: v88 *: v89 *: v90 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40], summon[F, v41], summon[F, v42], summon[F, v43], summon[F, v44], summon[F, v45], summon[F, v46], summon[F, v47], summon[F, v48], summon[F, v49], summon[F, v50], summon[F, v51], summon[F, v52], summon[F, v53], summon[F, v54], summon[F, v55], summon[F, v56], summon[F, v57], summon[F, v58], summon[F, v59], summon[F, v60], summon[F, v61], summon[F, v62], summon[F, v63], summon[F, v64], summon[F, v65], summon[F, v66], summon[F, v67], summon[F, v68], summon[F, v69], summon[F, v70], summon[F, v71], summon[F, v72], summon[F, v73], summon[F, v74], summon[F, v75], summon[F, v76], summon[F, v77], summon[F, v78], summon[F, v79], summon[F, v80], summon[F, v81], summon[F, v82], summon[F, v83], summon[F, v84], summon[F, v85], summon[F, v86], summon[F, v87], summon[F, v88], summon[F, v89], summon[F, v90])
      case _: (v1 *: v2 *: v3 *: v4 *: v5 *: v6 *: v7 *: v8 *: v9 *: v10 *: v11 *: v12 *: v13 *: v14 *: v15 *: v16 *: v17 *: v18 *: v19 *: v20 *: v21 *: v22 *: v23 *: v24 *: v25 *: v26 *: v27 *: v28 *: v29 *: v30 *: v31 *: v32 *: v33 *: v34 *: v35 *: v36 *: v37 *: v38 *: v39 *: v40 *: v41 *: v42 *: v43 *: v44 *: v45 *: v46 *: v47 *: v48 *: v49 *: v50 *: v51 *: v52 *: v53 *: v54 *: v55 *: v56 *: v57 *: v58 *: v59 *: v60 *: v61 *: v62 *: v63 *: v64 *: v65 *: v66 *: v67 *: v68 *: v69 *: v70 *: v71 *: v72 *: v73 *: v74 *: v75 *: v76 *: v77 *: v78 *: v79 *: v80 *: v81 *: v82 *: v83 *: v84 *: v85 *: v86 *: v87 *: v88 *: v89 *: v90 *: v91 *: v92 *: v93 *: v94 *: v95 *: v96 *: v97 *: v98 *: v99 *: v100 *: Unit) => Array[Any](summon[F, v1], summon[F, v2], summon[F, v3], summon[F, v4], summon[F, v5], summon[F, v6], summon[F, v7], summon[F, v8], summon[F, v9], summon[F, v10], summon[F, v11], summon[F, v12], summon[F, v13], summon[F, v14], summon[F, v15], summon[F, v16], summon[F, v17], summon[F, v18], summon[F, v19], summon[F, v20], summon[F, v21], summon[F, v22], summon[F, v23], summon[F, v24], summon[F, v25], summon[F, v26], summon[F, v27], summon[F, v28], summon[F, v29], summon[F, v30], summon[F, v31], summon[F, v32], summon[F, v33], summon[F, v34], summon[F, v35], summon[F, v36], summon[F, v37], summon[F, v38], summon[F, v39], summon[F, v40], summon[F, v41], summon[F, v42], summon[F, v43], summon[F, v44], summon[F, v45], summon[F, v46], summon[F, v47], summon[F, v48], summon[F, v49], summon[F, v50], summon[F, v51], summon[F, v52], summon[F, v53], summon[F, v54], summon[F, v55], summon[F, v56], summon[F, v57], summon[F, v58], summon[F, v59], summon[F, v60], summon[F, v61], summon[F, v62], summon[F, v63], summon[F, v64], summon[F, v65], summon[F, v66], summon[F, v67], summon[F, v68], summon[F, v69], summon[F, v70], summon[F, v71], summon[F, v72], summon[F, v73], summon[F, v74], summon[F, v75], summon[F, v76], summon[F, v77], summon[F, v78], summon[F, v79], summon[F, v80], summon[F, v81], summon[F, v82], summon[F, v83], summon[F, v84], summon[F, v85], summon[F, v86], summon[F, v87], summon[F, v88], summon[F, v89], summon[F, v90], summon[F, v91], summon[F, v92], summon[F, v93], summon[F, v94], summon[F, v95], summon[F, v96], summon[F, v97], summon[F, v98], summon[F, v99], summon[F, v100])
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

    implicit object Ops {
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

    inline implicit def mkInstances[F[_[_]], T[_]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
      inline gen match {
        case p: ProductGeneric[T] => mkProductInstances[F, T](p)
        case c: CoproductGeneric[T] => mkCoproductInstances[F, T](c)
      }

    inline implicit def mkProductInstances[F[_[_]], T[_]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
      new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

    inline implicit def mkCoproductInstances[F[_[_]], T[_]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
      new ErasedCoproductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

    inline def derive[F[_[_]], T[_]](gen: Generic[T], pg: ProductInstances[F, T] => F[T], cg: CoproductInstances[F, T] => F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => pg(mkProductInstances[F, T](p))
        case c: CoproductGeneric[T] => cg(mkCoproductInstances[F, T](c))
      }

    implicit def mkK1_0[O](implicit k0: K0.ProductGeneric[O]): ProductGeneric[Const[O]] { type MirroredElemTypes = Const[k0.MirroredElemTypes] } = k0.asInstanceOf
  }

  // Type class definitions

  trait Eq[A] {
    def eqv(x: A, y: A): Boolean
  }

  object Eq {
    inline def apply[A](implicit ea: Eq[A]): Eq[A] = ea

    implicit val eqUnit: Eq[Unit] = new Eq[Unit] {
      def eqv(x: Unit, y: Unit): Boolean = true
    }
    implicit val eqBoolean: Eq[Boolean] = new Eq[Boolean] {
      def eqv(x: Boolean, y: Boolean): Boolean = x == y
    }
    implicit val eqInt: Eq[Int] = new Eq[Int] {
      def eqv(x: Int, y: Int): Boolean = x == y
    }
    implicit val eqString: Eq[String] = new Eq[String] {
      def eqv(x: String, y: String): Boolean = x == y
    }

    implicit def eqGen[A](implicit inst: => K0.ProductInstances[Eq, A]): Eq[A] =
      new Eq[A] {
        def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
          [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
        )
      }

    implicit def eqGenC[A](implicit inst: => K0.CoproductInstances[Eq, A]): Eq[A] =
      new Eq[A] {
        def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
          [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
        )
      }

    inline def derived[A](implicit gen: K0.Generic[A]): Eq[A] = K0.derive(gen, eqGen, eqGenC)
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    inline def apply[F[_]](implicit ff: Functor[F]): Functor[F] = ff

    implicit val functorId: Functor[Id] = new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

    implicit def functorNested[F[_], G[_]](implicit ff: Functor[F], fg: Functor[G]): Functor[[t] =>> F[G[t]]] =
      new Functor[[t] =>> F[G[t]]] {
        def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = ff.map(fga)(ga => fg.map(ga)(f))
      }

    implicit def functorGen[F[_]](implicit inst: => K1.Instances[Functor, F]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))
      }

    implicit def functorConst[T]: Functor[Const[T]] = new Functor[Const[T]] {
      def map[A, B](t: T)(f: A => B): T = t
    }

    inline def derived[F[_]](implicit gen: K1.Generic[F]): Functor[F] =
      functorGen(K1.mkInstances[Functor, F](gen))
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

object Staged {
  type RE[X] = given Reflection => E[X]

  implicit class ListOps[A](l: List[A]) {
    def safeZip[B](o: List[B]): List[(A, B)] = {
      assert(l.size == o.size)
      l zip o
    }
  }

  // Third party library
  object K0 {
    class SummonInstances[F[_], T](val instances: List[Any])
    object SummonInstances {
      implicit def caseNil[F[_]]: SummonInstances[F, Unit] =
        new SummonInstances[F, Unit](Nil)

      implicit def caseCons[F[_], H, T <: Tuple](implicit h: F[H], t: SummonInstances[F, T]): SummonInstances[F, H *: T] =
        new SummonInstances[F, H *: T](h :: t.instances)
    }

    trait StagedInstances[F[_], T]
    // { def map(x: T)(f: [t] => (F[t], t) => t): T }

    trait StagedProductInstances[F[_], T] extends StagedInstances[F, T] {
      def instances: List[Any]
      def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]]
      def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T]

      def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] =
        accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
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

    trait StagedCoproductInstances[F[_], T] extends StagedInstances[F, T] {
      def instances: List[Any]
      def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]]
      def castsE(value: E[T])(implicit r: Reflection): List[E[Any]]

      def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): RE[R] =
        typetestsE(x).safeZip(castsE(x)).safeZip(typetestsE(y).safeZip(castsE(y))).safeZip(instances).foldLeft(i) {
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

          def castsE(value: E[T])(implicit r: Reflection): List[E[Any]] = {
            x.instances.map { case tpe: Type[_] => '{ $value.asInstanceOf[$tpe] } }
          }
        }
    }
  }

  type Id[t] = t
  type Const[c] = [t] =>> c

  object K1 {
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

    class SummonInstances[F[_[_]], G[_]](val instances: List[Any])
    object SummonInstances {
      implicit def applied[F[_[_]], T[_]](implicit s: SummonInstances0[F, Apply[T]]): SummonInstances[F, T] = {
        // assert(s.instances.nonEmpty)
        new SummonInstances[F, T](s.instances)
      }
    }

    class SummonInstances0[F[_[_]], T](val instances: List[Any])
    object SummonInstances0 {
      implicit def caseNil[F[_[_]]]: SummonInstances0[F, Unit] =
        new SummonInstances0[F, Unit](Nil)

      implicit def caseCons[F[_[_]], H, T <: Tuple]
        (implicit
          h: Unapply[F, Wrap[H]],
          t: SummonInstances0[F, T]
        ): SummonInstances0[F, H *: T] =
          new SummonInstances0[F, H *: T](h :: t.instances)
    }

    trait StagedInstances[F[_[_]], T[_]] {
      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]]
    }

    object StagedInstances {
      implicit def fromStagedProductInstances[F[_[_]], T[_]]
        (implicit spi: StagedProductInstances[F, T]): StagedInstances[F, T] = spi

      implicit def fromStagedCoproductInstances[F[_[_]], T[_]]
        (implicit spi: StagedCoproductInstances[F, T]): StagedInstances[F, T] = spi
    }

    trait StagedProductInstances[F[_[_]], T[_]] extends StagedInstances[F, T] {
      def instances: List[Any]
      def accessorsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]]
      def constructorE[A](fields: List[E[Any]])(implicit r: Reflection): E[T[A]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] = {
        val args = instances.safeZip(accessorsE(x)).map((in, a) => f(in.asInstanceOf, a.asInstanceOf))
        constructorE(args)
      }
    }

    object StagedProductInstances {
      implicit def apply[F[_[_]], T[_]]
        (implicit
          m: ProductGeneric[T],
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          def instances: List[Any] = s.instances

          def accessorsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]] = {
            import r._
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

          def constructorE[A](fields: List[E[Any]])(implicit r: Reflection): E[T[A]] = {
            import r._
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

    trait StagedCoproductInstances[F[_[_]], T[_]] extends StagedInstances[F, T] {
      def instances: List[Any]
      def typetestsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Boolean]]
      def castsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] =
        instances.safeZip(typetestsE(x).safeZip(castsE(x))).foldLeft('{ null }) {
          case (acc, (i, (t, c))) => '{ if ($t) ${ f(i.asInstanceOf, c.asInstanceOf) } else $acc }
        }.asInstanceOf
    }

    object StagedCoproductInstances {
      implicit def apply[F[_[_]], T[_]]
        (implicit
          m: CoproductGeneric[T],
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes],
          x: SummonInstances[Type, m.MirroredElemTypes],
        ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
          def instances: List[Any] = s.instances

          def typetestsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Boolean]] =
            x.instances.map { case tpe: Type[_] =>
              trait DummyK[X]
              val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
              '{ ${ value.asInstanceOf[E[T[Any]]] }.isInstanceOf[$tpe0[Any]] }
            }

          def castsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]] =
            x.instances.map { case tpe: Type[_] =>
              import r._
              tpe.unseal.tpe match {
                case Type.TypeLambda(_, bound :: Nil, _) =>
                  trait DummyK[X]
                  val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
                  '{ ${ value.asInstanceOf[E[T[Any]]] }.asInstanceOf[$tpe0[Any]] }
              }
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

    def eqStagedImpl[T](f1: E[T], f2: E[T])(e: Eq0[T])(implicit r: Reflection): E[Boolean] =
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

    implicit def eqGenP[A](implicit inst: K0.StagedProductInstances[Eq0, A]): Eq0[A] =
      new Eq0[A] {
        def eqv(x: E[A], y: E[A]): RE[Boolean] =
          inst.foldLeft2E(x, y)('{ true })(
            [t] => (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
              '{ $acc && ${ eqt.eqv(t0, t1) }}
          )
      }

    implicit def eqGenC[A](implicit inst: K0.StagedCoproductInstances[Eq0, A]): Eq0[A] =
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

    def mapStagedImpl[F[_], A: Type, B: Type](fa: E[F[A]], f: E[A => B])(e: Functor0[F])(implicit r: Reflection): E[F[B]] =
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

    implicit def derived[F[_]](implicit inst: K1.StagedInstances[Functor0, F]): Functor0[F] =
      new Functor0[F] {
        def map[A: Type, B: Type](fa: E[F[A]])(f: E[A => B]): RE[F[B]] =
          inst.mapE(fa)([t[_]] => (ft: Functor0[t], ta: E[t[A]]) => ft.map(ta)(f))
      }
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

enum C20 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B)
}
enum C30 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B)
}
enum C40 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B)
}
enum C50 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B)
}
enum C60 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B)
}
enum C70 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B)
}
enum C80 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case C7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B)
}
enum C90 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case C7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
  case A8(i: I); case B8(b: B); case C8(i: I); case D8(b: B); case E8(i: I); case F8(b: B); case G8(i: I); case H8(b: B); case I8(i: I); case J8(b: B)
}
enum C100 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case C7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
  case A8(i: I); case B8(b: B); case C8(i: I); case D8(b: B); case E8(i: I); case F8(b: B); case G8(i: I); case H8(b: B); case I8(i: I); case J8(b: B);
  case A9(i: I); case B9(b: B); case C9(i: I); case D9(b: B); case E9(i: I); case F9(b: B); case G9(i: I); case H9(b: B); case I9(i: I); case J9(b: B)
}

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
enum CK10[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
}
enum CK20[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B)
}
enum CK30[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B)
}
enum CK40[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B)
}
enum CK50[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B)
}
enum CK60[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B)
}
enum CK70[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B)
}
enum CK80[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case C7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B)
}
enum CK90[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case C7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
  case A8(a: A); case B8(b: B); case C8(a: A); case D8(b: B); case E8(a: A); case F8(b: B); case G8(a: A); case H8(b: B); case I8(a: A); case J8(b: B)
}
enum CK100[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case C7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
  case A8(a: A); case B8(b: B); case C8(a: A); case D8(b: B); case E8(a: A); case F8(b: B); case G8(a: A); case H8(b: B); case I8(a: A); case J8(b: B);
  case A9(a: A); case B9(b: B); case C9(a: A); case D9(b: B); case E9(a: A); case F9(b: B); case G9(a: A); case H9(b: B); case I9(a: A); case J9(b: B)
}
