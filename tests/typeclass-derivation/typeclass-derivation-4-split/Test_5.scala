// Tests

import Utils._

object Size {
  implicit def intCase: Case[Size.type, Int, Int] = identity(_)
  implicit def stringCase: Case[Size.type, String, Int] = _.length
  implicit def booleanCase: Case[Size.type, Boolean, Int] = _ => 1
}

object Inc {
  implicit def intCase: Case[Inc.type, Int, Int] = _+1
  implicit def stringCase: Case[Inc.type, String, String] = _+"!"
  implicit def booleanCase: Case[Inc.type, Boolean, Boolean] = !_
}

object Test extends App {
  val v0 = Monoid[ISB]
  //val v0 = Monoid.derive(ISB)
  assert(v0.empty == ISB(0, "", false))
  assert(v0.combine(ISB(1, "foo", false), ISB(2, "bar", true)) == ISB(3, "foobar", true))

  val v1 = Monoid[Box[Int]]
  //val v1 = Monoid.derive(Box.monoMirror[Int])
  assert(v1.empty == Box(0))
  assert(v1.combine(Box(1), Box(2)) == Box(3))
  val v2 = Functor[Box]
  //val v2 = Functor.derive(Box)
  assert(v2.map(Box("foo"))(_.length) == Box(3))

  val v3 = Eq[SomeInt]
  //val v3 = Eq.derive(SomeInt)
  assert(v3.eqv(SomeInt(23), SomeInt(23)))
  assert(!v3.eqv(SomeInt(23), SomeInt(13)))
  val v4 = Eq[NoneInt.type]
  //val v4 = Eq.derive(NoneInt)
  assert(v4.eqv(NoneInt, NoneInt))
  val v5 = Eq[OptionInt]
  //val v5 = Eq.derive(OptionInt)
  assert(v5.eqv(SomeInt(23), SomeInt(23)))
  assert(!v5.eqv(SomeInt(23), SomeInt(13)))
  assert(!v5.eqv(SomeInt(23), NoneInt))

  val v6 = Eq[Sm[Int]]
  assert(v6.eqv(Sm(23), Sm(23)))
  assert(!v6.eqv(Sm(23), Sm(13)))
  val v7 = Eq[Nn.type]
  assert(v7.eqv(Nn, Nn))
  val v8 = Eq[Opt[Int]]
  assert(v8.eqv(Sm(23), Sm(23)))
  assert(!v8.eqv(Sm(23), Sm(13)))
  assert(!v8.eqv(Sm(23), Nn))

  val v9 = Functor[Sm]
  assert(v9.map(Sm("foo"))(_.length) == Sm(3))
  val v10 = Functor[Const[Nn.type]]
  assert(v10.map(Nn)(identity) == Nn)
  val v11 = Functor[Opt]
  assert(v11.map(Sm("foo"))(_.length) == Sm(3))
  assert(v11.map(Nn)(identity) == Nn)

  val v12 = Eq[CNil.type]
  assert(v12.eqv(CNil, CNil))
  val v13 = Eq[CCons[Int]]
  assert(v13.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
  assert(!v13.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))
  val v14 = Eq[CList[Int]]
  //val v14 = Eq.derive(CList.monoMirror[Int])
  assert(v14.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
  assert(!v14.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))

  val v15 = Functor[Const[CNil.type]]
  assert(v15.map(CNil)(identity) == CNil)
  val v16 = Functor[CCons]
  assert(v16.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
  val v17 = Functor[CList]
  assert(v17.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
  val v18 = Functor[[t] => CList[Opt[t]]]
  assert(v18.map(CCons(Sm("foo"), CCons(Nn, CCons(Sm("quux"), CNil))))(_.length) == CCons(Sm(3), CCons(Nn, CCons(Sm(4), CNil))))

  val v19 = FunctorK[Order]
  assert(v19.mapK(Order[OptionD](Given("Epoisse"), Default(10)))(OptionD.fold) == Order[Id]("Epoisse", 10))

  val v20 = Bifunctor[ConsF]
  val v21 = Bifunctor[ListF]
  val v22: ListF.List[String] = Fix(ConsF("foo", Fix(ConsF("quux", Fix(ConsF("wibble", Fix(NilF)))))))
  val v23: ListF.List[Int] = Fix(ConsF(3, Fix(ConsF(4, Fix(ConsF(6, Fix(NilF)))))))
  assert(Bifunctor.map((_: String).length)(v22) == v23)

  val v24 = Data[Size.type, ISB, Int]
  assert(v24.gmapQ(ISB(23, "foo", true)).sum == 27)
  val v25 = Data[Size.type, OptionInt, Int]
  assert(v25.gmapQ(NoneInt).sum == 0)
  assert(v25.gmapQ(SomeInt(23)).sum == 23)
  val v26 = Data[Size.type, CList[String], Int]
  assert(v26.gmapQ(CCons("foo", CCons("quux", CCons("wibble", CNil)))).sum == 13)

  val v27 = DataT[Inc.type, ISB]
  assert(v27.gmapT(ISB(23, "foo", true)) == ISB(24, "foo!", false))
  val v28 = DataT[Inc.type, OptionInt]
  assert(v28.gmapT(NoneInt) == NoneInt)
  assert(v28.gmapT(SomeInt(23)) == SomeInt(24))
  val v29 = DataT[Inc.type, CList[Int]]
  assert(v29.gmapT(CCons(1, CCons(2, CCons(3, CNil)))) == CCons(2, CCons(3, CCons(4, CNil))))

  val v30 = Empty[ISB]
  assert(v30.empty == ISB(0, "", false))

  val v31 = EmptyK[Opt]
  assert(v31.empty[Int] == Nn)
  val v32 = EmptyK[CList]
  assert(v32.empty[Int] == CNil)

  val v33 = Pure[Box]
  assert(v33.pure(23) == Box(23))
  val v34 = Pure[CList]
  assert(v34.pure(23) == CCons(23, CNil))

  val v35 = K0.Generic[ISB]
  val v36 = summonValues[v35.ElemLabels]
  assert(v36 == ("i", "s", "b"))

  val v37 = Show[ISB]
  assert(v37.show(ISB(23, "foo", true)) == """ISB(i: 23, s: "foo", b: true)""")

  val v38 = Show[OptionInt]
  assert(v38.show(SomeInt(23)) == "SomeInt(value: 23)")
  assert(v38.show(NoneInt) == "NoneInt")

  val v39 = Show[Box[Int]]
  assert(v39.show(Box(23)) == "Box(x: 23)")

  val v40 = Show[Opt[Int]]
  assert(v40.show(Sm(23)) == "Sm(value: 23)")
  assert(v40.show(Nn) == "Nn")

  val v41 = Show[CList[Int]]
  assert(v41.show((CCons(1, CCons(2, CCons(3, CNil))))) == "CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))")

  val v42 = Show[Order[Id]]
  assert(v42.show(Order[Id]("Epoisse", 10)) == """Order(item: "Epoisse", quantity: 10)""")

  val v43 = Read[ISB]
  assert(v43.read("""ISB(i: 23, s: "foo", b: true)""") == Some((ISB(23, "foo", true), "")))

  val v44 = Read[OptionInt]
  assert(v44.read("SomeInt(value: 23)") == Some((SomeInt(23), "")))
  assert(v44.read("NoneInt") == Some((NoneInt, "")))

  val v45 = Read[Box[Int]]
  assert(v45.read("Box(x: 23)") == Some((Box(23), "")))

  val v46 = Read[Opt[Int]]
  assert(v46.read("Sm(value: 23)") == Some((Sm(23), "")))
  assert(v46.read("Nn") == Some((Nn, "")))

  val v47 = Read[CList[Int]]
  assert(v47.read("CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))") == Some((CCons(1, CCons(2, CCons(3, CNil))), "")))

  val v48 = Read[Order[Id]]
  assert(v48.read("""Order(item: "Epoisse", quantity: 10)""") == Some((Order[Id]("Epoisse", 10), "")))

  val v49 = Transform[BI, ISB]
  assert(v49(BI(true, 23)) == ISB(23, "", true))

  val v50 = K0.ProductGeneric[Box[Int]]
  val v51 = v50.toRepr(Box(23))
  val v51a: Tuple1[Int] = v51
  assert(v51 == Tuple1(23))

  val v52 = K0.ProductGeneric[Order[Id]]
  val v53 = v52.toRepr(Order[Id]("Epoisse", 10))
  val v53a: (String, Int) = v53
  assert(v53 == ("Epoisse", 10))
}
