package playground

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class Benchmarks {
  val p0_a: P0 = P0()
  val p0_b: P0 = P0()
  val p10_a: P10 = P10(0, true, 0, true, 0, true, 0, true, 0, true)
  val p10_b: P10 = P10(0, true, 0, true, 0, true, 0, true, 0, true)
  val p20_a: P20 = P20(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p20_b: P20 = P20(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p30_a: P30 = P30(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p30_b: P30 = P30(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p40_a: P40 = P40(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p40_b: P40 = P40(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p50_a: P50 = P50(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p50_b: P50 = P50(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p60_a: P60 = P60(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p60_b: P60 = P60(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p70_a: P70 = P70(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p70_b: P70 = P70(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p80_a: P80 = P80(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p80_b: P80 = P80(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p90_a: P90 = P90(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p90_b: P90 = P90(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p100_a: P100 = P100(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p100_b: P100 = P100(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)

  val inlinedEqDerivedC10  = Inlined.Eq.derived[C10]
  val inlinedEqDerivedC20  = Inlined.Eq.derived[C20]
  val inlinedEqDerivedC30  = Inlined.Eq.derived[C30]
  val inlinedEqDerivedC40  = Inlined.Eq.derived[C40]
  val inlinedEqDerivedC50  = Inlined.Eq.derived[C50]
  val inlinedEqDerivedC60  = Inlined.Eq.derived[C60]
  val inlinedEqDerivedC70  = Inlined.Eq.derived[C70]
  val inlinedEqDerivedC80  = Inlined.Eq.derived[C80]
  val inlinedEqDerivedC90  = Inlined.Eq.derived[C90]
  val inlinedEqDerivedC100 = Inlined.Eq.derived[C100]

  val inlinedEqDerivedP0   = Inlined.Eq.derived[P0]
  val inlinedEqDerivedP10  = Inlined.Eq.derived[P10]
  val inlinedEqDerivedP20  = Inlined.Eq.derived[P20]
  val inlinedEqDerivedP30  = Inlined.Eq.derived[P30]
  val inlinedEqDerivedP40  = Inlined.Eq.derived[P40]
  val inlinedEqDerivedP50  = Inlined.Eq.derived[P50]
  val inlinedEqDerivedP60  = Inlined.Eq.derived[P60]
  val inlinedEqDerivedP70  = Inlined.Eq.derived[P70]
  val inlinedEqDerivedP80  = Inlined.Eq.derived[P80]
  val inlinedEqDerivedP90  = Inlined.Eq.derived[P90]
  val inlinedEqDerivedP100 = Inlined.Eq.derived[P100]

  val stagedEqDerivedC10   = Eq.derived[C10]
  val stagedEqDerivedC20   = Eq.derived[C20]
  val stagedEqDerivedC30   = Eq.derived[C30]
  val stagedEqDerivedC40   = Eq.derived[C40]
  val stagedEqDerivedC50   = Eq.derived[C50]
  val stagedEqDerivedC60   = Eq.derived[C60]
  val stagedEqDerivedC70   = Eq.derived[C70]
  val stagedEqDerivedC80   = Eq.derived[C80]
  val stagedEqDerivedC90   = Eq.derived[C90]
  val stagedEqDerivedC100  = Eq.derived[C100]

  val stagedEqDerivedP0    = Eq.derived[P0]
  val stagedEqDerivedP10   = Eq.derived[P10]
  val stagedEqDerivedP20   = Eq.derived[P20]
  val stagedEqDerivedP30   = Eq.derived[P30]
  val stagedEqDerivedP40   = Eq.derived[P40]
  val stagedEqDerivedP50   = Eq.derived[P50]
  val stagedEqDerivedP60   = Eq.derived[P60]
  val stagedEqDerivedP70   = Eq.derived[P70]
  val stagedEqDerivedP80   = Eq.derived[P80]
  val stagedEqDerivedP90   = Eq.derived[P90]
  val stagedEqDerivedP100  = Eq.derived[P100]

  val shapeless3EqDerivedC10   = Shapeless3.Eq.derived[C10]
  val shapeless3EqDerivedC20   = Shapeless3.Eq.derived[C20]
  val shapeless3EqDerivedC30   = Shapeless3.Eq.derived[C30]
  val shapeless3EqDerivedC40   = Shapeless3.Eq.derived[C40]
  val shapeless3EqDerivedC50   = Shapeless3.Eq.derived[C50]
  val shapeless3EqDerivedC60   = Shapeless3.Eq.derived[C60]
  val shapeless3EqDerivedC70   = Shapeless3.Eq.derived[C70]
  val shapeless3EqDerivedC80   = Shapeless3.Eq.derived[C80]
  val shapeless3EqDerivedC90   = Shapeless3.Eq.derived[C90]
  val shapeless3EqDerivedC100  = Shapeless3.Eq.derived[C100]

  val shapeless3EqDerivedP0    = Shapeless3.Eq.derived[P0]
  val shapeless3EqDerivedP10   = Shapeless3.Eq.derived[P10]
  val shapeless3EqDerivedP20   = Shapeless3.Eq.derived[P20]
  val shapeless3EqDerivedP30   = Shapeless3.Eq.derived[P30]
  val shapeless3EqDerivedP40   = Shapeless3.Eq.derived[P40]
  val shapeless3EqDerivedP50   = Shapeless3.Eq.derived[P50]
  val shapeless3EqDerivedP60   = Shapeless3.Eq.derived[P60]
  val shapeless3EqDerivedP70   = Shapeless3.Eq.derived[P70]
  val shapeless3EqDerivedP80   = Shapeless3.Eq.derived[P80]
  val shapeless3EqDerivedP90   = Shapeless3.Eq.derived[P90]
  val shapeless3EqDerivedP100  = Shapeless3.Eq.derived[P100]

  val stagedFunctorDerivedCK10   = Functor.derived[CK10]
  val stagedFunctorDerivedCK20   = Functor.derived[CK20]
  val stagedFunctorDerivedCK30   = Functor.derived[CK30]
  val stagedFunctorDerivedCK40   = Functor.derived[CK40]
  val stagedFunctorDerivedCK50   = Functor.derived[CK50]
  val stagedFunctorDerivedCK60   = Functor.derived[CK60]
  val stagedFunctorDerivedCK70   = Functor.derived[CK70]
  val stagedFunctorDerivedCK80   = Functor.derived[CK80]
  val stagedFunctorDerivedCK90   = Functor.derived[CK90]
  val stagedFunctorDerivedCK100  = Functor.derived[CK100]

  val stagedFunctorDerivedPK0    = Functor.derived[PK0]
  val stagedFunctorDerivedPK10   = Functor.derived[PK10]
  val stagedFunctorDerivedPK20   = Functor.derived[PK20]
  val stagedFunctorDerivedPK30   = Functor.derived[PK30]
  val stagedFunctorDerivedPK40   = Functor.derived[PK40]
  val stagedFunctorDerivedPK50   = Functor.derived[PK50]
  val stagedFunctorDerivedPK60   = Functor.derived[PK60]
  val stagedFunctorDerivedPK70   = Functor.derived[PK70]
  val stagedFunctorDerivedPK80   = Functor.derived[PK80]
  val stagedFunctorDerivedPK90   = Functor.derived[PK90]
  val stagedFunctorDerivedPK100  = Functor.derived[PK100]

  val shapeless3FunctorDerivedCK10   = Shapeless3.Functor.derived[CK10]
  val shapeless3FunctorDerivedCK20   = Shapeless3.Functor.derived[CK20]
  val shapeless3FunctorDerivedCK30   = Shapeless3.Functor.derived[CK30]
  val shapeless3FunctorDerivedCK40   = Shapeless3.Functor.derived[CK40]
  val shapeless3FunctorDerivedCK50   = Shapeless3.Functor.derived[CK50]
  val shapeless3FunctorDerivedCK60   = Shapeless3.Functor.derived[CK60]
  val shapeless3FunctorDerivedCK70   = Shapeless3.Functor.derived[CK70]
  val shapeless3FunctorDerivedCK80   = Shapeless3.Functor.derived[CK80]
  val shapeless3FunctorDerivedCK90   = Shapeless3.Functor.derived[CK90]
  val shapeless3FunctorDerivedCK100  = Shapeless3.Functor.derived[CK100]

  val shapeless3FunctorDerivedPK0    = Shapeless3.Functor.derived[PK0]
  val shapeless3FunctorDerivedPK10   = Shapeless3.Functor.derived[PK10]
  val shapeless3FunctorDerivedPK20   = Shapeless3.Functor.derived[PK20]
  val shapeless3FunctorDerivedPK30   = Shapeless3.Functor.derived[PK30]
  val shapeless3FunctorDerivedPK40   = Shapeless3.Functor.derived[PK40]
  val shapeless3FunctorDerivedPK50   = Shapeless3.Functor.derived[PK50]
  val shapeless3FunctorDerivedPK60   = Shapeless3.Functor.derived[PK60]
  val shapeless3FunctorDerivedPK70   = Shapeless3.Functor.derived[PK70]
  val shapeless3FunctorDerivedPK80   = Shapeless3.Functor.derived[PK80]
  val shapeless3FunctorDerivedPK90   = Shapeless3.Functor.derived[PK90]
  val shapeless3FunctorDerivedPK100  = Shapeless3.Functor.derived[PK100]

  @Benchmark def assertInlinedEqDerivedP0EqvP0AP0B: Unit =
                 assert(inlinedEqDerivedP0.eqv(p0_a, p0_b))
  @Benchmark def assertInlinedEqDerivedP10EqvP10AP10B: Unit =
                 assert(inlinedEqDerivedP10.eqv(p10_a, p10_b))
  @Benchmark def assertInlinedEqDerivedP20EqvP20AP20B: Unit =
                 assert(inlinedEqDerivedP20.eqv(p20_a, p20_b))
  @Benchmark def assertInlinedEqDerivedP30EqvP30AP30B: Unit =
                 assert(inlinedEqDerivedP30.eqv(p30_a, p30_b))
  @Benchmark def assertInlinedEqDerivedP40EqvP40AP40B: Unit =
                 assert(inlinedEqDerivedP40.eqv(p40_a, p40_b))
  @Benchmark def assertInlinedEqDerivedP50EqvP50AP50B: Unit =
                 assert(inlinedEqDerivedP50.eqv(p50_a, p50_b))
  @Benchmark def assertInlinedEqDerivedP60EqvP60AP60B: Unit =
                 assert(inlinedEqDerivedP60.eqv(p60_a, p60_b))
  @Benchmark def assertInlinedEqDerivedP70EqvP70AP70B: Unit =
                 assert(inlinedEqDerivedP70.eqv(p70_a, p70_b))
  @Benchmark def assertInlinedEqDerivedP80EqvP80AP80B: Unit =
                 assert(inlinedEqDerivedP80.eqv(p80_a, p80_b))
  @Benchmark def assertInlinedEqDerivedP90EqvP90AP90B: Unit =
                 assert(inlinedEqDerivedP90.eqv(p90_a, p90_b))
  @Benchmark def assertInlinedEqDerivedP99EqvP99AP99B: Unit =
                 assert(inlinedEqDerivedP100.eqv(p100_a, p100_b))

  @Benchmark def assertStagedEqDerivedP0EqvP0AP0B: Unit =
                 assert(stagedEqDerivedP0.eqv(p0_a, p0_b))
  @Benchmark def assertStagedEqDerivedP10EqvP10AP10B: Unit =
                 assert(stagedEqDerivedP10.eqv(p10_a, p10_b))
  @Benchmark def assertStagedEqDerivedP20EqvP20AP20B: Unit =
                 assert(stagedEqDerivedP20.eqv(p20_a, p20_b))
  @Benchmark def assertStagedEqDerivedP30EqvP30AP30B: Unit =
                 assert(stagedEqDerivedP30.eqv(p30_a, p30_b))
  @Benchmark def assertStagedEqDerivedP40EqvP40AP40B: Unit =
                 assert(stagedEqDerivedP40.eqv(p40_a, p40_b))
  @Benchmark def assertStagedEqDerivedP50EqvP50AP50B: Unit =
                 assert(stagedEqDerivedP50.eqv(p50_a, p50_b))
  @Benchmark def assertStagedEqDerivedP60EqvP60AP60B: Unit =
                 assert(stagedEqDerivedP60.eqv(p60_a, p60_b))
  @Benchmark def assertStagedEqDerivedP70EqvP70AP70B: Unit =
                 assert(stagedEqDerivedP70.eqv(p70_a, p70_b))
  @Benchmark def assertStagedEqDerivedP80EqvP80AP80B: Unit =
                 assert(stagedEqDerivedP80.eqv(p80_a, p80_b))
  @Benchmark def assertStagedEqDerivedP90EqvP90AP90B: Unit =
                 assert(stagedEqDerivedP90.eqv(p90_a, p90_b))
  @Benchmark def assertStagedEqDerivedP99EqvP99AP99B: Unit =
                 assert(stagedEqDerivedP100.eqv(p100_a, p100_b))

  @Benchmark def assertShapeless3EqDerivedP0EqvP0AP0B: Unit =
                 assert(shapeless3EqDerivedP0.eqv(p0_a, p0_b))
  @Benchmark def assertShapeless3EqDerivedP10EqvP10AP10B: Unit =
                 assert(shapeless3EqDerivedP10.eqv(p10_a, p10_b))
  @Benchmark def assertShapeless3EqDerivedP20EqvP20AP20B: Unit =
                 assert(shapeless3EqDerivedP20.eqv(p20_a, p20_b))
  @Benchmark def assertShapeless3EqDerivedP30EqvP30AP30B: Unit =
                 assert(shapeless3EqDerivedP30.eqv(p30_a, p30_b))
  @Benchmark def assertShapeless3EqDerivedP40EqvP40AP40B: Unit =
                 assert(shapeless3EqDerivedP40.eqv(p40_a, p40_b))
  @Benchmark def assertShapeless3EqDerivedP50EqvP50AP50B: Unit =
                 assert(shapeless3EqDerivedP50.eqv(p50_a, p50_b))
  @Benchmark def assertShapeless3EqDerivedP60EqvP60AP60B: Unit =
                 assert(shapeless3EqDerivedP60.eqv(p60_a, p60_b))
  @Benchmark def assertShapeless3EqDerivedP70EqvP70AP70B: Unit =
                 assert(shapeless3EqDerivedP70.eqv(p70_a, p70_b))
  @Benchmark def assertShapeless3EqDerivedP80EqvP80AP80B: Unit =
                 assert(shapeless3EqDerivedP80.eqv(p80_a, p80_b))
  @Benchmark def assertShapeless3EqDerivedP90EqvP90AP90B: Unit =
                 assert(shapeless3EqDerivedP90.eqv(p90_a, p90_b))
  @Benchmark def assertShapeless3EqDerivedP99EqvP99AP99B: Unit =
                 assert(shapeless3EqDerivedP100.eqv(p100_a, p100_b))

  @Benchmark def assertP0AP0B: Unit =
                 assert(p0_a == p0_b)
  @Benchmark def assertP10AP10B: Unit =
                 assert(p10_a == p10_b)
  @Benchmark def assertP20AP20B: Unit =
                 assert(p20_a == p20_b)
  @Benchmark def assertP30AP30B: Unit =
                 assert(p30_a == p30_b)
  @Benchmark def assertP40AP40B: Unit =
                 assert(p40_a == p40_b)
  @Benchmark def assertP50AP50B: Unit =
                 assert(p50_a == p50_b)
  @Benchmark def assertP60AP60B: Unit =
                 assert(p60_a == p60_b)
  @Benchmark def assertP70AP70B: Unit =
                 assert(p70_a == p70_b)
  @Benchmark def assertP80AP80B: Unit =
                 assert(p80_a == p80_b)
  @Benchmark def assertP90AP90B: Unit =
                 assert(p90_a == p90_b)
  @Benchmark def assertP99AP99B: Unit =
                 assert(p100_a == p100_b)


  val c10_a: C10 = C10_J0(true)
  val c10_b: C10 = C10_J0(true)
  val c20_a: C20 = C20_J1(true)
  val c20_b: C20 = C20_J1(true)
  val c30_a: C30 = C30_J2(true)
  val c30_b: C30 = C30_J2(true)
  val c40_a: C40 = C40_J3(true)
  val c40_b: C40 = C40_J3(true)
  val c50_a: C50 = C50_J4(true)
  val c50_b: C50 = C50_J4(true)
  val c60_a: C60 = C60_J5(true)
  val c60_b: C60 = C60_J5(true)
  val c70_a: C70 = C70_J6(true)
  val c70_b: C70 = C70_J6(true)
  val c80_a: C80 = C80_J7(true)
  val c80_b: C80 = C80_J7(true)
  val c90_a: C90 = C90_J8(true)
  val c90_b: C90 = C90_J8(true)
  val c100_a: C100 = C100_J9(true)
  val c100_b: C100 = C100_J9(true)

  @Benchmark def assertInlinedEqDerivedC10EqvC10AC10B: Unit =
                 assert(inlinedEqDerivedC10.eqv(c10_a, c10_b))
  @Benchmark def assertInlinedEqDerivedC20EqvC20AC20B: Unit =
                 assert(inlinedEqDerivedC20.eqv(c20_a, c20_b))
  @Benchmark def assertInlinedEqDerivedC30EqvC30AC30B: Unit =
                 assert(inlinedEqDerivedC30.eqv(c30_a, c30_b))
  @Benchmark def assertInlinedEqDerivedC40EqvC40AC40B: Unit =
                 assert(inlinedEqDerivedC40.eqv(c40_a, c40_b))
  @Benchmark def assertInlinedEqDerivedC50EqvC50AC50B: Unit =
                 assert(inlinedEqDerivedC50.eqv(c50_a, c50_b))
  @Benchmark def assertInlinedEqDerivedC60EqvC60AC60B: Unit =
                 assert(inlinedEqDerivedC60.eqv(c60_a, c60_b))
  @Benchmark def assertInlinedEqDerivedC70EqvC70AC70B: Unit =
                 assert(inlinedEqDerivedC70.eqv(c70_a, c70_b))
  @Benchmark def assertInlinedEqDerivedC80EqvC80AC80B: Unit =
                 assert(inlinedEqDerivedC80.eqv(c80_a, c80_b))
  @Benchmark def assertInlinedEqDerivedC90EqvC90AC90B: Unit =
                 assert(inlinedEqDerivedC90.eqv(c90_a, c90_b))
  @Benchmark def assertInlinedEqDerivedC99EqvC99AC99B: Unit =
                 assert(inlinedEqDerivedC100.eqv(c100_a, c100_b))

  @Benchmark def assertStagedEqDerivedC10EqvC10AC10B: Unit =
                 assert(stagedEqDerivedC10.eqv(c10_a, c10_b))
  @Benchmark def assertStagedEqDerivedC20EqvC20AC20B: Unit =
                 assert(stagedEqDerivedC20.eqv(c20_a, c20_b))
  @Benchmark def assertStagedEqDerivedC30EqvC30AC30B: Unit =
                 assert(stagedEqDerivedC30.eqv(c30_a, c30_b))
  @Benchmark def assertStagedEqDerivedC40EqvC40AC40B: Unit =
                 assert(stagedEqDerivedC40.eqv(c40_a, c40_b))
  @Benchmark def assertStagedEqDerivedC50EqvC50AC50B: Unit =
                 assert(stagedEqDerivedC50.eqv(c50_a, c50_b))
  @Benchmark def assertStagedEqDerivedC60EqvC60AC60B: Unit =
                 assert(stagedEqDerivedC60.eqv(c60_a, c60_b))
  @Benchmark def assertStagedEqDerivedC70EqvC70AC70B: Unit =
                 assert(stagedEqDerivedC70.eqv(c70_a, c70_b))
  @Benchmark def assertStagedEqDerivedC80EqvC80AC80B: Unit =
                 assert(stagedEqDerivedC80.eqv(c80_a, c80_b))
  @Benchmark def assertStagedEqDerivedC90EqvC90AC90B: Unit =
                 assert(stagedEqDerivedC90.eqv(c90_a, c90_b))
  @Benchmark def assertStagedEqDerivedC99EqvC99AC99B: Unit =
                 assert(stagedEqDerivedC100.eqv(c100_a, c100_b))

  @Benchmark def assertShapeless3EqDerivedC10EqvC10AC10B: Unit =
                 assert(shapeless3EqDerivedC10.eqv(c10_a, c10_b))
  @Benchmark def assertShapeless3EqDerivedC20EqvC20AC20B: Unit =
                 assert(shapeless3EqDerivedC20.eqv(c20_a, c20_b))
  @Benchmark def assertShapeless3EqDerivedC30EqvC30AC30B: Unit =
                 assert(shapeless3EqDerivedC30.eqv(c30_a, c30_b))
  @Benchmark def assertShapeless3EqDerivedC40EqvC40AC40B: Unit =
                 assert(shapeless3EqDerivedC40.eqv(c40_a, c40_b))
  @Benchmark def assertShapeless3EqDerivedC50EqvC50AC50B: Unit =
                 assert(shapeless3EqDerivedC50.eqv(c50_a, c50_b))
  @Benchmark def assertShapeless3EqDerivedC60EqvC60AC60B: Unit =
                 assert(shapeless3EqDerivedC60.eqv(c60_a, c60_b))
  @Benchmark def assertShapeless3EqDerivedC70EqvC70AC70B: Unit =
                 assert(shapeless3EqDerivedC70.eqv(c70_a, c70_b))
  @Benchmark def assertShapeless3EqDerivedC80EqvC80AC80B: Unit =
                 assert(shapeless3EqDerivedC80.eqv(c80_a, c80_b))
  @Benchmark def assertShapeless3EqDerivedC90EqvC90AC90B: Unit =
                 assert(shapeless3EqDerivedC90.eqv(c90_a, c90_b))
  @Benchmark def assertShapeless3EqDerivedC99EqvC99AC99B: Unit =
                 assert(shapeless3EqDerivedC100.eqv(c100_a, c100_b))

  @Benchmark def assertC10AC10B: Unit =
                 assert(c10_a == c10_b)
  @Benchmark def assertC20AC20B: Unit =
                 assert(c20_a == c20_b)
  @Benchmark def assertC30AC30B: Unit =
                 assert(c30_a == c30_b)
  @Benchmark def assertC40AC40B: Unit =
                 assert(c40_a == c40_b)
  @Benchmark def assertC50AC50B: Unit =
                 assert(c50_a == c50_b)
  @Benchmark def assertC60AC60B: Unit =
                 assert(c60_a == c60_b)
  @Benchmark def assertC70AC70B: Unit =
                 assert(c70_a == c70_b)
  @Benchmark def assertC80AC80B: Unit =
                 assert(c80_a == c80_b)
  @Benchmark def assertC90AC90B: Unit =
                 assert(c90_a == c90_b)
  @Benchmark def assertC99AC99B: Unit =
                 assert(c100_a == c100_b)


  val inc: Int => Int = x => x + 1

  val pk0: PK0[Int] = PK0[Int]()
  val pk10: PK10[Int] = PK10[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk20: PK20[Int] = PK20[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk30: PK30[Int] = PK30[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk40: PK40[Int] = PK40[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk50: PK50[Int] = PK50[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk60: PK60[Int] = PK60[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk70: PK70[Int] = PK70[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk80: PK80[Int] = PK80[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk90: PK90[Int] = PK90[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk100: PK100[Int] = PK100[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

  val ck10: CK10[Int] = CK10_I0(1)
  val ck20: CK20[Int] = CK20_I1(1)
  val ck30: CK30[Int] = CK30_I2(1)
  val ck40: CK40[Int] = CK40_I3(1)
  val ck50: CK50[Int] = CK50_I4(1)
  val ck60: CK60[Int] = CK60_I5(1)
  val ck70: CK70[Int] = CK70_I6(1)
  val ck80: CK80[Int] = CK80_I7(1)
  val ck90: CK90[Int] = CK90_I8(1)
  val ck100: CK100[Int] = CK100_I9(1)

  @Benchmark def stagedFunctorDerivedCK10MapCK10Inc: Unit =
                 stagedFunctorDerivedCK10.map(ck10)(inc)
  @Benchmark def stagedFunctorDerivedCK20MapCK20Inc: Unit =
                 stagedFunctorDerivedCK20.map(ck20)(inc)
  @Benchmark def stagedFunctorDerivedCK30MapCK30Inc: Unit =
                 stagedFunctorDerivedCK30.map(ck30)(inc)
  @Benchmark def stagedFunctorDerivedCK40MapCK40Inc: Unit =
                 stagedFunctorDerivedCK40.map(ck40)(inc)
  @Benchmark def stagedFunctorDerivedCK50MapCK50Inc: Unit =
                 stagedFunctorDerivedCK50.map(ck50)(inc)
  @Benchmark def stagedFunctorDerivedCK60MapCK60Inc: Unit =
                 stagedFunctorDerivedCK60.map(ck60)(inc)
  @Benchmark def stagedFunctorDerivedCK70MapCK70Inc: Unit =
                 stagedFunctorDerivedCK70.map(ck70)(inc)
  @Benchmark def stagedFunctorDerivedCK80MapCK80Inc: Unit =
                 stagedFunctorDerivedCK80.map(ck80)(inc)
  @Benchmark def stagedFunctorDerivedCK90MapCK90Inc: Unit =
                 stagedFunctorDerivedCK90.map(ck90)(inc)
  @Benchmark def stagedFunctorDerivedCK99MapCK99Inc: Unit =
                 stagedFunctorDerivedCK100.map(ck100)(inc)

  @Benchmark def stagedFunctorDerivedPK0MapPK0Inc: Unit =
                 stagedFunctorDerivedPK0.map(pk0)(inc)
  @Benchmark def stagedFunctorDerivedPK10MapPK10Inc: Unit =
                 stagedFunctorDerivedPK10.map(pk10)(inc)
  @Benchmark def stagedFunctorDerivedPK20MapPK20Inc: Unit =
                 stagedFunctorDerivedPK20.map(pk20)(inc)
  @Benchmark def stagedFunctorDerivedPK30MapPK30Inc: Unit =
                 stagedFunctorDerivedPK30.map(pk30)(inc)
  @Benchmark def stagedFunctorDerivedPK40MapPK40Inc: Unit =
                 stagedFunctorDerivedPK40.map(pk40)(inc)
  @Benchmark def stagedFunctorDerivedPK50MapPK50Inc: Unit =
                 stagedFunctorDerivedPK50.map(pk50)(inc)
  @Benchmark def stagedFunctorDerivedPK60MapPK60Inc: Unit =
                 stagedFunctorDerivedPK60.map(pk60)(inc)
  @Benchmark def stagedFunctorDerivedPK70MapPK70Inc: Unit =
                 stagedFunctorDerivedPK70.map(pk70)(inc)
  @Benchmark def stagedFunctorDerivedPK80MapPK80Inc: Unit =
                 stagedFunctorDerivedPK80.map(pk80)(inc)
  @Benchmark def stagedFunctorDerivedPK90MapPK90Inc: Unit =
                 stagedFunctorDerivedPK90.map(pk90)(inc)
  @Benchmark def stagedFunctorDerivedPK99MapPK99Inc: Unit =
                 stagedFunctorDerivedPK100.map(pk100)(inc)


  @Benchmark def shapeless3FunctorDerivedCK10MapCK10Inc: Unit =
                 shapeless3FunctorDerivedCK10.map(ck10)(inc)
  @Benchmark def shapeless3FunctorDerivedCK20MapCK20Inc: Unit =
                 shapeless3FunctorDerivedCK20.map(ck20)(inc)
  @Benchmark def shapeless3FunctorDerivedCK30MapCK30Inc: Unit =
                 shapeless3FunctorDerivedCK30.map(ck30)(inc)
  @Benchmark def shapeless3FunctorDerivedCK40MapCK40Inc: Unit =
                 shapeless3FunctorDerivedCK40.map(ck40)(inc)
  @Benchmark def shapeless3FunctorDerivedCK50MapCK50Inc: Unit =
                 shapeless3FunctorDerivedCK50.map(ck50)(inc)
  @Benchmark def shapeless3FunctorDerivedCK60MapCK60Inc: Unit =
                 shapeless3FunctorDerivedCK60.map(ck60)(inc)
  @Benchmark def shapeless3FunctorDerivedCK70MapCK70Inc: Unit =
                 shapeless3FunctorDerivedCK70.map(ck70)(inc)
  @Benchmark def shapeless3FunctorDerivedCK80MapCK80Inc: Unit =
                 shapeless3FunctorDerivedCK80.map(ck80)(inc)
  @Benchmark def shapeless3FunctorDerivedCK90MapCK90Inc: Unit =
                 shapeless3FunctorDerivedCK90.map(ck90)(inc)
  @Benchmark def shapeless3FunctorDerivedCK99MapCK99Inc: Unit =
                 shapeless3FunctorDerivedCK100.map(ck100)(inc)
  @Benchmark def shapeless3FunctorDerivedPK0MapPK0Inc: Unit =
                 shapeless3FunctorDerivedPK0.map(pk0)(inc)
  @Benchmark def shapeless3FunctorDerivedPK10MapPK10Inc: Unit =
                 shapeless3FunctorDerivedPK10.map(pk10)(inc)
  @Benchmark def shapeless3FunctorDerivedPK20MapPK20Inc: Unit =
                 shapeless3FunctorDerivedPK20.map(pk20)(inc)
  @Benchmark def shapeless3FunctorDerivedPK30MapPK30Inc: Unit =
                 shapeless3FunctorDerivedPK30.map(pk30)(inc)
  @Benchmark def shapeless3FunctorDerivedPK40MapPK40Inc: Unit =
                 shapeless3FunctorDerivedPK40.map(pk40)(inc)
  @Benchmark def shapeless3FunctorDerivedPK50MapPK50Inc: Unit =
                 shapeless3FunctorDerivedPK50.map(pk50)(inc)
  @Benchmark def shapeless3FunctorDerivedPK60MapPK60Inc: Unit =
                 shapeless3FunctorDerivedPK60.map(pk60)(inc)
  @Benchmark def shapeless3FunctorDerivedPK70MapPK70Inc: Unit =
                 shapeless3FunctorDerivedPK70.map(pk70)(inc)
  @Benchmark def shapeless3FunctorDerivedPK80MapPK80Inc: Unit =
                 shapeless3FunctorDerivedPK80.map(pk80)(inc)
  @Benchmark def shapeless3FunctorDerivedPK90MapPK90Inc: Unit =
                 shapeless3FunctorDerivedPK90.map(pk90)(inc)
  @Benchmark def shapeless3FunctorDerivedPK99MapPK99Inc: Unit =
                 shapeless3FunctorDerivedPK100.map(pk100)(inc)

}
