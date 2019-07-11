# set -eux

o() {
  mkdir -p /tmp/o
  t=$(mktemp -p /tmp/o)
  cat - > "$t"
  echo "$t"
}

mkdir "$HOME/out2" >/dev/null 2>&1 && sbt "run -d $HOME/out2 $HOME/workspace/dotty/tests/run-custom-args/typeclass-derivation-macro/1.scala"

for framework in Inlined Shapeless3 Staged; do
  for type in P0 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    test -f "bench/$framework-eq-$type.log" ||\
    sbt "dotty-bench-bootstrapped/jmh:run 10 15 $(echo "val x = $framework.Eq.derived[$type]" | o) -classpath $HOME/out2 -Xmax-inlines 1000" | tee "bench/$framework-eq-$type.log"
  done
done

for framework in Shapeless3 Staged; do
  for type in PK0 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100  CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
    test -f "bench/$framework-functor-$type.log" ||\
    sbt "dotty-bench-bootstrapped/jmh:run 10 15 $(echo "val x = $framework.Functor.derived[$type]" | o) -classpath $HOME/out2 -Xmax-inlines 1000" | tee "bench/$framework-functor-$type.log"
  done
done

test -f "bench/runtime.log" ||\
(cd "bench/jmh-dotty" && sbt "jmh:run -wi 10 -i 20 -f 1 playground.Benchmarks" | tee "../runtime.log")

cmd=""
mkdir -p "bench/outs"

for framework in Inlined Shapeless3 Staged; do
  for type in P0 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    mkdir -p "bench/outs/$framework-$type"
    cmd="$cmd ; run $(echo "val x = $framework.Eq.derived[$type]" | o) -classpath $HOME/out2 -Xmax-inlines 1000 -d bench/outs/$framework-$type"
  done
done

for framework in Shapeless3 Staged; do
  for type in PK0 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
    mkdir -p "bench/outs/$framework-$type"
    cmd="$cmd ; run $(echo "val x = $framework.Functor.derived[$type]" | o) -classpath $HOME/out2 -Xmax-inlines 1000 -d bench/outs/$framework-$type"
  done
done

test -f "bench/bytecode-sizes.log" ||\
sbt "$cmd"

rm "bench/bytecode-sizes.log" || true
for framework in Inlined Shapeless3 Staged; do
  for type in P0 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    printf "$framework-$type,\t" >> "bench/bytecode-sizes.log"
    { find "bench/outs/$framework-$type" -type f -name "*.class" -printf "%s+"; echo 0; } | bc >> "bench/bytecode-sizes.log"
  done
done

for framework in Shapeless3 Staged; do
  for type in PK0 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
    printf "$framework-$type,\t" >> "bench/bytecode-sizes.log"
    { find "bench/outs/$framework-$type" -type f -name "*.class" -printf "%s+"; echo 0; } | bc >> "bench/bytecode-sizes.log"
  done
done

echo
echo --
echo Reporting
echo --


echo; echo "Compile time P"

for framework in Inlined Shapeless3 Staged; do
  for type in P0 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100; do
    printf "$framework.$type"
    cat "bench/$framework-eq-$type.log" | grep '±(' | grep -oP '.*(?=±)' | sed "s/  /,\t/"
  done
done

echo; echo "Compile time C"

for framework in Inlined Shapeless3 Staged; do
  for type in C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    printf "$framework.$type"
    cat "bench/$framework-eq-$type.log" | grep '±(' | grep -oP '.*(?=±)' | sed "s/  /,\t/"
  done
done

echo; echo "Compile time PK"

for framework in Shapeless3 Staged; do
  for type in PK0 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100; do
    printf "$framework.$type"
    cat "bench/$framework-functor-$type.log" | grep '±(' | grep -oP '.*(?=±)' | sed "s/  /,\t/"
  done
done

echo; echo "Compile time CK"

for framework in Shapeless3 Staged; do
  for type in CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100 ; do
    printf "$framework.$type"
    cat "bench/$framework-functor-$type.log" | grep '±(' | grep -oP '.*(?=±)' | sed "s/  /,\t/"
  done
done

echo; echo "Runtime P\tassertP\tInlinedEqDerivedP\tShapeless3EqDerivedP\tStagedEqDerivedP"
paste \
  $(seq 0 10 100 | o) \
  $(cat "bench/runtime.log" | grep -e 'assertP.*thrpt'                    | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'InlinedEqDerivedP.*thrpt'          | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'Shapeless3EqDerivedP.*thrpt'       | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'StagedEqDerivedP.*thrpt'           | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Runtime C\tassertC\tInlinedEqDerivedC\tShapeless3EqDerivedC\tStagedEqDerivedC"
paste \
  $(seq 10 10 100 | o) \
  $(cat "bench/runtime.log" | grep -e 'assertC.*thrpt'                    | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'InlinedEqDerivedC.*thrpt'          | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'Shapeless3EqDerivedC.*thrpt'       | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'StagedEqDerivedC.*thrpt'           | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Runtime PK\tshapeless3FunctorDerivedPK\tstagedFunctorDerivedPK"
paste \
  $(seq 0 10 100 | o) \
  $(cat "bench/runtime.log" | grep -e 'shapeless3FunctorDerivedPK.*thrpt' | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'stagedFunctorDerivedPK.*thrpt'     | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Runtime CK\tshapeless3FunctorDerivedCK\tstagedFunctorDerivedCK"
paste \
  $(seq 10 10 100 | o) \
  $(cat "bench/runtime.log" | grep -e 'shapeless3FunctorDerivedCK.*thrpt' | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'stagedFunctorDerivedCK.*thrpt'     | cut -c 19-88 | cut -c-54 --complement |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Bytecode P\tInlined\tShapeless3\tStaged"
paste \
  $(seq 0 10 100 | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Inlined-P[0-9].*,'    | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-P[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-P[0-9].*,'     | grep -oe '[0-9]*$' | o)

echo; echo "Bytecode C\tInlined\tShapeless3\tStaged"
paste \
  $(seq 10 10 100 | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Inlined-C[0-9].*,'    | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-C[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-C[0-9].*,'     | grep -oe '[0-9]*$' | o)

echo; echo "Bytecode PK\tShapeless3\tStaged"
paste \
  $(seq 0 10 100 | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-PK[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-PK[0-9].*,'     | grep -oe '[0-9]*$' | o)

echo; echo "Bytecode CK\tShapeless3\tStaged"
paste \
  $(seq 10 10 100 | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-CK[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-CK[0-9].*,'     | grep -oe '[0-9]*$' | o)
