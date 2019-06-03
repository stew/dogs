package cats.collections

import _root_.cats.{collections => cc}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.collection.{immutable, mutable}
import scala.math.pow
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
abstract class DoSetup {

  // @Param(Array("10000000")) var range: Int = _
  // @Param(Array("5000")) var size: Int = _

  @Param(Array("262144")) var range: Int = _
  @Param(Array("1024")) var size: Int = _

  // @Param(Array("1000")) var range: Int = _
  // @Param(Array("100")) var size: Int = _

  var indices: Array[Int] = _

  var values: Array[Int] = _
  var sci: immutable.BitSet = _
  var scm: mutable.BitSet = _
  var iset: immutable.Set[Int] = _
  var ccbs: cc.BitSet = _

  var values2: Array[Int] = _
  var sci2: immutable.BitSet = _
  var scm2: mutable.BitSet = _
  var iset2: immutable.Set[Int] = _
  var ccbs2: cc.BitSet = _

  def alloc(r: Random, num: Int, minValue: Int, maxValue: Int): Array[Int] = {
    val array = new Array[Int](num)
    val diff = maxValue.toLong - minValue.toLong
    var seen = Set.empty[Long]
    var i = 0
    while (i < num) {
      val n = ((Random.nextLong & 0x7fffffffffffffffL) % diff) + minValue
      if (!seen(n)) {
        array(i) = n.toInt
        i += 1
      }
    }
    array
  }

  @Setup
  def setup(): Unit = {
    val r = new Random(0x13572468)
    values = alloc(r, size, 0, range)
    values2 = alloc(r, size, 0, range)
    indices = alloc(r, size / 10, 0, range)

    sci = immutable.BitSet(values: _*)
    scm = mutable.BitSet(values: _*)
    iset = immutable.Set(values: _*)
    ccbs = cc.BitSet(values: _*)

    sci2 = immutable.BitSet(values2: _*)
    scm2 = mutable.BitSet(values2: _*)
    iset2 = immutable.Set(values2: _*)
    ccbs2 = cc.BitSet(values2: _*)
  }
}

// all benchmarks

class AllBenchmarks extends DoSetup
    with BuildSci with BuildScm with BuildIset with BuildCcbs
    with BulkAddSci with BulkAddScm with BulkAddIset with BulkAddCcbs
    with BulkRemoveSci with BulkRemoveScm with BulkRemoveIset with BulkRemoveCcbs
    with FoldIntoSci with FoldIntoScm with FoldIntoIset with FoldIntoCcbs
    with FoldOutOfSci with FoldOutOfScm with FoldOutOfIset with FoldOutOfCcbs
    with LookupSci with LookupScm with LookupIset with LookupCcbs
    with MergeSci with MergeScm with MergeIset with MergeCcbs
    with IntersectSci with IntersectScm with IntersectIset with IntersectCcbs
    with XorSci with XorScm with XorIset with XorCcbs
    with DiffSci with DiffScm with DiffIset with DiffCcbs

// per-method benchmarks

class BuildBenchmarks extends DoSetup
    with BuildSci with BuildScm with BuildIset with BuildCcbs

class BulkAddBenchmarks extends DoSetup
    with BulkAddSci with BulkAddScm with BulkAddIset with BulkAddCcbs

class BulkRemoveBenchmarks extends DoSetup
    with BulkRemoveSci with BulkRemoveScm with BulkRemoveIset with BulkRemoveCcbs

class FoldIntoBenchmarks extends DoSetup
    with FoldIntoSci with FoldIntoScm with FoldIntoIset with FoldIntoCcbs

class FoldOutOfBenchmarks extends DoSetup
    with FoldOutOfSci with FoldOutOfScm with FoldOutOfIset with FoldOutOfCcbs

class LookupBenchmarks extends DoSetup
    with LookupSci with LookupScm with LookupIset with LookupCcbs

class MergeBenchmarks extends DoSetup
    with MergeSci with MergeScm with MergeIset with MergeCcbs

class IntersectBenchmarks extends DoSetup
    with IntersectSci with IntersectScm with IntersectIset with IntersectCcbs

class XorBenchmarks extends DoSetup
    with XorSci with XorScm with XorIset with XorCcbs

class DiffBenchmarks extends DoSetup
    with DiffSci with DiffScm with DiffIset with DiffCcbs

// per-data type benchmarks

class SciBenchmarks extends DoSetup
    with BuildSci
    with BulkAddSci
    with BulkRemoveSci
    with FoldIntoSci
    with FoldOutOfSci
    with LookupSci
    with MergeSci
    with IntersectSci
    with XorSci
    with DiffSci

class ScmBenchmarks extends DoSetup
    with BuildScm
    with BulkAddScm
    with BulkRemoveScm
    with FoldIntoScm
    with FoldOutOfScm
    with LookupScm
    with MergeScm
    with IntersectScm
    with XorScm
    with DiffScm

class IsetBenchmarks extends DoSetup
    with BuildIset
    with BulkAddIset
    with BulkRemoveIset
    with FoldIntoIset
    with FoldOutOfIset
    with LookupIset
    with MergeIset
    with IntersectIset
    with XorIset
    with DiffIset

class CcbsBenchmarks extends DoSetup
    // with BuildCcbs
    // with BulkAddCcbs
    // with BulkRemoveCcbs
    // with FoldIntoCcbs
    // with FoldOutOfCcbs
    // with LookupCcbs
    // with MergeCcbs
    with IntersectCcbs
    // with XorCcbs
    // with DiffCcbs

// benchmark traits

trait BuildSci { self: DoSetup =>
  @Benchmark
  def buildSci(): immutable.BitSet =
    immutable.BitSet(values: _*)
}

trait BuildScm { self: DoSetup =>
  @Benchmark
  def buildScm(): mutable.BitSet =
    mutable.BitSet(values: _*)
}

trait BuildIset { self: DoSetup =>
  @Benchmark
  def buildIset(): immutable.Set[Int] =
    immutable.Set[Int](values: _*)
}

trait BuildCcbs { self: DoSetup =>
  @Benchmark
  def buildCcbs(): cc.BitSet =
    cc.BitSet(values: _*)
}

trait BulkAddSci { self: DoSetup =>
  @Benchmark
  def bulkAddSci(): immutable.BitSet =
    sci ++ values2
}

trait BulkAddScm { self: DoSetup =>
  @Benchmark
  def bulkAddScm(): mutable.BitSet = {
    val b = scm.clone()
    b ++= values2
    b
  }
}

trait BulkAddIset { self: DoSetup =>
  @Benchmark
  def bulkAddIset(): immutable.Set[Int] =
    iset ++ values2
}

trait BulkAddCcbs { self: DoSetup =>
  @Benchmark
  def bulkAddCcbs(): cc.BitSet =
    ccbs ++ values2
  @Benchmark
  def bulkAddCcbs2(): cc.BitSet =
    ccbs | BitSet.fromArray(values2)
}

trait BulkRemoveSci { self: DoSetup =>
  @Benchmark
  def bulkRemoveSci(): immutable.BitSet =
    sci -- values2
}

trait BulkRemoveScm { self: DoSetup =>
  @Benchmark
  def bulkRemoveScm(): mutable.BitSet = {
    val b = scm.clone()
    b --= values2
    b
  }
}

trait BulkRemoveIset { self: DoSetup =>
  @Benchmark
  def bulkRemoveIset(): immutable.Set[Int] =
    iset -- values2
}

trait BulkRemoveCcbs { self: DoSetup =>
  @Benchmark
  def bulkRemoveCcbs(): cc.BitSet =
    ccbs -- values2
  @Benchmark
  def bulkRemoveCcbs2(): cc.BitSet =
    ccbs &~ BitSet.fromArray(values2)
}

trait FoldIntoSci { self: DoSetup =>
  @Benchmark
  def foldIntoSci(): immutable.BitSet =
    values.foldLeft(immutable.BitSet.empty)(_ + _)
}

trait FoldIntoScm { self: DoSetup =>
  @Benchmark
  def foldIntoScm(): mutable.BitSet = {
    val b = mutable.BitSet.empty
    values.foreach(b += _)
    b
  }
}

trait FoldIntoIset { self: DoSetup =>
  @Benchmark
  def foldIntoIset(): immutable.Set[Int] =
    values.foldLeft(immutable.Set.empty[Int])(_ + _)
}

trait FoldIntoCcbs { self: DoSetup =>
  @Benchmark
  def foldIntoCcbs(): cc.BitSet =
    values.foldLeft(cc.BitSet.empty)(_ + _)
}

trait FoldOutOfSci { self: DoSetup =>
  @Benchmark
  def foldOutOfSci(): immutable.BitSet =
    values2.foldLeft(sci)(_ - _)
}

trait FoldOutOfScm { self: DoSetup =>
  @Benchmark
  def foldOutOfScm(): mutable.BitSet = {
    val b = scm.clone()
    values2.foreach(b -= _)
    b
  }
}

trait FoldOutOfIset { self: DoSetup =>
  @Benchmark
  def foldOutOfIset(): immutable.Set[Int] =
    values2.foldLeft(iset)(_ - _)
}

trait FoldOutOfCcbs { self: DoSetup =>
  @Benchmark
  def foldOutOfCcbs(): cc.BitSet =
    values2.foldLeft(ccbs)(_ - _)
}

trait LookupSci { self: DoSetup =>
  @Benchmark
  def lookupSci(): Int = indices.count(i => sci(i))
}

trait LookupScm { self: DoSetup =>
  @Benchmark
  def lookupScm(): Int = indices.count(i => scm(i))
}

trait LookupIset { self: DoSetup =>
  @Benchmark
  def lookupIset(): Int = indices.count(i => iset(i))
}

trait LookupCcbs { self: DoSetup =>
  @Benchmark
  def lookupCcbs(): Int = indices.count(i => ccbs(i))
}

trait MergeSci { self: DoSetup =>
  @Benchmark
  def mergeSci(): immutable.BitSet = sci | sci2
}

trait MergeScm { self: DoSetup =>
  @Benchmark
  def mergeScm(): mutable.BitSet = {
    val x = scm.clone
    x |= scm2
    x
  }
}

trait MergeIset { self: DoSetup =>
  @Benchmark
  def mergeIset(): immutable.Set[Int] = iset | iset2
}

trait MergeCcbs { self: DoSetup =>
  @Benchmark
  def mergeCcbs(): cc.BitSet = ccbs | ccbs2
}

trait IntersectSci { self: DoSetup =>
  @Benchmark
  def intersectSci(): immutable.BitSet = sci & sci2
}

trait IntersectScm { self: DoSetup =>
  @Benchmark
  def intersectScm(): mutable.BitSet = {
    val x = scm.clone
    x &= scm2
    x
  }
}

trait IntersectIset { self: DoSetup =>
  @Benchmark
  def intersectIset(): immutable.Set[Int] = iset & iset2
}

trait IntersectCcbs { self: DoSetup =>
  @Benchmark
  def intersectCcbs(): cc.BitSet = ccbs & ccbs2

  @Benchmark
  def intersectTestCcbs(): Boolean = ccbs intersects ccbs2
}

trait XorSci { self: DoSetup =>
  @Benchmark
  def xorSci(): immutable.BitSet = sci ^ sci2
}

trait XorScm { self: DoSetup =>
  @Benchmark
  def xorScm(): mutable.BitSet = {
    val x = scm.clone
    x ^= scm2
    x
  }
}

trait XorIset { self: DoSetup =>
  @Benchmark
  def xorIset(): immutable.Set[Int] =
    (iset | iset2) &~ (iset & iset2)
}

trait XorCcbs { self: DoSetup =>
  @Benchmark
  def xorCcbs(): cc.BitSet = ccbs ^ ccbs2
}

trait DiffSci { self: DoSetup =>
  @Benchmark
  def diffSci(): immutable.BitSet = sci &~ sci2
}

trait DiffScm { self: DoSetup =>
  @Benchmark
  def diffScm(): mutable.BitSet = {
    val x = scm.clone
    x &~= scm2
    x
  }
}

trait DiffIset { self: DoSetup =>
  @Benchmark
  def diffIset(): immutable.Set[Int] = iset &~ iset2
}

trait DiffCcbs { self: DoSetup =>
  @Benchmark
  def diffCcbs(): cc.BitSet = ccbs &~ ccbs2
}
