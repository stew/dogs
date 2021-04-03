package cats.collections
package bench

import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import cats._
import cats.implicits._

@State(Scope.Benchmark)
class ChainedPredicateBench {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var pred: Predicate[Int] = _

  @Setup
  def setup: Unit = {
    pred = Predicate(_ == 0)
    pred = Iterator.iterate(pred.negate)(_ - pred).drop(n).next()
  }

  @Benchmark
  def catsCollectionsPredicateUnravel: Unit = {
    pred.contains(0)
  }
}
