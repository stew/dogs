package dogs
package tests

import Predef._
import cats._
import cats.implicits._
import dogs.tests.arbitrary._

class MapSpec extends DogsSuite with ArbitraryList {
  import scala.collection.immutable.{Set => SSet, Map => MMap}

  def fromSet[K: Order,V](s: SSet[(K,V)]): Map[K,V] =
    s.foldLeft[Map[K,V]](Map.empty)(_ + _)

  def fromSetS[K: Order,V](s: SSet[(K,V)]): MMap[K,V] =
    s.foldLeft[MMap[K,V]](MMap.empty)(_ + _)

  def toSet[K: Order,V](m: Map[K,V]): SSet[(K,V)] =
    m.foldLeft[SSet[(K,V)]](SSet.empty)(_ + _)

  test("we can add things to a Map, then find them")(forAll {(xs: SSet[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSetS(xs)

    xs.forall {
      case (k,v) => m.containsKey(k) && (m.get(k).toScalaOption == m2.get(k))
    } should be (true)
  })

  test("we can add things to a Map, then remove them")(forAll {(xs: SSet[((String,Int), Boolean)]) =>
    val n = fromSet(xs.map(_._1))
    val n2 = fromSetS(xs.map(_._1))

    val m = xs.foldLeft[Map[String,Int]](n)((mm,kvr) =>
      if(kvr._2) mm.remove(kvr._1._1) else mm
    )

    val m2 = xs.foldLeft[MMap[String,Int]](n2)((mm,kvr) =>
      if(kvr._2) mm - (kvr._1._1) else mm
    )

    xs.forall {
      case (k,v) =>
        m.get(k._1).toScalaOption == m2.get(k._1)
    } should be (true)
  })

  test("we can combine maps")(forAll {(xs: SSet[(String,Int)],xs2: SSet[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSet(xs2)

    val sm = fromSetS(xs)
    val sm2 = fromSetS(xs2)

    toSet(m ++ m2) should contain theSameElementsAs (sm ++ sm2).to[SSet]
  })

  test("map works")(forAll {(xs: SSet[(String,Int)]) =>
    val f: Int => Int = _ + 1
    val f2: ((String,Int)) => (String,Int) = kv => kv._1 -> (kv._2 + 1)
    val m = fromSet(xs)
    val sm = fromSetS(xs)

    toSet(m map f) should contain theSameElementsAs (sm map f2).to[SSet]
  })
}

class MapShow extends DogsSuite {

  test("show empty") {
    val map = Map.empty[Int, Int]

    map.show should be("{}")
  }

  test("show mappings") {
    val map = Map.empty[Int, Int].+((1, 2)).+((2, 3))

    map.show should be("{[1-->2]\n[2-->3]\n}")
  }
}
