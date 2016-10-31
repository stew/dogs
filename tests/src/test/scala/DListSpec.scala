package dogs
package tests

import Predef._
import dogs.tests.arbitrary._
import dogs.tests.arbitrary.cogen._
import cats._
import cats.implicits._
import cats.laws.discipline._
import cats.kernel.laws._
import org.scalacheck._
import Cogen._
import catalysts.Platform

class DListSpec extends SlowDogsSuite with ArbitraryList with ArbitraryDList with ArbitraryOption {
  import DList._

  checkAll("DList[Int]", GroupLaws[DList[Int]].monoid)
  checkAll("DList[Int]", OrderLaws[DList[Int]].eqv)

  checkAll("Traverse[DList]", TraverseTests[DList].traverse[Int, Int, Int, DList[Int], Option, Option])
  checkAll("Traverse[DList]", SerializableTests.serializable(Traverse[DList]))

  test("headOption")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.headOption should be(l.headOption)
  })

  test("tailOption")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.tailOption.map(_.toList) should be (l.tailOption)
  })

  test("isEmpty")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.isEmpty should be(l.isEmpty)
  })

  test("foldRight")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.foldRight[List[Int]](Eval.now(List.empty))((l,a) => a.map(l :: _)).value should be (l)
  })

  test("map")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.map(_ + 1).toList should be (l.map(_ + 1))
  })

  test("stack safe append"){
    val fill = if(Platform.isJvm) 100000 else 1000
    val dl = List.fill(fill)(1).foldLeft[DList[Int]](DList.empty)((dl,i) =>
      dl ++ DList(List(i))
    )

    dl.headOption should be (Some(1))
  }
  test("stack safe post"){
    val fill = if(Platform.isJvm) 100000 else 1000
    val dl = List.fill(fill)(1).foldLeft[DList[Int]](DList.empty)(_ :+ _)
    dl.headOption should be (Some(1))
  }


  test("stack safe pre") {
    val fill = if(Platform.isJvm) 100000 else 1000
    val dl = List.fill(fill)(1).foldLeft[DList[Int]](DList.empty)((dl,a) => a +: dl)
    dl.headOption should be(Some(1))
  }

}
