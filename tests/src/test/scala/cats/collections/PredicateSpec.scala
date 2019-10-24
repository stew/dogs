package cats.collections
package tests

import cats.laws.discipline.SerializableTests
import cats._
import cats.tests.CatsSuite

class PredicateSpec extends CatsSuite {

  checkAll("MonoidK[Predicate]", SerializableTests.serializable(MonoidK[Predicate]))

  test("intersection works")(
    forAll { (as: List[Int], bs: List[Int]) =>

      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA & setEven
      val s2 = setA intersection setEven

      bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 == 0))) &&
                  (s2(b) == (as.contains(b) && (b % 2 == 0)))) should be(true)

    })

  test("union works")(
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA | setEven
      val s2 = setA union setEven

      bs.forall(b => (s1(b) == (as.contains(b) || (b % 2 == 0))) &&
                  (s2(b) == (as.contains(b) || (b % 2 == 0)))) should be (true)

    })

  test("difference works") (
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA - setEven
      val s2 = setA diff setEven

      bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 != 0))) &&
                  (s2(b) == (as.contains(b) && (b % 2 != 0)))) should be (true)

    })

  test("negation works")(
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = !(setA - setEven)

      bs.forall(b => (s1(b) != (as.contains(b) && (b % 2 != 0)))) should be(true)

    })
}
