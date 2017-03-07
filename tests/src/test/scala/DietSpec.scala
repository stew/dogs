package dogs
package tests

import Predef._
import dogs.syntax.range._
import cats._
import cats.implicits._
import org.scalacheck._

class DietSpec extends DogsSuite {

  //  Properties("Diet")  with DogMatcher {

  // we'll pick 8 ranges between 0 and 1000,
  // 5 which we'll add, then 3 which we'll remove
  case class Ranges(a1: (Int, Int),
                    a2: (Int, Int),
                    a3: (Int, Int),
                    a4: (Int, Int),
                    a5: (Int, Int),
                    r1: (Int, Int),
                    r2: (Int, Int),
                    r3: (Int, Int)
                     ) {
    def in(i: Int)(r: (Int, Int)): Boolean = i >= r._1 && i <= r._2

    // test if we think any given I *should* be returns as in the Diet
    def contains(i: Int) = {
      val f = in(i) _
      (f(a1) | f(a2) | f(a3) | f(a4) | f(a5)) && !(f(r1) | f(r2) | f(r3))
    }

  def render: dogs.List[(Int,Int)] = {
    val adds = List(a1,a2,a3,a4,a5)
    val removes = List(r1,r2,r3)

    for {
      a <- adds
      r <- removes
      l <- (Range(a._1, a._2) - Range(r._1, r._2)) match {
        case None() => List.empty
        case Some((l, None())) => List((l.start, l.end))
        case Some((l, Some(r))) => List((l.start, l.end),(r.start, r.end))
      }
    } yield l
  }

  def min: dogs.Option[Int] = render.foldLeft(Option.none[Int])((b,a) =>
    b match {
      case None() => Some(a._1)
      case Some(x) => Some(x min a._1)
    })

  def max: dogs.Option[Int] = render.foldLeft(Option.none[Int])((b,a) =>
    b match {
      case None() => Some(a._2)
      case Some(x) => Some(x max a._2)
    })
  }

  def orderedTuple(x: Int, y: Int): (Int, Int) =
    if (Order[Int].compare(x, y) > 0)
      y -> x
    else
      x -> y

  implicit val arbNine = //: Arbitrary[Ranges] = Arbitrary(
    for {
      i1 <- Gen.choose(0, 9999)
      i2 <- Gen.choose(0, 9999)
      i3 <- Gen.choose(0, 9999)
      i4 <- Gen.choose(0, 9999)
      i5 <- Gen.choose(0, 9999)
      i6 <- Gen.choose(0, 9999)
      i7 <- Gen.choose(0, 9999)
      i8 <- Gen.choose(0, 9999)
      i9 <- Gen.choose(0, 9999)
      i10 <- Gen.choose(0, 9999)
      i11 <- Gen.choose(0, 9999)
      i12 <- Gen.choose(0, 9999)
      i13 <- Gen.choose(0, 9999)
      i14 <- Gen.choose(0, 9999)
      i15 <- Gen.choose(0, 9999)
      i16 <- Gen.choose(0, 9999)
    } yield Ranges(orderedTuple(i1, i2),
      orderedTuple(i3, i4),
      orderedTuple(i5, i6),
      orderedTuple(i7, i8),
      orderedTuple(i9, i10),
      orderedTuple(i11, i12),
      orderedTuple(i13, i14),
      orderedTuple(i15, i16))


  def fromRanges(r: Ranges): Diet[Int] = {
    def remove(d: Diet[Int], i: (Int, Int)): Diet[Int] = d - Range(i._1, i._2)

    val d = Diet.empty[Int].addRange(Range(r.a1._1, r.a1._2))
      .addRange(Range(r.a2._1, r.a2._2))
      .addRange(Range(r.a3._1, r.a3._2))
      .addRange(Range(r.a4._1, r.a4._2))
      .addRange(Range(r.a5._1, r.a5._2))

    remove(remove(remove(d, r.r1), r.r2), r.r3)
  }

  test("diet")(forAll(arbNine) { r =>
    val d = fromRanges(r)

    (0 to 1000).toList.foreach { i =>
      if (d.contains(i) != r.contains(i)) {
        println(s"for $i, got ${d.contains(i)} expected ${r.contains(i)}")
      }
    }
    (0 to 1000).toList.forall(i => {
      d.contains(i) == r.contains(i)
    })
    ()
  })

  test("min")(forAll(arbNine)(rs => rs.min ==  fromRanges(rs).min))
  test("max")(forAll(arbNine)(rs => rs.max ==  fromRanges(rs).max))

  test("merge")(forAll(arbNine, arbNine)  { (r1, r2) =>
    val d = Diet.empty[Int] ++ fromRanges(r1) ++ fromRanges(r2)

    (0 to 1000).toList.foreach {i =>
      if(d.contains(i) != (r1.contains(i) || r2.contains(i)))
        println(s"for $i, got ${d.contains(i)} expected ${r1.contains(i)} || ${r2.contains(i)}")
    }

    (0 to 1000).toList.forall(i => d.contains(i) == (r1.contains(i) || r2.contains(i) ))
    ()
  })


//  property("match") = forAll{ (r: Ranges) =>
//    val d = fromRanges(r)
//
//    val matcher = matchTo(d)
//
//    matcher.apply(d).matches == true
//  }
}
/*
class DietTest extends DogsSuite {
  import Diet._

  test("join nodes when item adj to existing seq"){
    val diet = Diet.empty[Int].add(5).add(6).add(1).add(3).add(2).add(8)

    val result = diet.intervals.map(l => l.generate)

    result should matchTo(List[List[Int]](List(1, 2, 3), List(5, 6), List(8)))
  }

  test("be always sorted"){
    val diet = Diet.empty[Int].add(5).add(6).add(1).add(3).add(2).add(8)

    val sorted = diet.toList()

    sorted should matchTo(List(1, 2, 3, 5, 6, 8))
  }

  test("add disjoint range"){
    val diet = Diet.empty[Int]

    val result = diet.addRange(Range(0, 100))

    val other = result.intervals.map(l => l.generate)

    other should matchTo(List(Range(0, 100).toList))
  }

  test("join disjoint range"){
    val diet = Diet.empty[Int] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    other.toList should matchTo(List(1, 2, 3, 4, 5, 6, 7))
  }

  test("contain items from range"){
    val diet = Diet.empty[Int].addRange(Range(5, 10)).addRange(Range(1, 3)).addRange(Range(12, 20))

    diet.contains(1) should be (true)
    diet.contains(2) should be (true)
    diet.contains(3) should be (true)

    diet.contains(4) should be (false)

    diet.contains(6) should be (true)

    diet.contains(15) should be (true)
  }

  test("return empty when removing from empty"){
    Diet.empty[Int].remove(1) should be (EmptyDiet())
  }

  test("not be modified when removing non existed item"){

    val diet = Diet.empty[Int] + 1 +2 + 3 + 5

    diet.remove(4) should be (diet)
  }

  test("be spl"){
    val diet = Diet.empty[Int] + 1 +2 + 3 + 5

    val other = diet.remove(2).intervals.map(x => x.generate.toScalaList).toScalaList

    other should contain  inOrder (scala.List(1), scala.List(3), scala.List(5))
  }

  test("map"){
    val diet = Diet.empty[Int] + 1 +2 + 8 + 5

    val other = diet.map(x => x + 2).intervals.map(x => x.generate)

    other should matchTo(List[List[Int]](List(3,4), List(7), List(10)))
  }

  test("foldLeft"){
    val diet = Diet.empty[Int] + 1 +2 + 8 + 5

    diet.foldLeft(10)(_ + _) should be (26)
    diet.foldRight(10)(_ + _) should be (26)
  }

  test("contain range"){
    val x = Diet.empty[Int] + Range(20, 30)

    x.containsRange(Range(20, 30)) should be (true)
    x.containsRange(Range(25, 26)) should be (true)
    x.containsRange(Range(1,10)) should be (false)

    val s = x + Range(10, 15)

    s.containsRange(Range(9, 15)) should be (false)
    s.containsRange(Range(10, 15)) should be (true)
    s.containsRange(Range(9, 16)) should be (false)
  }
}

class DietTestJoin extends DogsSuite {
  test("return the same diet when join to empty range"){
    val diet = Diet.empty[Int] + 20 + 30

    val range = Range.empty[Int]

    diet.addRange(range) should be (diet)
  }

  test("return a diet with range when added to empty diet"){
    val diet = Diet.empty[Int]

    val range = Range(20, 30)

    val other = diet.addRange(range)

    other.min should be (Some(20))
    other.max should be (Some(30))
  }

  test("increase range to the left"){
    val diet = Diet.empty[Int] + 20 + 21
    val range = Range(15, 19)

    val other = diet.addRange(range)

    other.intervals.toScalaList(0).generate should matchTo(List(15, 16, 17, 18, 19, 20, 21))
  }

  test("create disjoint range to the left"){
    val diet = Diet.empty[Int] + 20 + 21
    val range = Range(15, 18)

    val sets = diet.addRange(range).intervals.map(r=>r.generate).toScalaList

    sets(0) should matchTo(List(15, 16, 17, 18))
    sets(1) should matchTo(List(20, 21))
  }

  test("increase range to the right"){
    val diet = Diet.empty[Int] + 20 + 22
    val range = Range(21, 30)

    val other = diet.addRange(range).intervals.map(r => r.generate)

    other should matchTo(List(Range(20, 30).toList))
  }

  test("join to an empty diet"){
    val diet = Diet.empty[Int] + Range(20, 30)

    val other = diet ++ Diet.empty[Int]

    other should be (diet)
  }

  test("join to another diet"){
    val diet = Diet.empty[Int] + Range(20, 30)

    val other = diet ++ (Diet.empty[Int] + Range(25, 35) + Range(5, 10) + Range(15, 22))

    val sets = other.intervals.map(r => r.generate)

    sets should matchTo(List(Range(5, 10).toList, Range(15, 35).toList))

    val otherSets = diet | other

    otherSets.intervals.map(r => r.generate) should matchTo(List(Range(5, 10).toList, Range(15, 35).toList))
  }

  test("intersect with another diet"){
    val diet = Diet.empty[Int] + Range(20, 30)

    (diet & Diet.empty[Int]).intervals.toScalaList.length should be (0)

    (diet & diet) should be(diet)

    (diet & (Diet.empty[Int] + Range(15, 25) + Range(28, 32))).toList should
      matchTo(List (20, 21, 22, 23, 24, 25, 28, 29, 30))

    (diet & (Diet.empty[Int] + Range(10, 15))).toList should matchTo(El[Int])
  }
}

class DietTestRemove extends DogsSuite {
  import Diet._

  test("return empty when removing from empty"){

    (Diet.empty[Int] - Range(10, 100)) should be (EmptyDiet())
  }

  test("remove inner range"){
    val diet = ((Diet.empty[Int] + Range(20, 30)) - Range(22, 27))

    diet.toList() should matchTo(List(20, 21, 28, 29, 30))
  }

  test("remove side ranges"){
    val diet = ((Diet.empty[Int]
      + Range(20, 21)
      + Range(9,10)
      + Range(12, 18)
      + Range(23, 30)
      + Range(40, 50)
      + Range(35, 48)
      + Range(55, 60))
      - Range(15, 18)
      - Range(25, 60))

    diet.toList() should matchTo(List(9, 10, 12, 13, 14, 20, 21, 23, 24))
  }
}

class DietTestShow extends DogsSuite {

  import Diet._
  import cats.implicits._

  test("shown empty"){
    val diet = Diet.empty[Int]

    diet.show should be ("{}")
  }

  test("shown all intervals"){
    val diet = Diet.empty[Int] + Range(1, 10) + Range(20, 100)

    diet.show should be ("{[1, 10], [20, 100]}")
  }
}
 */
