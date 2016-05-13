package dogs
package tests

import Predef._
import dogs.tests.arbitrary._
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._
import cats.std.int._
import scala.{annotation}
import cats._
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests, CartesianTests}

class DequeueSpec extends SlowDogsSuite with ArbitraryList with ArbitraryOption {
  import Option._
  import Dequeue._

  checkAll("Dequeue[Int]", CartesianTests[Dequeue].cartesian[Int, Int, Int])
  checkAll("Cartesian[Dequeue]", SerializableTests.serializable(Cartesian[Dequeue]))

  checkAll("Dequeue[Int]", CoflatMapTests[Dequeue].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Dequeue]", SerializableTests.serializable(CoflatMap[Dequeue]))

  checkAll("Dequeue[Int]", MonadCombineTests[Dequeue].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Dequeue]", SerializableTests.serializable(MonadCombine[Dequeue]))

  checkAll("Dequeue[Int] with Option", TraverseTests[Dequeue].traverse[Int, Int, Int, Dequeue[Int], Option, Option])
  checkAll("Traverse[Dequeue]", SerializableTests.serializable(Traverse[Dequeue]))

   @annotation.tailrec
   final def consL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
     case El() => q
     case Nel(x,xs) => consL(xs, q cons x)
   }
 
   @annotation.tailrec
   final def unconsL[A](q: Dequeue[A], acc: List[A]): List[A] = q uncons match {
     case None() => acc
     case Some((i, q)) => unconsL(q, i :: acc)
   }
 
   @annotation.tailrec
   final def snocL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
     case El() => q
     case Nel(x,xs) => snocL(xs, q snoc x)
   }
 
   @annotation.tailrec
   final def unsnocL[A](q: Dequeue[A], acc: List[A]): List[A] = q unsnoc match {
     case None() => acc
     case Some((i, q)) => unsnocL(q, i :: acc)
   }

  test("enqueue onto an empty q can be fetched from either end"){
    val x = "xyzzy"
    val q = Dequeue.empty cons x

    q.uncons should be(Some((x,EmptyDequeue())))
    q.unsnoc should be(Some((x,EmptyDequeue())))
  }

  test("cons and then uncons")(forAll { (xs: List[Int]) =>
    val q = consL(xs, Dequeue.empty)
    val l = unconsL(q, List.empty)

    xs should be (l)
  })

  test("snoc and then unsnoc")(forAll { (xs: List[Int]) =>
    val q = snocL(xs, Dequeue.empty)
    val l = unsnocL(q, List.empty)

    xs should be(l)
  })

  test("cons and then unsnoc")(forAll { (xs: List[Int]) =>
    val q = consL(xs, Dequeue.empty)
    val l = unsnocL(q, List.empty)

    xs should be(l.reverse)
  })

  test("snoc and then uncons")(forAll { (xs: List[Int]) =>
    val q = snocL(xs, Dequeue.empty)
    val l = unconsL(q, List.empty)

    xs should be(l.reverse)
  })

  implicit def genQ[A: Arbitrary]: Arbitrary[Dequeue[A]] = Arbitrary(
    for {
      l <- getArbitrary[List[A]]
      r <- getArbitrary[List[A]]
    } yield consL(l, snocL(r, Dequeue.empty)))

  test("foldLeft")(forAll{ (q: Dequeue[Int]) =>
    q.foldLeft[List[Int]](List.empty)((xs,x) => Nel(x, xs)) should be (q.toBackStream.toList)
  })

  test("foldRight")(forAll { (q: Dequeue[Int]) =>
    q.foldRight[List[Int]](Eval.now(List.empty))((x,xs) => xs.map(xs => Nel(x,xs))).value should be (q.toStreaming.toList)
  })


}


