package cats.collections
package tests

import cats.{Order, Show}
import cats.collections.laws.discipline.PartiallyOrderedSetTests
import cats.kernel.laws.discipline.OrderTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}

class HeapSpec extends CatsSuite {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration.copy(
      minSuccessful = 1000
    )

  def heapGen[A: Order](size: Int, agen: Gen[A]): Gen[Heap[A]] = {
    val listA = Gen.listOfN(size, agen)
    val startWith1 =
      listA.map {
        case Nil => Heap.empty[A]
        case h :: tail => tail.foldLeft(Heap(h))(_.add(_))
      }
    val addOnly = listA.map(_.foldLeft(Heap.empty[A])(_.add(_)))
    val heapify = listA.map(Heap.fromIterable(_))
    // This one is recursive and with small probability can get quite deep
    val addMoreAndRemove: Gen[Heap[A]] =
      for {
        extraSize <- Gen.choose(1, size + 1)
        withExtra <- Gen.lzy(heapGen[A](size + extraSize, agen))
      } yield (0 until extraSize).foldLeft(withExtra) { (h, _) => h.remove }
    // we can also make smaller one and add to it:
    val smallerAdd =
      if (size > 0) {
        for {
          a <- agen
          heap <- heapGen(size - 1, agen)
        } yield heap + a
      }
      else Gen.const(Heap.empty[A])

    Gen.frequency(
      (2, addOnly),
      (3, startWith1),
      (5, heapify),
      (1, addMoreAndRemove),
      (1, smallerAdd))
  }

  implicit def arbHeap[A: Arbitrary: Order]: Arbitrary[Heap[A]] =
    Arbitrary {
      Gen.sized(heapGen[A](_, Arbitrary.arbitrary[A]))
    }

  implicit def cogenHeap[A: Cogen: Order]: Cogen[Heap[A]] =
    Cogen[List[A]].contramap { (h: Heap[A]) => h.toList }

  checkAll("PartiallyOrderedSet[Heap]",
    PartiallyOrderedSetTests[Heap].partiallyOrderedSet[Long, Int])

  checkAll("Order[Heap[Int]]", OrderTests[Heap[Int]].order)

  test("sorted")(
    forAll { (list: List[Int]) =>

      val heap = list.foldLeft(Heap.empty[Int])((h, i) => h.add(i))

      heap.toList should be(list.sorted)

    })

  test("heapify is sorted") {
    forAll { (list: List[Int]) =>
      val heap = Heap.heapify(list)
      val heapList = heap.toList
      val heap1 = Heap.heapify(heapList)

      assert(heapList == list.sorted)
      assert(Order[Heap[Int]].eqv(heap, heap1))
    }
  }

  test("adding increases size") {
    forAll { (heap: Heap[Int], x: Int) =>
      val heap1 = heap + x
      assert(heap1.size == (heap.size + 1))
    }
  }

  test("add is the same as +") {
    forAll { (heap: Heap[Int], x: Int) =>
      val heap1 = heap + x
      val heap2 = heap.add(x)
      assert(Order[Heap[Int]].eqv(heap1, heap2))
    }
  }

  test("addAll is the same as ++") {
    forAll { (heap: Heap[Int], x: List[Int]) =>
      val heap1 = heap ++ x
      val heap2 = heap.addAll(x)
      assert(Order[Heap[Int]].eqv(heap1, heap2))
    }
  }

  test("addAll is the same as folding with add") {
    forAll { (heap: Heap[Int], x: List[Int]) =>
      val heap1 = heap.addAll(x)
      val heap2 = x.foldLeft(heap)(_.add(_))
      assert(Order[Heap[Int]].eqv(heap1, heap2))
    }
  }

  test("remove decreases size") {
    forAll { (heap: Heap[Int]) =>
      val heap1 = heap.remove
      assert((heap1.size == (heap.size - 1)) || (heap1.isEmpty && heap.isEmpty))
    }

    assert(Heap.empty[Int].remove == Heap.empty[Int])
  }

  test("pop and remove return the same heap") {
    forAll { (heap: Heap[Int]) =>
      val heap1 = heap.remove
      heap.pop.map(_._2) match {
        case Some(heap2) => assert(Order[Heap[Int]].eqv(heap1, heap2))
        case None => assert(heap1.isEmpty)
      }
    }
  }

  test("pop returns the minimum element") {
    forAll { (heap: Heap[Int]) =>
      val min1 = heap.pop.map(_._1)
      val min2 = heap.minimumOption
      assert(min1 == min2)
    }
  }

  test("size is consistent with isEmpty/nonEmpty") {
    forAll { (heap: Heap[Int]) =>
      assert(heap.isEmpty == (heap.size == 0))
      assert(heap.nonEmpty == (heap.size > 0))
      assert(heap.isEmpty == (!heap.nonEmpty))
    }
  }

  test("height is <= log_2 N + 1 for all heaps") {
    forAll { (heap: Heap[Int]) =>
      val bound = math.log(heap.size.toDouble) / math.log(2.0) + 1.0
      assert(heap.isEmpty || heap.height.toDouble <= bound)
    }
  }

  test("heapify is the same as adding") {
    forAll { (init: List[Int]) =>
      val heap1 = Heap.fromIterable(init)
      val heap2 = init.foldLeft(Heap.empty[Int])(_.add(_))
      assert(heap1.toList == heap2.toList)
    }
  }

  test("minimumOption after removing one is >= before") {
    forAll { (heap: Heap[Int]) =>
      val min0 = heap.minimumOption
      val min1 = heap.remove.minimumOption

      (min0, min1) match {
        case (None, next) => assert(next.isEmpty)
        case (_, None) => assert(heap.size == 1)
        case (Some(m0), Some(m1)) =>
          assert(m0 <= m1)
      }
    }
  }

  test("Heap.minimumOption is the real minimum") {
    def heapLaw(heap: Heap[Int]) =
      heap.minimumOption match {
        case None => assert(heap.isEmpty)
        case Some(min) =>
          val heap1 = heap.remove
          assert(heap1.isEmpty || {
            min <= heap1.toList.min
          })
      }

    forAll { (heap: Heap[Int]) =>
      heapLaw(heap)
      // even after removing this is true
      heapLaw(heap.remove)
      heapLaw(heap.remove.remove)
      heapLaw(heap.remove.remove.remove)
    }

    assert(Heap.empty[Int].minimumOption.isEmpty)
  }

  test("Heap.foldLeft is consistent with toList.foldLeft") {
    forAll { (heap: Heap[Int], init: Long, fn: (Long, Int) => Long) =>
      assert(heap.foldLeft(init)(fn) == heap.toList.foldLeft(init)(fn))
    }
  }

  test("Show[Heap[Int]] works like toList.mkString") {
    forAll { (heap: Heap[Int]) =>
      assert(Show[Heap[Int]].show(heap) == heap.toList.mkString("Heap(", ", ", ")"))
    }
  }

  test("Order[Heap[Int]] works like List[Int]") {
    forAll { (a: Heap[Int], b: Heap[Int]) =>
      assert(Order[Heap[Int]].compare(a, b) == Order[List[Int]].compare(a.toList, b.toList))
    }
  }

  test("Heap.exists is correct") {
    forAll { (a: Heap[Int], fn: Int => Boolean) =>
      assert(a.exists(fn) == a.toList.exists(fn))
    }
  }

  test("Heap.forall is correct") {
    forAll { (a: Heap[Int], fn: Int => Boolean) =>
      assert(a.forall(fn) == a.toList.forall(fn))
    }
  }

  test("Heap.empty is less than nonEmpty") {
    forAll { (item: Int, heap: Heap[Int]) =>
      val ord = Order[Heap[Int]]
      assert(ord.lteqv(Heap.empty, heap))
      assert(ord.lt(Heap.empty, Heap.empty + item))
      assert(ord.gteqv(heap, Heap.empty))
      assert(ord.gt(Heap.empty + item, Heap.empty))
    }
  }

  test("takeLargest is the same as sort.reverse.take") {
    forAll { (as: Iterable[Int], k: Int) =>
      assert(Heap.takeLargest(as, k).toList.reverse == as.toList.sorted.reverse.take(k))
    }
  }

  test("Heap.toPairingHeap.toList == Heap.toList") {
    forAll { (h: Heap[Int]) =>
      assert(h.toPairingHeap.toList == h.toList)
    }
  }

  test("Heap property is always maintained") {
    def law[A](h: Heap[A], outer: Heap[A])(implicit ord: Order[A]): Unit =
      h match {
        case Heap.Leaf() => ()
        case Heap.Branch(min, left, right) =>
          lazy val heapStr = if (outer != h) s"in $outer, subheap $h" else h.toString
          left.getMin.foreach { m => assert(ord.gteqv(m, min), s"$heapStr violates heap order property on left") }
          right.getMin.foreach { m => assert(ord.gteqv(m, min), s"$heapStr violates heap order property on right") }
          // we expect fully balanced heaps, but we put the size on the left first:
          assert(left.size >= right.size,
            s"$heapStr has left size = ${left.size} vs right size = ${right.size}")
          assert((left.height == right.height) || (left.height == right.height + 1),
            s"$heapStr has unbalanced height: ${left.height} vs ${right.height}")

          law(left, outer)
          law(right, outer)
      }

    forAll { (h: Heap[Int]) =>
      law(h, h)
    }
  }

  test("toIterator maintains order")(forAll { heap: Heap[Int] =>
    val numbers = heap.toIterator.toList

    numbers should contain inOrderElementsOf (numbers.sorted)
  })

  test("toIterator maintains the inverse order")(forAll { xs: List[Int] =>
    whenever(xs.size > 1) {
      val order = Order.from[Int]((a, b) => a.compareTo(b) * -1)
      val inverseOrder = Order.from[Int]((a, b) => a.compareTo(b))

      val heap = Heap.fromIterable(xs)(order)

      val it = heap.toIterator(order)
      var min = it.next()

      while (it.hasNext) {
        val current = it.next()

        order.gteqv(current, min) should be (true)
        inverseOrder.lteqv(current, min) should be (true)

        min = current
      }
    }
  })

  test("toIterator should match with toList")(forAll {heap: Heap[Int] =>
    heap.toIterator.toList === heap.toList
  })
}
