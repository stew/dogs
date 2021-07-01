/**
 * Created by nperez on 3/28/16.
 */

package cats.collections

import cats.{Order, Show}
import cats.kernel.CommutativeMonoid
import scala.annotation.tailrec

/**
 * `Heap` is a Purely Functional Binary Heap. Binary Heaps are not common in the functional space, especially because
 * their implementation depends on mutable arrays in order to gain in performance. This functional binary heap is based
 * on [[https://arxiv.org/pdf/1312.4666.pdf Vladimir Kostyukov's paper]] and it does support the basic operations on a heap without compromising performance.
 *
 * It is important to note that we can, in fact, to create the Binary Heap in order O(n) from a `List` using the
 * function `heapify`.
 */
sealed abstract class Heap[A] {

  import Heap._

  /**
   * Returns min value on the heap, if it exists
   */
  final def getMin: Option[A] = minimumOption

  /**
   * Returns min value on the heap, if it exists
   */
  def minimumOption: Option[A]

  // this is size < 2^height - 1
  // this is always false for empty, and sometimes false for non-empty
  private[collections] def unbalanced: Boolean

  /**
   * Returns the size of the heap.
   */
  def size: Long

  /**
   * Returns the height of the heap.
   */
  def height: Int

  /**
   * Verifies if the heap is empty.
   */
  def isEmpty: Boolean

  /**
   * Return true if this is not empty
   */
  def nonEmpty: Boolean = !isEmpty

  /**
   * Insert a new element into the heap.
   * Order O(log n)
   */
  def add(x: A)(implicit order: Order[A]): Heap[A] =
    if (isEmpty) Heap(x)
    else {
      // this is safe since we are non-empty
      val branch = this.asInstanceOf[Branch[A]]
      import branch.{min, left, right}
      if (left.unbalanced)
        bubbleUp(min, left.add(x), right)
      else if (right.unbalanced)
        bubbleUp(min, left, right.add(x))
      else if (right.height < left.height)
        bubbleUp(min, left, right.add(x))
      else
        bubbleUp(min, left.add(x), right)
    }

  /*
   * Add a collection of items in. This is O(N log N) if as is size N
   */
  def addAll(as: Iterable[A])(implicit order: Order[A]): Heap[A] = {
    val ait = as.iterator
    var heap = this
    while(ait.hasNext) {
      heap = heap + ait.next()
    }
    heap
  }

  /**
   * This is O(N) in the worst case, but we use the
   * heap property to be lazy
   */
  def contains(a: A)(implicit order: Order[A]): Boolean =
    if (isEmpty) false
    else {
      val br = this.asInstanceOf[Branch[A]]
      val c = order.compare(a, br.min)
      if (c < 0) false // a is less than the min
      else if (c == 0) true // a == min
      else {
        // check left and right
        br.left.contains(a) || br.right.contains(a)
      }
    }

  /**
   * Check to see if a predicate is ever true
   */
  def exists(fn: A => Boolean): Boolean =
    this match {
      case Branch(a, l, r) => fn(a) || l.exists(fn) || r.exists(fn)
      case _ => false
    }

  /**
   * Check to see if a predicate is always true
   */
  def forall(fn: A => Boolean): Boolean =
    this match {
      case Branch(a, l, r) => fn(a) && l.forall(fn) && r.forall(fn)
      case _ => true
    }

  /**
   * Avoid this, it should really have been on the companion because
   * this totally ignores `this`.
   */
  @deprecated("this method ignores `this` and is very easy to misuse. Use Heap.fromIterable", "0.8.0")
  def heapify(a: List[A])(implicit order: Order[A]): Heap[A] =
    Heap.heapify(a)

  def toIterator(implicit order: Order[A]): Iterator[A] = {
    @scala.annotation.tailrec
    def build(heap: Heap[A], acc: Stream[A]): Stream[A] = heap match {
      case Leaf() => acc
      case b@Branch(_, _, _) => b.pop match {
        case Some((m, h)) => build(h, acc.append(Seq(m)))
        case None => acc
      }
    }

    build(this, Stream.empty).toIterator
  }

  /**
   * Remove the min element from the heap (the root) and return it along with the updated heap.
   * Order O(log n)
   */
  def pop(implicit order: Order[A]): Option[(A, Heap[A])] = this match {
    case Branch(m, l, r) => Some((m, bubbleRootDown(mergeChildren(l, r))))
    case Leaf()          => None
  }

  /**
   * Remove the min element from the heap (the root).
   * Order O(log n)
   */
  def remove(implicit order: Order[A]): Heap[A] = this match {
    case Branch(_, l, r) => bubbleRootDown(mergeChildren(l, r))
    case Leaf()          => Leaf()
  }

  /**
   * Aggregate with a commutative monoid, since the Heap is not totally
   * ordered
   */
  final def unorderedFoldMap[B](fn: A => B)(implicit m: CommutativeMonoid[B]): B =
    this match {
      case Branch(min, left, right) =>
        // This recursion is safe because the trees have depth ~ log(size)
        m.combine(fn(min), m.combine(left.unorderedFoldMap(fn), right.unorderedFoldMap(fn)))
      case _ => m.empty
    }

  /**
   * Similar to unorderedFoldMap without a transformation
   */
  final def unorderedFold(implicit m: CommutativeMonoid[A]): A =
    this match {
      case Branch(min, left, right) => m.combine(min, m.combine(left.unorderedFold, right.unorderedFold))
      case _ => m.empty
    }

  /**
   * Returns a sorted list of the elements within the heap.
   */
  def toList(implicit order: Order[A]): List[A] = toIterator.toList

  /**
   * do a foldLeft in the same order as toList.
   * requires an Order[A], which prevents us from making a Foldable[Heap] instance.
   *
   * prefer unorderedFoldMap if you can express your operation as a commutative monoid
   * since it is O(N) vs O(N log N) for this method
   */
  def foldLeft[B](init: B)(fn: (B, A) => B)(implicit order: Order[A]): B = {
    @tailrec
    def loop(h: Heap[A], init: B): B =
      h match {
        case Branch(a, _, _) => loop(h.remove, fn(init, a))
        case Leaf()          => init
      }

    loop(this, init)
  }

  /**
   * Alias for add
   */
  def +(x: A)(implicit order: Order[A]): Heap[A] = add(x)

  /**
   * Alias for addAll
   */
  def ++(as: Iterable[A])(implicit order: Order[A]): Heap[A] = addAll(as)

  /**
   * Alias for remove
   */
  def --(implicit order: Order[A]): Heap[A] = remove

  /**
   * convert to a PairingHeap which can do fast merges,
   * this is an O(N) operation
   */
  def toPairingHeap: PairingHeap[A] =
    if (isEmpty) PairingHeap.empty
    else {
      val thisBranch = this.asInstanceOf[Branch[A]]
      import thisBranch.{min, left, right}
      PairingHeap.Tree(min, left.toPairingHeap :: right.toPairingHeap :: Nil)
    }
}

object Heap {

  def empty[A]: Heap[A] = Leaf()

  def apply[A](x: A): Heap[A] = Branch(x, empty, empty)

  // This is private since it allows you to create Heaps that violate the invariant
  // that min has a minimum value
  private def apply[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] =
    Branch(x, l, r)

  /**
   * alias for heapify
   */
  def fromIterable[A](as: Iterable[A])(implicit order: Order[A]): Heap[A] =
    heapify(as)

  /**
   * this is useful for finding the k maximum values in O(N) times for N items
   * same as as.toList.sorted.reverse.take(count), but O(N log(count)) vs O(N log N)
   * for a full sort. When N is very large, this can be a very large savings
   */
  def takeLargest[A](as: Iterable[A], count: Int)(implicit order: Order[A]): Heap[A] =
    if (count <= 0) empty
    else {
      var heap = empty[A]
      val iter = as.iterator
      while (iter.hasNext) {
        val a = iter.next()
        heap =
          if (heap.size < count) heap + a
          else if (order.lt(heap.asInstanceOf[Branch[A]].min, a)) heap.remove + a
          else heap
      }

      heap
    }

  /**
   * Build a heap using an Iterable
   * Order O(n)
   */
  def heapify[A](a: Iterable[A])(implicit order: Order[A]): Heap[A] = {
    val ary = (a: Iterable[Any]).toArray
    def loop(i: Int): Heap[A] =
      if (i < ary.length) {
        // we only insert A values, but we don't have a ClassTag
        // so we can't create an array of type A.
        // But since A was already boxed, and needs to be boxed in Heap
        // this shouldn't cause a performance problem
        bubbleDown(ary(i).asInstanceOf[A], loop((i << 1) + 1), loop((i + 1) << 1))
      }
      else {
        Leaf()
      }

    loop(0)
  }

  private[collections] case class Branch[A](min: A, left: Heap[A], right: Heap[A]) extends Heap[A] {
    override val size = left.size + right.size + 1L

    override val height = scala.math.max(left.height, right.height) + 1

    override def isEmpty: Boolean = false

    override def minimumOption: Option[A] = Some(min)

    override def unbalanced: Boolean = size < (1L << height) - 1L
  }

  private[collections] case object Leaf extends Heap[Nothing] {
    def apply[A](): Heap[A] = this.asInstanceOf[Heap[A]]

    def unapply[A](heap: Heap[A]): Boolean = heap.isEmpty

    override def size: Long = 0L

    override def height: Int = 0

    // 0 < 2^0 - 1, or 0 < 0, which is false
    override def unbalanced: Boolean = false

    override def isEmpty: Boolean = true

    override def minimumOption: Option[Nothing] = None
  }

  private[collections] def bubbleUp[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt), _) if order.gt(x, y) => Heap(y, Heap(x, lt, rt), r)
    case (_, Branch(z, lt, rt)) if order.gt(x, z) => Heap(z, l, Heap(x, lt, rt))
    case (_, _)                                   => Heap(x, l, r)
  }

  private[collections] def bubbleDown[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, _, _), Branch(z, lt, rt)) if (order.lt(z, y) && order.gt(x, z)) => Heap(z, l, bubbleDown(x, lt, rt))
    case (Branch(y, lt, rt), _) if order.gt(x, y)                                   => Heap(y, bubbleDown(x, lt, rt), r)
    case (_, _)                                                                     => Heap(x, l, r)
  }

  private[collections] def bubbleRootDown[A](h: Heap[A])(implicit order: Order[A]): Heap[A] =
    h match {
      case Branch(min, left, right) => bubbleDown(min, left, right)
      case Leaf()                   => Leaf()
    }

  /*
   * This implementation uses what is effectively flow typing which is
   * hard to efficiently encode in scala, therefore, we instead include
   * proofs (informal ones) as to why the casts inside here are safe
   */
  private[collections] def mergeChildren[A](l: Heap[A], r: Heap[A]): Heap[A] =
    if (l.isEmpty && r.isEmpty) {
      Leaf()
    }
    else if (l.unbalanced) {
      // empty Heaps are never unbalanced, so we can cast l to a branch:
      val bl: Branch[A] = l.asInstanceOf[Branch[A]]
      floatLeft(bl.min, mergeChildren(bl.left, bl.right), r)
    }
    else if (r.unbalanced) {
      // empty Heaps are never unbalanced, so we can cast r to a branch:
      val br: Branch[A] = r.asInstanceOf[Branch[A]]
      floatRight(br.min, l, mergeChildren(br.left, br.right))
    }
    else if (r.height < l.height) {
      // l.height >= 1, because r.height >= 0, so, l must be a branch
      val bl: Branch[A] = l.asInstanceOf[Branch[A]]
      floatLeft(bl.min, mergeChildren(bl.left, bl.right), r)
    }
    else {
      // we know r.height >= l.height,
      // we also know both r and l are not empty.
      // since l and r are not both empty, if r is empty,
      // then l is not, but then r.height == 0 >= (some number > 0),
      // which is false, so this implies r must be a branch
      val br: Branch[A] = r.asInstanceOf[Branch[A]]
      floatRight(br.min, l, mergeChildren(br.left, br.right))
    }

  private[collections] def floatLeft[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] = l match {
    case Branch(y, lt, rt) => Heap(y, Heap(x, lt, rt), r)
    case _                 => Heap(x, l, r)
  }

  private[collections] def floatRight[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] = r match {
    case Branch(y, lt, rt) => Heap(y, l, Heap(x, lt, rt))
    case _                 => Heap(x, l, r)
  }

  implicit def toShowable[A](implicit s: Show[A], order: Order[A]): Show[Heap[A]] = new Show[Heap[A]] {
    override def show(f: Heap[A]): String = {
      val sb = new java.lang.StringBuilder
      sb.append("Heap(")
      f.foldLeft(false) { (notFirst, a) =>
        if (notFirst) sb.append(", ")
        sb.append(s.show(a))
        true
      }
      sb.append(")")
      sb.toString
    }
  }

  implicit val catsCollectionHeapPartiallyOrderedSet: PartiallyOrderedSet[Heap] =
    new PartiallyOrderedSet[Heap] {
      def unorderedFoldMap[A, B: CommutativeMonoid](ha: Heap[A])(fn: A => B): B =
        ha.unorderedFoldMap(fn)

      override def unorderedFold[A: CommutativeMonoid](ha: Heap[A]): A =
        ha.unorderedFold

      override def isEmpty[A](h: Heap[A]) = h.isEmpty
      override def nonEmpty[A](h: Heap[A]) = h.nonEmpty
      override def exists[A](ha: Heap[A])(fn: A => Boolean) = ha.exists(fn)
      override def forall[A](ha: Heap[A])(fn: A => Boolean) = ha.forall(fn)
      override def size[A](h: Heap[A]) = h.size

      // PartiallyOrderedSet methods
      override def add[A](fa: Heap[A], a: A)(implicit order: Order[A]): Heap[A] =
        fa.add(a)
      override def addAll[A: Order](fa: Heap[A], as: Iterable[A]): Heap[A] =
        fa.addAll(as)
      override def contains[A](fa: Heap[A], a: A)(implicit order: Order[A]): Boolean =
        fa.contains(a)
      override def build[A](as: Iterable[A])(implicit order: Order[A]): Heap[A] =
        Heap.fromIterable(as)
      override def empty[A]: Heap[A] = Heap.empty[A]
      override def minimumOption[A](fa: Heap[A]): Option[A] = fa.getMin
      override def removeMin[A](fa: Heap[A])(implicit order: Order[A]): Heap[A] = fa.remove
      override def singleton[A](a: A): Heap[A] = Heap(a)
      override def toSortedList[A: Order](fa: Heap[A]): List[A] =
        fa.toList
      override def sortedFoldLeft[A: Order, B](fa: Heap[A], init: B)(fn: (B, A) => B): B =
        fa.foldLeft(init)(fn)

      override def order[A: Order] = new HeapOrder[A]
    }

  private[collections] class HeapOrder[A](implicit ordA: Order[A]) extends Order[Heap[A]] {
    @tailrec
    final def compare(left: Heap[A], right: Heap[A]): Int =
      if (left.isEmpty) {
        if (right.isEmpty) 0
        else -1
      }
      else if (right.isEmpty) 1
      else {
        // both are not empty
        val lb = left.asInstanceOf[Branch[A]]
        val rb = right.asInstanceOf[Branch[A]]
        val c = ordA.compare(lb.min, rb.min)
        if (c != 0) c
        else compare(left.remove, right.remove)
      }
  }
  /**
   * This is the same order as you would get by doing `.toList` and ordering by that
   */
  implicit def catsCollectionHeapOrder[A: Order]: Order[Heap[A]] =
    new HeapOrder[A]
}
