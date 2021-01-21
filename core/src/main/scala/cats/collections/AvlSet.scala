package cats.collections

import java.util.NoSuchElementException

import scala.annotation.tailrec
import scala.collection.immutable.List
import cats._
import cats.syntax.all._
import cats.collections.compat.Factory

/**
 * An immutable, ordered, extensional set
 *
 * This data-structure maintains balance using the
 * [AVL](https://en.wikipedia.org/wiki/AVL_tree) algorithm.
 */
sealed abstract class AvlSet[A] {
  import AvlSet._

  /**
   * The number of items in the Set.
   * O(1)
   */
  val size: Int

  /**
   * Returns `true` if the Set is the empty Set.
   * O(1)
   */
  def isEmpty: Boolean

  /**
   * Map a function on all values of the set
   */
  def map[B: Order](f: A => B): AvlSet[B] =
    foldLeft[AvlSet[B]](empty)((s,a) => s + f(a))

  /**
   * Map a function on all values of the set
   */
  def flatMap[B: Order](f: A => AvlSet[B]): AvlSet[B] =
    foldLeft[AvlSet[B]](empty)((s,a) => s ++ f(a))

  /**
   * Returns None if the set is empty, otherwise returns the minimum
   * element.
   * O(log n)
   */
  def min: Option[A] = {
    @tailrec def loop(sub: AvlSet[A], x: A): A = sub match {
      case Branch(a, l, _) => loop(l, a)
      case _ =>  x
    }

    this match {
      case Branch(a, l, _) => Some(loop(l, a))
      case _ => None
    }
  }

  /**
   * Returns `None` if the set is empty, otherwise returns the maximum
   * element.
   * O(log n)
   */
  def max: Option[A] = {
    @tailrec def loop(sub: AvlSet[A], x: A): A = sub match {
      case Branch(a, _, r) => loop(r, a)
      case _ =>  x
    }

    this match {
      case Branch(a, _, r) => Some(loop(r, a))
      case _ => None
    }
  }

  /**
   * Applies a function to each element, in ascending order
   * O(n)
   */
  def foreach(f: A => Unit): Unit = this match {
    case Branch(v, l, r) => l.foreach(f); f(v); r.foreach(f)
    case _ =>
  }

  /**
   * fold the elements together from min to max, using the passed
   * seed, and accumulator function.
   * O(n)
   */
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Branch(v, l, r) => r.foldLeft(f(l.foldLeft(z)(f), v))(f)
    case _ => z
  }

  /**
   * fold the elements together from min to max, using the passed
   * seed, and accumulator function.
   * O(n)
   */
  def foldRight[B](z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = this match {
    case Branch(v, l, r) => l.foldRight(f(v, r.foldRight(z)(f)))(f)
    case _ => z
  }

  /**
   * Find the minimum element matching the given predicate. Returns
   * None if there is no element matching the predicate.
   * O(log n)
   */
  def find(pred: A => Boolean): Option[A] = this match {
    case Branch(v, l, r) =>
      l.find(pred) orElse (if(pred(v)) Some(v) else r.find(pred))
    case _ => None
  }

  /**
   * Returns `true` if the given element is in the set.
   * O(log n)
   */
  def contains(x: A)(implicit order: Order[A]): Boolean = this match {
    case Branch(a, l, r) => order.compare(x, a) match {
      case 0 => true
      case o if o < 0 => l.contains(x)
      case _ => r.contains(x)
    }
    case _ => false
  }

  /**
   * Add's the given element to the set if it is not already present.
   * O(log n)
   */
  def add(x: A)(implicit order: Order[A]): Branch[A] =
    (this match {
      case Branch(a, l, r) =>  order.compare(x, a) match {
        case 0 => Branch(x, l, r)
        case o if o < 0 => Branch(a, l.add(x), r)
        case _ => Branch(a, l, r.add(x))
      }
      case _ =>  Branch(x, AvlSet.empty, AvlSet.empty)
    }).balance


  /**
   * Add's the given element to the set if it is not already present.
   * O(log n)
   */
  def +(x: A)(implicit order: Order[A]): AvlSet[A] = add(x)

  /**
   * Return a set which does not contain the given element.
   * O(log n)
   */
  def remove(x: A)(implicit order: Order[A]): AvlSet[A] =
    this match {
      case Branch(a, l, r) =>
        order.compare(x, a) match {
          case 0 => r.min match {
            case None => l
            case Some(v) => Branch(v,l,r.remove(v)).balance
          }
          case o if o < 0 => Branch(a, l.remove(x), r).balance
          case _ => Branch(a, l, r.remove(x)).balance
        }
      case _ => AvlSet.empty
    }

  // STU: this is used by Map, not sure what to do about this
  private[collections] def removef[B](x: B, f: A => B)(implicit B: Order[B]): AvlSet[A] =
    this match {
      case Branch(a, l, r) =>
        B.compare(x, f(a)) match {
          case 0 => r.min match {
            case None => l
            case Some(v) =>
              Branch(v,l,r.removef(f(v), f)).balance
          }
          case o if o < 0 => Branch(a, l.removef(x, f), r).balance
          case _ => Branch(a, l, r.removef(x, f)).balance
        }
      case _ => AvlSet.empty
    }

  /**
   * Return a set containing the union of elements with this set and
   * the given set.
   * O(n log n)
   */
  def union(another: AvlSet[A])(implicit order: Order[A]): AvlSet[A] = another.foldLeft(this)(_ + _)

  /**
   * Return a set containing the union of elements with this set and
   * the given set.
   * O(n log n)
   */
  def |(another: AvlSet[A])(implicit order: Order[A]): AvlSet[A] = this union another

  /**
   * Return a set containing the intersection of elements with this set and
   * the given set.
   * O(n log n)
   */
  def intersect(another: AvlSet[A])(implicit order: Order[A]): AvlSet[A] = {
    def _intersect(small: AvlSet[A], large: AvlSet[A]): AvlSet[A] =
      small.foldLeft[AvlSet[A]](empty)((t,a) => if(large.contains(a)) t + a else t)

    if (this.size < another.size)
      _intersect(this, another)
    else
      _intersect(another, this)
  }

  /**
   * Return a set containing the intersection of elements with this set and
   * the given set.
   * O(n log n)
   */
  def &(another: AvlSet[A])(implicit order: Order[A]): AvlSet[A] = this intersect another

  /**
   * Return a set containing the union of elements with this set and
   * the given set.
   * O(n log n)
   */
  def ++(another: AvlSet[A])(implicit order: Order[A]): AvlSet[A] = this union another

  /**
   * Return a set that has any elements appearing in the removals set removed
   * O(n log n)
   */
  def diff(removals: AvlSet[A])(implicit order: Order[A]): AvlSet[A] =
    removals.foldLeft(this)(_ remove _)

  /**
   * Return a set that has any elements appearing in the removals set removed
   * O(n log n)
   */
  def -(removals: AvlSet[A])(implicit order: Order[A]): AvlSet[A] =
    removals.foldLeft(this)(_ remove _)


  /**
   * Return an Predicate with the same members as this set
   */
  def predicate(implicit order: Order[A]): Predicate[A] = Predicate(contains)

  /**
    * Converts this set into a Scala collection
    * O(n)
    */
  def to[Col[_]](implicit cbf: Factory[A, Col[A]]): Col[A] = {
    val builder = cbf.newBuilder
    this.foreach(builder += _)
    builder.result()
  }

  /**
    * Return the sorted list of elements.
    * O(n)
    */
  def toList: List[A] = to[List]

  /**
   * Return a Scala set containing the elements in the set
   * O(n)
   */
  def toScalaSet: Set[A] = to[Set]

  def toIterator: Iterator[A] = new Iterator[A] {
    var stack: List[Either[A, AvlSet[A]]] = List(Right(AvlSet.this))

    @tailrec override def hasNext: Boolean = stack match {
      case Nil => false
      case Left(_) :: _ => true
      case Right(Branch(_, _, _)) :: _ => true
      case _ :: ss =>
        stack = ss
        hasNext
    }

    @tailrec override def next(): A = stack match {
      case Nil => throw new NoSuchElementException()
      case Left(v) :: ss =>
        stack = ss
        v
      case Right(Branch(v, l, r)) :: ss =>
        stack = Right(l) :: Left(v) :: Right(r) :: ss
        next()
      case _ :: ss =>
        stack = ss
        next()
    }
  }

  override def toString: String =
    "AvlSet(" + Foldable[List].intercalate(toList.map(_.toString), ", ") + ")"

  // So yeah. we had to make a decision, either we have to make this
  // structure Key/Value pairs even when we don't always need a value
  // (in the case of a Set), or we have to have separate structures
  // for Set and Map, or we have to have a function like this one,
  // that only really make sense fo Map. I chose this one. This
  // function makes it so that we can find things in the tree just
  // based on a Key, when the set is really storing a Key value pair.
  // The name was chosen so that it would be convenient for nobody to
  // remember.
  private[collections] def _getkv[B](f: A => B, b: B)(implicit B: Order[B]): Option[A] = {
    @tailrec def go(t: AvlSet[A]): Option[A] = t match {
      case Branch(v,l,r) =>
        B.compare(b, f(v)) match {
          case 0 => Some(v)
          case x if x < 0 => go(l)
          case _ => go(r)
        }
      case _ => None
    }
    go(this)
  }

  private[collections] def updateKey[K,V](key: K, value: V)(implicit order: Order[K], ev: A =:= (K,V), V: Semigroup[V]): AvlSet[A] = {
    (this match {
      case Branch(a, l, r) =>  order.compare(key, ev(a)._1) match {
        case 0 =>
          val (k,v) = ev(a)
          Branch((k -> V.combine(v,value)).asInstanceOf[A], l, r)
        case o if o < 0 => Branch(a, l.updateKey(key, value), r)
        case _ => Branch(a, l, r.updateKey(key,value))
      }
      case _ =>  Branch((key -> value).asInstanceOf[A], AvlSet.empty, AvlSet.empty)
    }).balance
  }

  private[collections] val height: Int
}

object AvlSet extends AvlSetInstances {

  /**
   * Create a set with the given elements.
   */
  def apply[A: Order](as: A*): AvlSet[A] =
    as.foldLeft[AvlSet[A]](empty)(_ + _)

  def fromList[A: Order](as: List[A]): AvlSet[A] =
    as.foldLeft[AvlSet[A]](empty)(_ + _)

  /**
    * Create a set from any collection of elements which has a [[cats.Foldable]] instance.
    */
  def fromFoldable[F[_], A: Order](as: F[A])(implicit fa: Foldable[F]): AvlSet[A] =
    fa.foldLeft[A, AvlSet[A]](as, empty)(_ + _)

  /**
   * The empty set.
   */
  def empty[A]: AvlSet[A] = BTNil()

  private[collections] case class Branch[A](value: A,
                                     left: AvlSet[A],
                                     right: AvlSet[A]) extends AvlSet[A] {

    val size = left.size + right.size + 1
    val height = java.lang.Math.max(left.height, right.height) + 1

    override def isEmpty: Boolean = false

    // Determine the direction that the tree should be rotated,
    // given the allowed amount of imbalance.
    // Returns -1 when a left rotation is called for.
    // Returns 0 when a right rotation is called for.
    // Returns 1 when the tree is within the allowance.
    private def rotation(l: Int, r: Int, allow: Int): Int =
      if(l - r > allow ) 1
      else if(r - l > allow) -1
      else 0

    private[collections] def balance: Branch[A] = {
      val r = rotation(left.height, right.height, 1)

      if(r == 0) this
      else if(r > 0) {
        left match {
          case Branch(lv,ll,lr) =>
            if(rotation(ll.height, lr.height, 0) < 0) {
              val Branch(lrv,lrl,lrr) = lr
              Branch(lrv,Branch(lv, ll, lrl), Branch(value, lrr, right))
            } else {
              Branch(lv, ll, Branch(value, lr, right))
            }
          case _ => this
        }
      } else {
        right match {
          case Branch(rv,rl,rr) =>
            if(rotation(rl.height, rr.height, 0) > 0) {
              val Branch(rlv,rll,rlr) = rl
              Branch(rlv, Branch(value, left, rll), Branch(rv, rlr, rr))
            } else {
              Branch(rv, Branch(value, left, rl), rr)
            }
          case _ => this
        }
      }
    }
  }

  private[collections] case object BTNil extends AvlSet[Nothing] {
    override def isEmpty: Boolean = true

    def apply[A](): AvlSet[A] = this.asInstanceOf[AvlSet[A]]

    def unapply[A](a: AvlSet[A]): Boolean = a.isEmpty

    override val size: Int = 0
    override val height: Int = 0
  }

}

trait AvlSetInstances {
  implicit def eqSet[A: Eq]: Eq[AvlSet[A]] = new Eq[AvlSet[A]] {
    override def eqv(x: AvlSet[A], y: AvlSet[A]): Boolean =
      iteratorEq(x.toIterator, y.toIterator)
  }

  implicit def showSet[A: Show]: Show[AvlSet[A]] = new Show[AvlSet[A]] {
    override def show(t: AvlSet[A]): String =
      t.toIterator.map(_.show).mkString("AvlSet(", ", ", ")")
  }
}
