package cats.collections

import cats._
import cats.implicits._

class HashSet[A](val buckets: Vector[List[A]]) {

  def +(a: A)(implicit hash: Hash[A]): HashSet[A] = {
    def updateBucket(bucket: List[A], value: A): List[A] =
      bucket match {
        case Nil => a :: Nil
        case h :: tail =>
          if(h === value) h :: tail
          else h :: updateBucket(tail, value)
      }

    val bucketIndex = a.hash % buckets.size
    val bucket = buckets(bucketIndex)
    val updatedBucket = updateBucket(bucket, a)
    new HashSet(buckets.updated(bucketIndex, updatedBucket))
  }

  def toIterator: Iterator[A] =
    buckets.flatten.toIterator

}

object HashSet extends HashSetInstances {

  val defaultCapacity = 16

  def apply[A: Hash](as: A*): HashSet[A] =
    as.foldLeft(empty[A])(_ + _)

  def empty[A]: HashSet[A] = new HashSet(Vector.fill(defaultCapacity)(List.empty))

}

trait HashSetInstances {

  implicit def eqSet[A](implicit A: Eq[A]): Eq[HashSet[A]] = new Eq[HashSet[A]] {

    override def eqv(x: HashSet[A], y: HashSet[A]): Boolean =
      iteratorEq(x.toIterator, y.toIterator)

  }

}
