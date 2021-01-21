package cats.collections

import cats._
import cats.implicits._

class HashMap[K, V](val buckets: Vector[List[(K, V)]]) {

  def +(pair: (K, V))(implicit K: Hash[K]): HashMap[K, V] = {
    def updateBucket(bucket: List[(K, V)], key: K, value: V): List[(K, V)] =
      bucket match {
        case Nil => (key, value) :: Nil
        case (bk, _) :: tail if bk === key => (key, value) :: tail
        case head :: tail => head :: updateBucket(tail, key, value)
      }

    val (key, value) = pair
    val keyBucketIndex = key.hash % buckets.size
    val keyBucket = buckets(keyBucketIndex)
    val updatedKeyBucket = updateBucket(keyBucket, key, value)
    new HashMap(buckets.updated(keyBucketIndex, updatedKeyBucket))
  }

  def get(key: K)(implicit K: Eq[K]): Option[V] = if(buckets.nonEmpty) {
    val bucketIndex = key.hashCode % buckets.size
    buckets(bucketIndex).collectFirst {
      case (pkey, pvalue) if pkey === key => pvalue
    }
  } else None

  def entryHashSet(implicit K: Hash[K], V: Hash[V]): HashSet[(K, V)] = buckets.foldLeft(HashSet.empty[(K, V)]) {
    case (oset, bucket) => bucket.foldLeft(oset)(_ + _)
  }

}

object HashMap extends HashMapInstances {

  val defaultCapacity = 16

  def apply[K: Hash, V](entries: (K, V)*): HashMap[K, V] = entries.foldLeft(empty[K, V])(_ + _)

  def empty[K, V]: HashMap[K, V] = new HashMap[K, V](Vector.fill(defaultCapacity)(List.empty))

}

trait HashMapInstances {

  implicit def eqMap[K, V](implicit K: Hash[K], V: Hash[V]): Eq[HashMap[K, V]] = new Eq[HashMap[K, V]] {

    override def eqv(x: HashMap[K,V], y: HashMap[K,V]): Boolean = {
      x.entryHashSet === y.entryHashSet
    }

  }

}
