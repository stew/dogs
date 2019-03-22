package cats.collections
package syntax

import cats.{Foldable,Order,Semigroup}

trait FoldableSyntax {
  implicit def foldableSyntax[F[_]: Foldable, A](fa: F[A]): FoldableOps[F,A] =
    new FoldableOps(fa)
}

final class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
  def toCatsVector: Vector[A] =
    F.foldLeft[A, Vector[A]](fa, Vector.empty)(_ :+ _)

  def toCatsMap[K,V](implicit K: Order[K], ev: A =:= (K,V)): AvlMap[K,V] = {
    F.foldLeft(fa, AvlMap.empty[K,V])(_ + _)
  }

  def toCatsMultiMap[K,V](implicit K: Order[K], ev: A =:= (K,V), V: Semigroup[V]): AvlMap[K,V] = {
    F.foldLeft(fa, AvlMap.empty[K,V]){(m,a) =>
      val (k,v) = ev(a)
      m.updateAppend(k,v)
    }
  }
}
