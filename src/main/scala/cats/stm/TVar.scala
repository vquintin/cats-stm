package cats.stm

import cats.syntax.flatMap._
import cats.syntax.functor._
import scala.concurrent.stm.Ref

final class TVar[A] private[stm](private[stm] val ref: Ref[A]) {
  def get: STM[A] =
    ReadTVar(this)

  def set(newValue: A): STM[Unit] =
    WriteTVar(this, newValue)

  def modify(f: A => A): STM[Unit] =
    get.flatMap(a => set(f(a)))

  def swap(a: A): STM[A] =
    for {
      oldA <- get
      _ <- set(a)
    } yield oldA
}
