package cats.stm

import cats.syntax.flatMap._
import scala.concurrent.stm.Ref

final case class TVar[A] private[stm](private[stm] val ref: Ref[A]) {
  def get: STM[A] =
    ReadTVar(this)

  def set(newValue: A): STM[Unit] =
    WriteTVar(this, newValue)

  def modify(f: A => A): STM[Unit] =
    get.flatMap(a => set(f(a)))
}
