package cats.stm

import cats.implicits._
import cats.effect.IO

class TVar[A] private[stm](val id: Long, var value: A) {
  def get: STM[A] = ReadTVar(this)

  def set(newValue: A): STM[Unit] = WriteTVar(this, newValue)

  def modify(f: A => A): STM[Unit] =
    get.flatMap(a => set(f(a)))

  private[stm] def getValue: IO[A] =
    IO {
      this.synchronized {
        value
      }
    }

  private[stm] def setValue(a: A): IO[Unit] =
    IO {
      this.synchronized {
        value = a
      }
    }

  private[stm] def atomicModify[E](f: A => IO[Either[E, A]]): IO[Option[E]] =
    IO {
      this.synchronized {
        f(value).unsafeRunSync match {
          case Right(newValue) =>
            if (value != newValue) {
              value = newValue
              this.notifyAll()
            }
          case Left(e) => Some(e)
        }
      }
    }
}
