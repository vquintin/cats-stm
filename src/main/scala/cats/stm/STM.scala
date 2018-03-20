package cats.stm

import cats.data.StateT
import cats.{Alternative, Monad, MonadError, ~>}
import cats.effect.IO
import cats.implicits._

import scala.collection.{SortedMap, SortedSet}

sealed trait STM[+A]

private[stm] final case class Pure[A](a: A) extends STM[A]

private[stm] final case class RaiseError[A](e: Throwable) extends STM[A]

private[stm] final case class Bind[E, A](source: STM[E], f: E => STM[A]) extends STM[A]

private[stm] case object Retry extends STM[Nothing]

private[stm] final case class OrElse[A](fst: STM[A], snd: STM[A]) extends STM[A]

private[stm] final case class ReadTVar[A](tVar: TVar[A]) extends STM[A]

private[stm] final case class WriteTVar[A](tVar: TVar[A], a: A) extends STM[Unit]

private[stm] final case class NewTVar[A](a: A) extends STM[TVar[A]]

object STM {
  implicit val stmInstances: MonadError[STM, Throwable] with Alternative[STM] =
    new MonadError[STM, Throwable] with Alternative[STM] {
      override def pure[A](x: A): STM[A] = Pure(x)

      override def flatMap[A, B](fa: STM[A])(f: A => STM[B]): STM[B] = Bind(fa, f)

      override def tailRecM[A, B](a: A)(f: A => STM[Either[A, B]]): STM[B] =
        f(a).flatMap {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => pure(b)
        }

      override def raiseError[A](e: Throwable): STM[A] = RaiseError(e)

      override def handleErrorWith[A](fa: STM[A])(f: Throwable => STM[A]): STM[A] =
        fa match {
          case RaiseError(e) => f(e)
          case _ => fa
        }

      override def empty[A]: STM[A] = Retry

      override def combineK[A](x: STM[A], y: STM[A]): STM[A] = OrElse(x, y)
    }

  def newTVar[A](a: A): STM[TVar[A]] = NewTVar(a)

  def retry: STM[Unit] = Retry

  val atomically: STM ~> IO =
    new ~>[STM, IO] {
      override def apply[A](fa: STM[A]): IO[A] =
        Monad[IO].tailRecM(())(tryRunAndCommit(fa))

      private def tryRunAndCommit[A](fa: STM[A])(unit: Unit): IO[Either[Unit, A]] =
        for {
          logAndA <- run(fa)
          (log, aOpt) = logAndA
          unitOpt <- tryCommit(log)
        } yield {
          if (unitOpt.isDefined) {
            Left(())
          } else {
            Right(a)
          }
        }

      private def run[A](fa: STM[A]): IO[(Log, Option[A])] = ???

      private type F[X] = StateT[IO, Log, Option[X]]

      private val interpreter: STM ~> F =
        new ~>[STM, F] {
          override def apply[A](fa: STM[A]): F[A] =
            fa match {
              case Pure(a) => StateT.pure(Some(a))
              case OrElse(l, r) => ???
              case NewTVar(x) => StateT.liftF(IO(new TVar[A](???, x)))
              case ReadTVar(tVar) => StateT.liftF[IO, Log, Option[A]](tVar.getValue.map(Some(_)))
              case WriteTVar(tVar, a) => StateT.liftF[IO, Log, Option[Unit]](tVar.setValue(a).map(Some(_)))
              case Retry => StateT.liftF[IO, Log, Option[Unit]](IO.pure(None))
              case RaiseError(e) => StateT.liftF[IO, Log, Option[A]](IO.raiseError[Option[A]](e))
              case Bind(fa, f) => apply(fa).flatMap{
                case Some(e) => apply(f(e))
                case None => StateT.liftF[IO, Log, Option[A]](IO.pure(None))
              }
            }
        }


      private def tryCommit(log: Log): IO[Option[Unit]] = ???

      private case class LogEntry[A](tVar: TVar[A], oldValue: A, newValue: A)

      private type UntypedLogEntry = LogEntry[X] forSome {type X}

      private type Log = SortedMap[Int, UntypedLogEntry]
    }
}


