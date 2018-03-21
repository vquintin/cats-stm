package cats.stm

import cats.{Alternative, Functor, MonadError, ~>}
import cats.effect.IO
import cats.mtl.FunctorEmpty
import cats.mtl.syntax.empty._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.stm._


sealed trait STM[+A]

private[stm] final case class Pure[A](a: A) extends STM[A]

private[stm] final case class Bind[E, A](e: STM[E], f: E => STM[A]) extends STM[A]

private[stm] final case class RaiseError[A](e: Throwable) extends STM[A]

private[stm] case object Retry extends STM[Nothing]

private[stm] final case class OrElse[A](fst: STM[A], snd: STM[A]) extends STM[A]

private[stm] final case class ReadTVar[A](tVar: TVar[A]) extends STM[A]

private[stm] final case class WriteTVar[A](tVar: TVar[A], a: A) extends STM[Unit]

private[stm] final case class NewTVar[A](a: A) extends STM[TVar[A]]

object STM {
  implicit val stmInstances: MonadError[STM, Throwable] with Alternative[STM] =
    new MonadError[STM, Throwable] with Alternative[STM] {
      override def pure[A](x: A): STM[A] =
        Pure(x)

      override def flatMap[A, B](fa: STM[A])(f: A => STM[B]): STM[B] =
        Bind(fa, f)

      override def tailRecM[A, B](a: A)(f: A => STM[Either[A, B]]): STM[B] =
        f(a) flatMap {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => pure(b)
        }

      override def raiseError[A](e: Throwable): STM[A] =
        RaiseError(e)

      override def handleErrorWith[A](fa: STM[A])(f: Throwable => STM[A]): STM[A] =
        fa match {
          case RaiseError(e) => f(e)
          case _ => fa
        }

      override def empty[A]: STM[A] =
        Retry

      override def combineK[A](x: STM[A], y: STM[A]): STM[A] =
        OrElse(x, y)
    }

  def newTVar[A](a: A): STM[TVar[A]] =
    NewTVar(a)

  def pure[A](a: A): STM[A] =
    Pure(a)

  def retry: STM[Unit] =
    Retry

  def check(cond: Boolean): STM[Unit] =
    if (cond) pure(()) else retry

  val atomically: STM ~> IO =
    new ~>[STM, IO] {
      override def apply[A](fa: STM[A]): IO[A] =
        IO(atomic { implicit txn => unsafeRun(fa) })
    }

  private def unsafeRun[A](fa: STM[A])(implicit txn: scala.concurrent.stm.InTxn): A =
    fa match {
      case Pure(a) =>
        a
      case OrElse(l, r) =>
        atomic[A] { implicit txn =>
          unsafeRun(l)
        }.orAtomic[A] { implicit txn =>
          unsafeRun(r)
        }
      case NewTVar(a) =>
        new TVar(Ref(a))
      case ReadTVar(tVar) =>
        tVar.ref()
      case WriteTVar(tVar, a) =>
        tVar.ref() = a
      case Retry =>
        scala.concurrent.stm.retry
      case RaiseError(e) =>
        throw e
      case Bind(fa, f) =>
        unsafeRun(f(unsafeRun(fa)))
    }
}
