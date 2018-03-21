package cats.stm

import cats.Applicative
import cats.syntax.flatMap._
import cats.syntax.functor._

final class TMVar[A] private[stm](private[stm] val value: TVar[Option[A]]) {

  def take: STM[A] =
    for {
      aOpt <- tryTake
      _ <- STM.check(aOpt.isDefined)
    } yield aOpt.get


  def put(a: A): STM[Unit] =
    for {
      success <- tryPut(a)
      _ <- STM.check(success)
    } yield ()


  def read: STM[A] =
    for {
      a <- take
      _ <- put(a)
    } yield a


  def tryTake: STM[Option[A]] =
    for {
      aOpt <- value.get
      _ <- value.set(None)
    } yield aOpt


  def tryPut(a: A): STM[Boolean] =
    for {
      aOpt <- value.get
      canPut = aOpt.isEmpty
      _ <- Applicative[STM].whenA(canPut)(value.set(Some(a)))
    } yield canPut


  def tryRead: STM[Option[A]] =
    value.get
}
