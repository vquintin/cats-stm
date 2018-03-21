package cats.stm

import cats.{Applicative, Monad}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.option._

import scala.collection.immutable.Stream.Cons

class TBQueue[A] private[stm](val capacity: Int,
                              private[stm] val writerHead: TVar[Cons[TMVar[A]]],
                              private[stm] val readerHead: TVar[Cons[TMVar[A]]]
                             ) {
  def read: STM[A] =
    for {
      cons <- readerHead.get
      result <- cons.head.take
      _ <- readerHead.set(cons.tail.asInstanceOf[Cons[TMVar[A]]])
    } yield result

  def write(a: A): STM[Unit] =
    for {
      cons <- writerHead.get
      _ <- cons.head.put(a)
      _ <- writerHead.set(cons.tail.asInstanceOf[Cons[TMVar[A]]])
    } yield ()

  def flush: STM[List[A]] =
    Monad[STM].tailRecM[List[A], List[A]](Nil) { acc =>
      tryRead.map { opt =>
        opt.map(a => Left(a :: acc))
          .getOrElse(Right(acc))
      }
    }.map(_.reverse)

  def peek: STM[A] =
    for {
      cons <- readerHead.get
      result <- cons.head.read
    } yield result

  def tryRead: STM[Option[A]] =
    for {
      cons <- readerHead.get
      resultOpt <- cons.head.tryTake
      _ <- Applicative[STM].whenA(resultOpt.isDefined)(readerHead.set(cons.tail.asInstanceOf[Cons[TMVar[A]]]))
    } yield resultOpt
}
