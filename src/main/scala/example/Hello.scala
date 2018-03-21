package example

import cats.{Applicative, MonadError}
import cats.effect.IO
import cats.stm.{STM, TVar}
import cats.syntax.flatMap._
import cats.syntax.semigroupk._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.instances.list._


object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"

  case class Account(money: Int)


  def transfer(from: TVar[Account], to: TVar[Account], amount: Int): STM[Unit] =
    for {
      _ <- Applicative[STM].whenA(amount < 0) {
        MonadError[STM, Throwable].raiseError(new IllegalArgumentException("amount is negative"))
      }
      accountFrom <- from.get
      _ <- STM.check(accountFrom.money >= amount)
      _ <- from.modify(x => Account(x.money - amount))
      _ <- to.modify(x => Account(x.money + amount))
    } yield ()


  def transferWithFallBack(from: TVar[Account], fallbackFrom: TVar[Account], to: TVar[Account], amount: Int): STM[Unit] =
    transfer(from, to, amount) <+> transfer(fallbackFrom, to, amount)


  def skim(myAccount: TVar[Account], accounts: List[TVar[Account]], amount: Int): STM[Unit] =
    accounts.traverse[STM, Unit] { account: TVar[Account] =>
      transfer(account, myAccount, amount) <+> STM.pure(())
    }.map(_ => ())


}
