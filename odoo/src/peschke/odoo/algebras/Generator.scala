package peschke.odoo.algebras

import cats.effect.std.Random

trait Generator[F[_], A] {
  def create: F[A]
}
object Generator         {
  def apply[F[_], A](implicit ID: Generator[F, A]): ID.type = ID

  def usingRandom[F[_]: Random, A](f: Random[F] => F[A]): Generator[F, A] = new Generator[F, A] {
    override def create: F[A] = f(Random[F])
  }
}
