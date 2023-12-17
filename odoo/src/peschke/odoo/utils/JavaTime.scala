package peschke.odoo.utils

import cats.Order

import java.time.LocalDate

object JavaTime {
  implicit val catsJavaTimeLocalDateOrder: Order[LocalDate] = Order.fromLessThan(_ isBefore  _)
}
