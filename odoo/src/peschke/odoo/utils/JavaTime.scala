package peschke.odoo.utils

import cats.Order

import java.time.LocalDate
import java.time.LocalDateTime

object JavaTime {
  implicit val catsJavaTimeLocalDateOrder: Order[LocalDate] = Order.fromLessThan(_ isBefore _)
  implicit val catsJavaTimeLocalDateTimeOrder: Order[LocalDateTime] = Order.fromLessThan(_ isBefore _)
}
