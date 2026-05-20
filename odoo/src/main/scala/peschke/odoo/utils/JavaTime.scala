package peschke.odoo.utils

import cats.Order

import java.time.{LocalDate, LocalDateTime, Month}

object JavaTime {
  implicit val catsJavaTimeLocalDateOrder: Order[LocalDate] = Order.fromLessThan(_ isBefore _)
  implicit val catsJavaTimeLocalDateTimeOrder: Order[LocalDateTime] = Order.fromLessThan(_ isBefore _)
  implicit val catsJavaTimeMonthOrder: Order[Month] = Order.by(_.getValue)
}
