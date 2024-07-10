package peschke.odoo.models

import cats.Show
import cats.syntax.all._
import cats.data.NonEmptyList
import peschke.odoo.models.Template.Tag

sealed trait TagFilter extends Product with Serializable
object TagFilter {
  final case class TaggedWith(tags: NonEmptyList[Tag]) extends TagFilter
  final case class Not(filter: TagFilter) extends TagFilter
  final case class And(a: TagFilter, b: TagFilter) extends TagFilter
  case object True extends TagFilter

  implicit val show: Show[TagFilter] = Show.show {
    case TaggedWith(tags) => tags.mkString_("{", " | ", "}")
    case Not(filter) => s"!${show.show(filter)}"
    case And(a, b) => s"${show.show(a)} && ${show.show(b)}"
    case True => "true"
  }
}
